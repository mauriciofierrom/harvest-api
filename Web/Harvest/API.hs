-- |
-- Module      :  Web.Harvest.API
-- Copyright   :  © 2016 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Mark Karpov <mkarpov@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level bindings to the Harvest web API.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Harvest.API
  ( -- * Types
    module Web.Harvest.API.Type
    -- * Users
  , getUsers
    -- * Projects
  , getProjects
    -- * Tasks
  , getTasks
    -- * TaskAssignments
  , getTaskAssignments
    -- * Timesheets
  , getTimeEntries )
where

import Control.Monad.Except
import Data.Monoid (Sum (..))
import Data.Proxy
import Data.Time (Day)
import Data.Void
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Web.Harvest.API.Type
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Time             as Time

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
import Data.Word (Word)
#endif

-- | Type representation of Basic Auth. We don't serve the API, so user type
-- is just 'Void'.

type Auth = BasicAuth "" Void

-- | Entire Harvest API as a type.

type HarvestAPI =
       "people" :> Auth :> Get '[JSON] [User]
  :<|> "daily"  :> Capture "day" Word :> Capture "year" Word
                :> QueryParam "of_user" UserId
                :> Auth :> Get '[JSON] TimeEntries
  :<|> "project" :> Auth :> Get '[JSON] [Project]
  :<|> "task"   :> Auth :> Get '[JSON] [Task]
  :<|> "task-assignment" :> Auth :> Get '[JSON] [TaskAssignment]

-- | A shortcut for the boilerplate arguments.

type Query a = BasicAuthData -> Manager -> BaseUrl -> ClientM a

getUsers_       :: Query [User]
getTimeEntries_ :: Word -> Word -> Maybe UserId -> Query TimeEntries
getProjects_    :: Query [Project]
getTasks_       :: Query [Task]
getTaskAssignments_ :: Query [TaskAssignment]

getUsers_ :<|> getTimeEntries_ :<|> getProjects_ :<|> getTasks_ :<|> getTaskAssignments_ = client (Proxy :: Proxy HarvestAPI)

-- | Get list of all users for specific account.

getUsers :: MonadIO m
  => Manager           -- ^ HTTPS manager
  -> Credentials       -- ^ Credentials
  -> m (Either ServantError [User]) -- ^ Result of request
getUsers = runHarvestQuery getUsers_

-- | Get time entries for specific date and user.

getTimeEntries :: MonadIO m
  => Manager           -- ^ HTTPS manager
  -> Credentials       -- ^ Credentials
  -> Day               -- ^ Date of interest
  -> UserId            -- ^ User id
  -> m (Either ServantError TimeEntries)
getTimeEntries manager creds date uid =
  runHarvestQuery (getTimeEntries_ day year (Just uid)) manager creds
  where (day, year) = getDayAndYear date

-- | Get list of all projects for specific account.

getProjects :: MonadIO m
            => Manager
            -> Credentials
            -> m (Either ServantError [Project])
getProjects = runHarvestQuery getProjects_

-- | Get list of all tasks for specific account

getTasks :: MonadIO m
         => Manager
         -> Credentials
         -> m (Either ServantError [Task])
getTasks = runHarvestQuery getTasks_

-- | Get list of all project task assignments for specific account.

getTaskAssignments :: MonadIO m
                   => Manager
                   -> Credentials
                   -> m (Either ServantError [TaskAssignment])
getTaskAssignments = runHarvestQuery getTaskAssignments_


-- | A helper to run a query against Harvest API.

runHarvestQuery :: MonadIO m
  => (BasicAuthData -> Manager -> BaseUrl -> ClientM a) -- ^ Query function
  -> Manager           -- ^ HTTPS Manager
  -> Credentials       -- ^ Credentials
  -> m (Either ServantError a) -- ^ The result
runHarvestQuery action manager Credentials {..} = liftIO $ do
  let host     = BC8.unpack credentialsAccount ++ ".harvestapp.com"
      authData = BasicAuthData credentialsUsername credentialsPassword
  runExceptT (action authData manager (BaseUrl Https host 443 ""))

-- | Extract day and year from given date. Days are in the range from 1 to
-- 366.

getDayAndYear :: Day -> (Word, Word)
getDayAndYear date = (day, fromIntegral year)
  where
    (year, month, day') = Time.toGregorian date
    day = fromIntegral . (+ day') . getSum . foldMap
      (Sum . Time.gregorianMonthLength year) $ [1..pred month]
