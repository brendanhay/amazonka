{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeApps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of apps. You must specify at
-- least one of the parameters. Required Permissions: To use this action, an
-- IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeApps where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeApps' request.
describeApps :: DescribeApps
describeApps = DescribeApps
    { _dasStackId = Nothing
    , _dasAppIds = mempty
    }

data DescribeApps = DescribeApps
    { _dasStackId :: Maybe Text
      -- ^ The app stack ID. If you use this parameter, DescribeApps returns
      -- a description of the apps in the specified stack.
    , _dasAppIds :: [Text]
      -- ^ An array of app IDs for the apps to be described. If you use this
      -- parameter, DescribeApps returns a description of the specified
      -- apps. Otherwise, it returns a description of every app.
    } deriving (Generic)

makeLenses ''DescribeApps

instance ToPath DescribeApps

instance ToQuery DescribeApps

instance ToHeaders DescribeApps

instance ToJSON DescribeApps

data DescribeAppsResponse = DescribeAppsResponse
    { _datApps :: [App]
      -- ^ An array of App objects that describe the specified apps.
    } deriving (Generic)

makeLenses ''DescribeAppsResponse

instance FromJSON DescribeAppsResponse

instance AWSRequest DescribeApps where
    type Sv DescribeApps = OpsWorks
    type Rs DescribeApps = DescribeAppsResponse

    request = get
    response _ = jsonResponse
