{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeApps
    (
    -- * Request
      DescribeApps
    -- ** Request constructor
    , mkDescribeApps
    -- ** Request lenses
    , da1StackId
    , da1AppIds

    -- * Response
    , DescribeAppsResponse
    -- ** Response constructor
    , mkDescribeAppsResponse
    -- ** Response lenses
    , darApps
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeApps = DescribeApps
    { _da1StackId :: Maybe Text
    , _da1AppIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApps' request.
mkDescribeApps :: DescribeApps
mkDescribeApps = DescribeApps
    { _da1StackId = Nothing
    , _da1AppIds = mempty
    }

-- | The app stack ID. If you use this parameter, DescribeApps returns a
-- description of the apps in the specified stack.
da1StackId :: Lens' DescribeApps (Maybe Text)
da1StackId = lens _da1StackId (\s a -> s { _da1StackId = a })

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, DescribeApps returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
da1AppIds :: Lens' DescribeApps [Text]
da1AppIds = lens _da1AppIds (\s a -> s { _da1AppIds = a })

instance ToPath DescribeApps

instance ToQuery DescribeApps

instance ToHeaders DescribeApps

instance ToJSON DescribeApps

-- | Contains the response to a DescribeApps request.
newtype DescribeAppsResponse = DescribeAppsResponse
    { _darApps :: [App]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAppsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeAppsResponse :: DescribeAppsResponse
mkDescribeAppsResponse = DescribeAppsResponse
    { _darApps = mempty
    }

-- | An array of App objects that describe the specified apps.
darApps :: Lens' DescribeAppsResponse [App]
darApps = lens _darApps (\s a -> s { _darApps = a })

instance FromJSON DescribeAppsResponse

instance AWSRequest DescribeApps where
    type Sv DescribeApps = OpsWorks
    type Rs DescribeApps = DescribeAppsResponse

    request = get
    response _ = jsonResponse
