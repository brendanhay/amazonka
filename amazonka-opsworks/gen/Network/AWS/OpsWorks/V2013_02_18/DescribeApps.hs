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
    , describeApps
    -- ** Request lenses
    , dasStackId
    , dasAppIds

    -- * Response
    , DescribeAppsResponse
    -- ** Response lenses
    , datApps
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeApps' request.
describeApps :: DescribeApps
describeApps = DescribeApps
    { _dasStackId = Nothing
    , _dasAppIds = mempty
    }
{-# INLINE describeApps #-}

data DescribeApps = DescribeApps
    { _dasStackId :: Maybe Text
      -- ^ The app stack ID. If you use this parameter, DescribeApps returns
      -- a description of the apps in the specified stack.
    , _dasAppIds :: [Text]
      -- ^ An array of app IDs for the apps to be described. If you use this
      -- parameter, DescribeApps returns a description of the specified
      -- apps. Otherwise, it returns a description of every app.
    } deriving (Show, Generic)

-- | The app stack ID. If you use this parameter, DescribeApps returns a
-- description of the apps in the specified stack.
dasStackId :: Lens' DescribeApps (Maybe Text)
dasStackId f x =
    f (_dasStackId x)
        <&> \y -> x { _dasStackId = y }
{-# INLINE dasStackId #-}

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, DescribeApps returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
dasAppIds :: Lens' DescribeApps ([Text])
dasAppIds f x =
    f (_dasAppIds x)
        <&> \y -> x { _dasAppIds = y }
{-# INLINE dasAppIds #-}

instance ToPath DescribeApps

instance ToQuery DescribeApps

instance ToHeaders DescribeApps

instance ToJSON DescribeApps

data DescribeAppsResponse = DescribeAppsResponse
    { _datApps :: [App]
      -- ^ An array of App objects that describe the specified apps.
    } deriving (Show, Generic)

-- | An array of App objects that describe the specified apps.
datApps :: Lens' DescribeAppsResponse ([App])
datApps f x =
    f (_datApps x)
        <&> \y -> x { _datApps = y }
{-# INLINE datApps #-}

instance FromJSON DescribeAppsResponse

instance AWSRequest DescribeApps where
    type Sv DescribeApps = OpsWorks
    type Rs DescribeApps = DescribeAppsResponse

    request = get
    response _ = jsonResponse
