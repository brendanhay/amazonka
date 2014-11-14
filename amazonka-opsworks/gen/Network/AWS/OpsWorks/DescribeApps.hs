{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of apps. Required Permissions: To
-- use this action, an IAM user must have a Show, Deploy, or Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.DescribeApps
    (
    -- * Request
      DescribeApps
    -- ** Request constructor
    , describeApps
    -- ** Request lenses
    , daAppIds
    , daStackId

    -- * Response
    , DescribeAppsResponse
    -- ** Response constructor
    , describeAppsResponse
    -- ** Response lenses
    , darApps
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeApps = DescribeApps
    { _daAppIds  :: [Text]
    , _daStackId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeApps' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAppIds' @::@ ['Text']
--
-- * 'daStackId' @::@ 'Maybe' 'Text'
--
describeApps :: DescribeApps
describeApps = DescribeApps
    { _daStackId = Nothing
    , _daAppIds  = mempty
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, DescribeApps returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
daAppIds :: Lens' DescribeApps [Text]
daAppIds = lens _daAppIds (\s a -> s { _daAppIds = a })

-- | The app stack ID. If you use this parameter, DescribeApps returns a
-- description of the apps in the specified stack.
daStackId :: Lens' DescribeApps (Maybe Text)
daStackId = lens _daStackId (\s a -> s { _daStackId = a })

instance ToPath DescribeApps where
    toPath = const "/"

instance ToQuery DescribeApps where
    toQuery = const mempty

instance ToHeaders DescribeApps

instance ToBody DescribeApps where
    toBody = toBody . encode . _daStackId

newtype DescribeAppsResponse = DescribeAppsResponse
    { _darApps :: [App]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAppsResponse where
    type Item DescribeAppsResponse = App

    fromList = DescribeAppsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _darApps

-- | 'DescribeAppsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darApps' @::@ ['App']
--
describeAppsResponse :: DescribeAppsResponse
describeAppsResponse = DescribeAppsResponse
    { _darApps = mempty
    }

-- | An array of App objects that describe the specified apps.
darApps :: Lens' DescribeAppsResponse [App]
darApps = lens _darApps (\s a -> s { _darApps = a })

instance AWSRequest DescribeApps where
    type Sv DescribeApps = OpsWorks
    type Rs DescribeApps = DescribeAppsResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeAppsResponse
        <$> o .: "Apps"
