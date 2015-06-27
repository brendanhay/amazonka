{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a description of a specified set of apps.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeApps.html>
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
    , darStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeApps' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAppIds'
--
-- * 'daStackId'
data DescribeApps = DescribeApps'
    { _daAppIds  :: Maybe [Text]
    , _daStackId :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DescribeApps' smart constructor.
describeApps :: DescribeApps
describeApps =
    DescribeApps'
    { _daAppIds = Nothing
    , _daStackId = Nothing
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, @DescribeApps@ returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
daAppIds :: Lens' DescribeApps [Text]
daAppIds = lens _daAppIds (\ s a -> s{_daAppIds = a}) . _Default;

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
-- description of the apps in the specified stack.
daStackId :: Lens' DescribeApps (Maybe Text)
daStackId = lens _daStackId (\ s a -> s{_daStackId = a});

instance AWSRequest DescribeApps where
        type Sv DescribeApps = OpsWorks
        type Rs DescribeApps = DescribeAppsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAppsResponse' <$>
                   (x .?> "Apps" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders DescribeApps where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeApps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeApps where
        toJSON DescribeApps'{..}
          = object
              ["AppIds" .= _daAppIds, "StackId" .= _daStackId]

instance ToPath DescribeApps where
        toPath = const "/"

instance ToQuery DescribeApps where
        toQuery = const mempty

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'describeAppsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darApps'
--
-- * 'darStatus'
data DescribeAppsResponse = DescribeAppsResponse'
    { _darApps   :: Maybe [App]
    , _darStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeAppsResponse' smart constructor.
describeAppsResponse :: Int -> DescribeAppsResponse
describeAppsResponse pStatus =
    DescribeAppsResponse'
    { _darApps = Nothing
    , _darStatus = pStatus
    }

-- | An array of @App@ objects that describe the specified apps.
darApps :: Lens' DescribeAppsResponse [App]
darApps = lens _darApps (\ s a -> s{_darApps = a}) . _Default;

-- | FIXME: Undocumented member.
darStatus :: Lens' DescribeAppsResponse Int
darStatus = lens _darStatus (\ s a -> s{_darStatus = a});
