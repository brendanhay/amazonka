{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
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
    , darqAppIds
    , darqStackId

    -- * Response
    , DescribeAppsResponse
    -- ** Response constructor
    , describeAppsResponse
    -- ** Response lenses
    , darsApps
    , darsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeApps' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqAppIds'
--
-- * 'darqStackId'
data DescribeApps = DescribeApps'
    { _darqAppIds  :: !(Maybe [Text])
    , _darqStackId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeApps' smart constructor.
describeApps :: DescribeApps
describeApps =
    DescribeApps'
    { _darqAppIds = Nothing
    , _darqStackId = Nothing
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, @DescribeApps@ returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
darqAppIds :: Lens' DescribeApps [Text]
darqAppIds = lens _darqAppIds (\ s a -> s{_darqAppIds = a}) . _Default;

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a
-- description of the apps in the specified stack.
darqStackId :: Lens' DescribeApps (Maybe Text)
darqStackId = lens _darqStackId (\ s a -> s{_darqStackId = a});

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
              ["AppIds" .= _darqAppIds, "StackId" .= _darqStackId]

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
-- * 'darsApps'
--
-- * 'darsStatus'
data DescribeAppsResponse = DescribeAppsResponse'
    { _darsApps   :: !(Maybe [App])
    , _darsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAppsResponse' smart constructor.
describeAppsResponse :: Int -> DescribeAppsResponse
describeAppsResponse pStatus =
    DescribeAppsResponse'
    { _darsApps = Nothing
    , _darsStatus = pStatus
    }

-- | An array of @App@ objects that describe the specified apps.
darsApps :: Lens' DescribeAppsResponse [App]
darsApps = lens _darsApps (\ s a -> s{_darsApps = a}) . _Default;

-- | FIXME: Undocumented member.
darsStatus :: Lens' DescribeAppsResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
