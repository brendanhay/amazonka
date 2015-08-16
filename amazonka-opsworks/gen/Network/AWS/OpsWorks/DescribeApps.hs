{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeApps.html AWS API Reference> for DescribeApps.
module Network.AWS.OpsWorks.DescribeApps
    (
    -- * Creating a Request
      describeApps
    , DescribeApps
    -- * Request Lenses
    , daAppIds
    , daStackId

    -- * Destructuring the Response
    , describeAppsResponse
    , DescribeAppsResponse
    -- * Response Lenses
    , darsApps
    , darsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeApps' smart constructor.
data DescribeApps = DescribeApps'
    { _daAppIds  :: !(Maybe [Text])
    , _daStackId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeApps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAppIds'
--
-- * 'daStackId'
describeApps
    :: DescribeApps
describeApps =
    DescribeApps'
    { _daAppIds = Nothing
    , _daStackId = Nothing
    }

-- | An array of app IDs for the apps to be described. If you use this
-- parameter, 'DescribeApps' returns a description of the specified apps.
-- Otherwise, it returns a description of every app.
daAppIds :: Lens' DescribeApps [Text]
daAppIds = lens _daAppIds (\ s a -> s{_daAppIds = a}) . _Default . _Coerce;

-- | The app stack ID. If you use this parameter, 'DescribeApps' returns a
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

-- | Contains the response to a 'DescribeApps' request.
--
-- /See:/ 'describeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
    { _darsApps   :: !(Maybe [App])
    , _darsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAppsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsApps'
--
-- * 'darsStatus'
describeAppsResponse
    :: Int -- ^ 'darsStatus'
    -> DescribeAppsResponse
describeAppsResponse pStatus_ =
    DescribeAppsResponse'
    { _darsApps = Nothing
    , _darsStatus = pStatus_
    }

-- | An array of 'App' objects that describe the specified apps.
darsApps :: Lens' DescribeAppsResponse [App]
darsApps = lens _darsApps (\ s a -> s{_darsApps = a}) . _Default . _Coerce;

-- | The response status code.
darsStatus :: Lens' DescribeAppsResponse Int
darsStatus = lens _darsStatus (\ s a -> s{_darsStatus = a});
