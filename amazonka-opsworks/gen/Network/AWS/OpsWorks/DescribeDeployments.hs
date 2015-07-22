{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of deployments.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeDeployments.html>
module Network.AWS.OpsWorks.DescribeDeployments
    (
    -- * Request
      DescribeDeployments
    -- ** Request constructor
    , describeDeployments
    -- ** Request lenses
    , ddrqAppId
    , ddrqDeploymentIds
    , ddrqStackId

    -- * Response
    , DescribeDeploymentsResponse
    -- ** Response constructor
    , describeDeploymentsResponse
    -- ** Response lenses
    , ddrsDeployments
    , ddrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDeployments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrqAppId'
--
-- * 'ddrqDeploymentIds'
--
-- * 'ddrqStackId'
data DescribeDeployments = DescribeDeployments'
    { _ddrqAppId         :: !(Maybe Text)
    , _ddrqDeploymentIds :: !(Maybe [Text])
    , _ddrqStackId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDeployments' smart constructor.
describeDeployments :: DescribeDeployments
describeDeployments =
    DescribeDeployments'
    { _ddrqAppId = Nothing
    , _ddrqDeploymentIds = Nothing
    , _ddrqStackId = Nothing
    }

-- | The app ID. If you include this parameter, @DescribeDeployments@ returns
-- a description of the commands associated with the specified app.
ddrqAppId :: Lens' DescribeDeployments (Maybe Text)
ddrqAppId = lens _ddrqAppId (\ s a -> s{_ddrqAppId = a});

-- | An array of deployment IDs to be described. If you include this
-- parameter, @DescribeDeployments@ returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
ddrqDeploymentIds :: Lens' DescribeDeployments [Text]
ddrqDeploymentIds = lens _ddrqDeploymentIds (\ s a -> s{_ddrqDeploymentIds = a}) . _Default;

-- | The stack ID. If you include this parameter, @DescribeDeployments@
-- returns a description of the commands associated with the specified
-- stack.
ddrqStackId :: Lens' DescribeDeployments (Maybe Text)
ddrqStackId = lens _ddrqStackId (\ s a -> s{_ddrqStackId = a});

instance AWSRequest DescribeDeployments where
        type Sv DescribeDeployments = OpsWorks
        type Rs DescribeDeployments =
             DescribeDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeploymentsResponse' <$>
                   (x .?> "Deployments" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeDeployments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeDeployments" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDeployments where
        toJSON DescribeDeployments'{..}
          = object
              ["AppId" .= _ddrqAppId,
               "DeploymentIds" .= _ddrqDeploymentIds,
               "StackId" .= _ddrqStackId]

instance ToPath DescribeDeployments where
        toPath = const "/"

instance ToQuery DescribeDeployments where
        toQuery = const mempty

-- | Contains the response to a @DescribeDeployments@ request.
--
-- /See:/ 'describeDeploymentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrsDeployments'
--
-- * 'ddrsStatus'
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
    { _ddrsDeployments :: !(Maybe [Deployment])
    , _ddrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDeploymentsResponse' smart constructor.
describeDeploymentsResponse :: Int -> DescribeDeploymentsResponse
describeDeploymentsResponse pStatus =
    DescribeDeploymentsResponse'
    { _ddrsDeployments = Nothing
    , _ddrsStatus = pStatus
    }

-- | An array of @Deployment@ objects that describe the deployments.
ddrsDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrsDeployments = lens _ddrsDeployments (\ s a -> s{_ddrsDeployments = a}) . _Default;

-- | FIXME: Undocumented member.
ddrsStatus :: Lens' DescribeDeploymentsResponse Int
ddrsStatus = lens _ddrsStatus (\ s a -> s{_ddrsStatus = a});
