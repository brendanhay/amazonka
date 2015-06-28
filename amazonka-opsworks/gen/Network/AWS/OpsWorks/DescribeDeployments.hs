{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeDeployments
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

-- | Requests a description of a specified set of deployments.
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
    , ddAppId
    , ddDeploymentIds
    , ddStackId

    -- * Response
    , DescribeDeploymentsResponse
    -- ** Response constructor
    , describeDeploymentsResponse
    -- ** Response lenses
    , ddrDeployments
    , ddrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDeployments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddAppId'
--
-- * 'ddDeploymentIds'
--
-- * 'ddStackId'
data DescribeDeployments = DescribeDeployments'
    { _ddAppId         :: !(Maybe Text)
    , _ddDeploymentIds :: !(Maybe [Text])
    , _ddStackId       :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeDeployments' smart constructor.
describeDeployments :: DescribeDeployments
describeDeployments =
    DescribeDeployments'
    { _ddAppId = Nothing
    , _ddDeploymentIds = Nothing
    , _ddStackId = Nothing
    }

-- | The app ID. If you include this parameter, @DescribeDeployments@ returns
-- a description of the commands associated with the specified app.
ddAppId :: Lens' DescribeDeployments (Maybe Text)
ddAppId = lens _ddAppId (\ s a -> s{_ddAppId = a});

-- | An array of deployment IDs to be described. If you include this
-- parameter, @DescribeDeployments@ returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
ddDeploymentIds :: Lens' DescribeDeployments [Text]
ddDeploymentIds = lens _ddDeploymentIds (\ s a -> s{_ddDeploymentIds = a}) . _Default;

-- | The stack ID. If you include this parameter, @DescribeDeployments@
-- returns a description of the commands associated with the specified
-- stack.
ddStackId :: Lens' DescribeDeployments (Maybe Text)
ddStackId = lens _ddStackId (\ s a -> s{_ddStackId = a});

instance AWSRequest DescribeDeployments where
        type Sv DescribeDeployments = OpsWorks
        type Rs DescribeDeployments =
             DescribeDeploymentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeploymentsResponse' <$>
                   (x .?> "Deployments" .!@ mempty) <*> (pure s))

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
              ["AppId" .= _ddAppId,
               "DeploymentIds" .= _ddDeploymentIds,
               "StackId" .= _ddStackId]

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
-- * 'ddrDeployments'
--
-- * 'ddrStatus'
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
    { _ddrDeployments :: !(Maybe [Deployment])
    , _ddrStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeDeploymentsResponse' smart constructor.
describeDeploymentsResponse :: Status -> DescribeDeploymentsResponse
describeDeploymentsResponse pStatus =
    DescribeDeploymentsResponse'
    { _ddrDeployments = Nothing
    , _ddrStatus = pStatus
    }

-- | An array of @Deployment@ objects that describe the deployments.
ddrDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrDeployments = lens _ddrDeployments (\ s a -> s{_ddrDeployments = a}) . _Default;

-- | FIXME: Undocumented member.
ddrStatus :: Lens' DescribeDeploymentsResponse Status
ddrStatus = lens _ddrStatus (\ s a -> s{_ddrStatus = a});
