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
-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of deployments.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeDeployments
    (
    -- * Creating a Request
      describeDeployments
    , DescribeDeployments
    -- * Request Lenses
    , ddAppId
    , ddDeploymentIds
    , ddStackId

    -- * Destructuring the Response
    , describeDeploymentsResponse
    , DescribeDeploymentsResponse
    -- * Response Lenses
    , ddrsDeployments
    , ddrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDeployments' smart constructor.
data DescribeDeployments = DescribeDeployments'
  { _ddAppId         :: !(Maybe Text)
  , _ddDeploymentIds :: !(Maybe [Text])
  , _ddStackId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddAppId' - The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
--
-- * 'ddDeploymentIds' - An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
--
-- * 'ddStackId' - The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
describeDeployments
    :: DescribeDeployments
describeDeployments =
  DescribeDeployments'
    {_ddAppId = Nothing, _ddDeploymentIds = Nothing, _ddStackId = Nothing}


-- | The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
ddAppId :: Lens' DescribeDeployments (Maybe Text)
ddAppId = lens _ddAppId (\ s a -> s{_ddAppId = a})

-- | An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
ddDeploymentIds :: Lens' DescribeDeployments [Text]
ddDeploymentIds = lens _ddDeploymentIds (\ s a -> s{_ddDeploymentIds = a}) . _Default . _Coerce

-- | The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
ddStackId :: Lens' DescribeDeployments (Maybe Text)
ddStackId = lens _ddStackId (\ s a -> s{_ddStackId = a})

instance AWSRequest DescribeDeployments where
        type Rs DescribeDeployments =
             DescribeDeploymentsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeploymentsResponse' <$>
                   (x .?> "Deployments" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDeployments where

instance NFData DescribeDeployments where

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
              (catMaybes
                 [("AppId" .=) <$> _ddAppId,
                  ("DeploymentIds" .=) <$> _ddDeploymentIds,
                  ("StackId" .=) <$> _ddStackId])

instance ToPath DescribeDeployments where
        toPath = const "/"

instance ToQuery DescribeDeployments where
        toQuery = const mempty

-- | Contains the response to a @DescribeDeployments@ request.
--
--
--
-- /See:/ 'describeDeploymentsResponse' smart constructor.
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
  { _ddrsDeployments    :: !(Maybe [Deployment])
  , _ddrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsDeployments' - An array of @Deployment@ objects that describe the deployments.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeDeploymentsResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DescribeDeploymentsResponse
describeDeploymentsResponse pResponseStatus_ =
  DescribeDeploymentsResponse'
    {_ddrsDeployments = Nothing, _ddrsResponseStatus = pResponseStatus_}


-- | An array of @Deployment@ objects that describe the deployments.
ddrsDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrsDeployments = lens _ddrsDeployments (\ s a -> s{_ddrsDeployments = a}) . _Default . _Coerce

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeDeploymentsResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DescribeDeploymentsResponse where
