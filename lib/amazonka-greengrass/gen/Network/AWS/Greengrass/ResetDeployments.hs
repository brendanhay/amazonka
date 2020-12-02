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
-- Module      : Network.AWS.Greengrass.ResetDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a group's deployments.
module Network.AWS.Greengrass.ResetDeployments
    (
    -- * Creating a Request
      resetDeployments
    , ResetDeployments
    -- * Request Lenses
    , rdAmznClientToken
    , rdForce
    , rdGroupId

    -- * Destructuring the Response
    , resetDeploymentsResponse
    , ResetDeploymentsResponse
    -- * Response Lenses
    , rdrsDeploymentId
    , rdrsDeploymentARN
    , rdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Information needed to reset deployments.
--
-- /See:/ 'resetDeployments' smart constructor.
data ResetDeployments = ResetDeployments'
  { _rdAmznClientToken :: !(Maybe Text)
  , _rdForce           :: !(Maybe Bool)
  , _rdGroupId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'rdForce' - If true, performs a best-effort only core reset.
--
-- * 'rdGroupId' - The ID of the AWS Greengrass group.
resetDeployments
    :: Text -- ^ 'rdGroupId'
    -> ResetDeployments
resetDeployments pGroupId_ =
  ResetDeployments'
    {_rdAmznClientToken = Nothing, _rdForce = Nothing, _rdGroupId = pGroupId_}


-- | A client token used to correlate requests and responses.
rdAmznClientToken :: Lens' ResetDeployments (Maybe Text)
rdAmznClientToken = lens _rdAmznClientToken (\ s a -> s{_rdAmznClientToken = a})

-- | If true, performs a best-effort only core reset.
rdForce :: Lens' ResetDeployments (Maybe Bool)
rdForce = lens _rdForce (\ s a -> s{_rdForce = a})

-- | The ID of the AWS Greengrass group.
rdGroupId :: Lens' ResetDeployments Text
rdGroupId = lens _rdGroupId (\ s a -> s{_rdGroupId = a})

instance AWSRequest ResetDeployments where
        type Rs ResetDeployments = ResetDeploymentsResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ResetDeploymentsResponse' <$>
                   (x .?> "DeploymentId") <*> (x .?> "DeploymentArn")
                     <*> (pure (fromEnum s)))

instance Hashable ResetDeployments where

instance NFData ResetDeployments where

instance ToHeaders ResetDeployments where
        toHeaders ResetDeployments'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _rdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON ResetDeployments where
        toJSON ResetDeployments'{..}
          = object (catMaybes [("Force" .=) <$> _rdForce])

instance ToPath ResetDeployments where
        toPath ResetDeployments'{..}
          = mconcat
              ["/greengrass/groups/", toBS _rdGroupId,
               "/deployments/$reset"]

instance ToQuery ResetDeployments where
        toQuery = const mempty

-- | /See:/ 'resetDeploymentsResponse' smart constructor.
data ResetDeploymentsResponse = ResetDeploymentsResponse'
  { _rdrsDeploymentId   :: !(Maybe Text)
  , _rdrsDeploymentARN  :: !(Maybe Text)
  , _rdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdrsDeploymentId' - The ID of the deployment.
--
-- * 'rdrsDeploymentARN' - The ARN of the deployment.
--
-- * 'rdrsResponseStatus' - -- | The response status code.
resetDeploymentsResponse
    :: Int -- ^ 'rdrsResponseStatus'
    -> ResetDeploymentsResponse
resetDeploymentsResponse pResponseStatus_ =
  ResetDeploymentsResponse'
    { _rdrsDeploymentId = Nothing
    , _rdrsDeploymentARN = Nothing
    , _rdrsResponseStatus = pResponseStatus_
    }


-- | The ID of the deployment.
rdrsDeploymentId :: Lens' ResetDeploymentsResponse (Maybe Text)
rdrsDeploymentId = lens _rdrsDeploymentId (\ s a -> s{_rdrsDeploymentId = a})

-- | The ARN of the deployment.
rdrsDeploymentARN :: Lens' ResetDeploymentsResponse (Maybe Text)
rdrsDeploymentARN = lens _rdrsDeploymentARN (\ s a -> s{_rdrsDeploymentARN = a})

-- | -- | The response status code.
rdrsResponseStatus :: Lens' ResetDeploymentsResponse Int
rdrsResponseStatus = lens _rdrsResponseStatus (\ s a -> s{_rdrsResponseStatus = a})

instance NFData ResetDeploymentsResponse where
