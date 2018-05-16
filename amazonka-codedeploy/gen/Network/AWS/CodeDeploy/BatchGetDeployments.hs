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
-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployments.
--
--
module Network.AWS.CodeDeploy.BatchGetDeployments
    (
    -- * Creating a Request
      batchGetDeployments
    , BatchGetDeployments
    -- * Request Lenses
    , bgdDeploymentIds

    -- * Destructuring the Response
    , batchGetDeploymentsResponse
    , BatchGetDeploymentsResponse
    -- * Response Lenses
    , bgdrsDeploymentsInfo
    , bgdrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetDeployments operation.
--
--
--
-- /See:/ 'batchGetDeployments' smart constructor.
newtype BatchGetDeployments = BatchGetDeployments'
  { _bgdDeploymentIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeployments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdDeploymentIds' - A list of deployment IDs, separated by spaces.
batchGetDeployments
    :: BatchGetDeployments
batchGetDeployments = BatchGetDeployments' {_bgdDeploymentIds = mempty}


-- | A list of deployment IDs, separated by spaces.
bgdDeploymentIds :: Lens' BatchGetDeployments [Text]
bgdDeploymentIds = lens _bgdDeploymentIds (\ s a -> s{_bgdDeploymentIds = a}) . _Coerce

instance AWSRequest BatchGetDeployments where
        type Rs BatchGetDeployments =
             BatchGetDeploymentsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetDeploymentsResponse' <$>
                   (x .?> "deploymentsInfo" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchGetDeployments where

instance NFData BatchGetDeployments where

instance ToHeaders BatchGetDeployments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetDeployments" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetDeployments where
        toJSON BatchGetDeployments'{..}
          = object
              (catMaybes
                 [Just ("deploymentIds" .= _bgdDeploymentIds)])

instance ToPath BatchGetDeployments where
        toPath = const "/"

instance ToQuery BatchGetDeployments where
        toQuery = const mempty

-- | Represents the output of a BatchGetDeployments operation.
--
--
--
-- /See:/ 'batchGetDeploymentsResponse' smart constructor.
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
  { _bgdrsDeploymentsInfo :: !(Maybe [DeploymentInfo])
  , _bgdrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeploymentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdrsDeploymentsInfo' - Information about the deployments.
--
-- * 'bgdrsResponseStatus' - -- | The response status code.
batchGetDeploymentsResponse
    :: Int -- ^ 'bgdrsResponseStatus'
    -> BatchGetDeploymentsResponse
batchGetDeploymentsResponse pResponseStatus_ =
  BatchGetDeploymentsResponse'
    {_bgdrsDeploymentsInfo = Nothing, _bgdrsResponseStatus = pResponseStatus_}


-- | Information about the deployments.
bgdrsDeploymentsInfo :: Lens' BatchGetDeploymentsResponse [DeploymentInfo]
bgdrsDeploymentsInfo = lens _bgdrsDeploymentsInfo (\ s a -> s{_bgdrsDeploymentsInfo = a}) . _Default . _Coerce

-- | -- | The response status code.
bgdrsResponseStatus :: Lens' BatchGetDeploymentsResponse Int
bgdrsResponseStatus = lens _bgdrsResponseStatus (\ s a -> s{_bgdrsResponseStatus = a})

instance NFData BatchGetDeploymentsResponse where
