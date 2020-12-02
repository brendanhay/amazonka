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
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more instance that are part of a deployment group.
--
--
module Network.AWS.CodeDeploy.BatchGetDeploymentInstances
    (
    -- * Creating a Request
      batchGetDeploymentInstances
    , BatchGetDeploymentInstances
    -- * Request Lenses
    , bgdiDeploymentId
    , bgdiInstanceIds

    -- * Destructuring the Response
    , batchGetDeploymentInstancesResponse
    , BatchGetDeploymentInstancesResponse
    -- * Response Lenses
    , bgdirsInstancesSummary
    , bgdirsErrorMessage
    , bgdirsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetDeploymentInstances operation.
--
--
--
-- /See:/ 'batchGetDeploymentInstances' smart constructor.
data BatchGetDeploymentInstances = BatchGetDeploymentInstances'
  { _bgdiDeploymentId :: !Text
  , _bgdiInstanceIds  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeploymentInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdiDeploymentId' - The unique ID of a deployment.
--
-- * 'bgdiInstanceIds' - The unique IDs of instances in the deployment group.
batchGetDeploymentInstances
    :: Text -- ^ 'bgdiDeploymentId'
    -> BatchGetDeploymentInstances
batchGetDeploymentInstances pDeploymentId_ =
  BatchGetDeploymentInstances'
    {_bgdiDeploymentId = pDeploymentId_, _bgdiInstanceIds = mempty}


-- | The unique ID of a deployment.
bgdiDeploymentId :: Lens' BatchGetDeploymentInstances Text
bgdiDeploymentId = lens _bgdiDeploymentId (\ s a -> s{_bgdiDeploymentId = a})

-- | The unique IDs of instances in the deployment group.
bgdiInstanceIds :: Lens' BatchGetDeploymentInstances [Text]
bgdiInstanceIds = lens _bgdiInstanceIds (\ s a -> s{_bgdiInstanceIds = a}) . _Coerce

instance AWSRequest BatchGetDeploymentInstances where
        type Rs BatchGetDeploymentInstances =
             BatchGetDeploymentInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetDeploymentInstancesResponse' <$>
                   (x .?> "instancesSummary" .!@ mempty) <*>
                     (x .?> "errorMessage")
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetDeploymentInstances where

instance NFData BatchGetDeploymentInstances where

instance ToHeaders BatchGetDeploymentInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetDeploymentInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetDeploymentInstances where
        toJSON BatchGetDeploymentInstances'{..}
          = object
              (catMaybes
                 [Just ("deploymentId" .= _bgdiDeploymentId),
                  Just ("instanceIds" .= _bgdiInstanceIds)])

instance ToPath BatchGetDeploymentInstances where
        toPath = const "/"

instance ToQuery BatchGetDeploymentInstances where
        toQuery = const mempty

-- | Represents the output of a BatchGetDeploymentInstances operation.
--
--
--
-- /See:/ 'batchGetDeploymentInstancesResponse' smart constructor.
data BatchGetDeploymentInstancesResponse = BatchGetDeploymentInstancesResponse'
  { _bgdirsInstancesSummary :: !(Maybe [InstanceSummary])
  , _bgdirsErrorMessage     :: !(Maybe Text)
  , _bgdirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeploymentInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdirsInstancesSummary' - Information about the instance.
--
-- * 'bgdirsErrorMessage' - Information about errors that may have occurred during the API call.
--
-- * 'bgdirsResponseStatus' - -- | The response status code.
batchGetDeploymentInstancesResponse
    :: Int -- ^ 'bgdirsResponseStatus'
    -> BatchGetDeploymentInstancesResponse
batchGetDeploymentInstancesResponse pResponseStatus_ =
  BatchGetDeploymentInstancesResponse'
    { _bgdirsInstancesSummary = Nothing
    , _bgdirsErrorMessage = Nothing
    , _bgdirsResponseStatus = pResponseStatus_
    }


-- | Information about the instance.
bgdirsInstancesSummary :: Lens' BatchGetDeploymentInstancesResponse [InstanceSummary]
bgdirsInstancesSummary = lens _bgdirsInstancesSummary (\ s a -> s{_bgdirsInstancesSummary = a}) . _Default . _Coerce

-- | Information about errors that may have occurred during the API call.
bgdirsErrorMessage :: Lens' BatchGetDeploymentInstancesResponse (Maybe Text)
bgdirsErrorMessage = lens _bgdirsErrorMessage (\ s a -> s{_bgdirsErrorMessage = a})

-- | -- | The response status code.
bgdirsResponseStatus :: Lens' BatchGetDeploymentInstancesResponse Int
bgdirsResponseStatus = lens _bgdirsResponseStatus (\ s a -> s{_bgdirsResponseStatus = a})

instance NFData BatchGetDeploymentInstancesResponse
         where
