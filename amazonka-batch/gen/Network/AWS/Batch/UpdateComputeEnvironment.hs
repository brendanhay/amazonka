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
-- Module      : Network.AWS.Batch.UpdateComputeEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AWS Batch compute environment.
--
--
module Network.AWS.Batch.UpdateComputeEnvironment
    (
    -- * Creating a Request
      updateComputeEnvironment
    , UpdateComputeEnvironment
    -- * Request Lenses
    , uceState
    , uceComputeResources
    , uceServiceRole
    , uceComputeEnvironment

    -- * Destructuring the Response
    , updateComputeEnvironmentResponse
    , UpdateComputeEnvironmentResponse
    -- * Response Lenses
    , ucersComputeEnvironmentName
    , ucersComputeEnvironmentARN
    , ucersResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateComputeEnvironment' smart constructor.
data UpdateComputeEnvironment = UpdateComputeEnvironment'
  { _uceState              :: !(Maybe CEState)
  , _uceComputeResources   :: !(Maybe ComputeResourceUpdate)
  , _uceServiceRole        :: !(Maybe Text)
  , _uceComputeEnvironment :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateComputeEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uceState' - The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
--
-- * 'uceComputeResources' - Details of the compute resources managed by the compute environment. Required for a managed compute environment.
--
-- * 'uceServiceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf. If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
--
-- * 'uceComputeEnvironment' - The name or full Amazon Resource Name (ARN) of the compute environment to update.
updateComputeEnvironment
    :: Text -- ^ 'uceComputeEnvironment'
    -> UpdateComputeEnvironment
updateComputeEnvironment pComputeEnvironment_ =
  UpdateComputeEnvironment'
    { _uceState = Nothing
    , _uceComputeResources = Nothing
    , _uceServiceRole = Nothing
    , _uceComputeEnvironment = pComputeEnvironment_
    }


-- | The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
uceState :: Lens' UpdateComputeEnvironment (Maybe CEState)
uceState = lens _uceState (\ s a -> s{_uceState = a})

-- | Details of the compute resources managed by the compute environment. Required for a managed compute environment.
uceComputeResources :: Lens' UpdateComputeEnvironment (Maybe ComputeResourceUpdate)
uceComputeResources = lens _uceComputeResources (\ s a -> s{_uceComputeResources = a})

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf. If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
uceServiceRole :: Lens' UpdateComputeEnvironment (Maybe Text)
uceServiceRole = lens _uceServiceRole (\ s a -> s{_uceServiceRole = a})

-- | The name or full Amazon Resource Name (ARN) of the compute environment to update.
uceComputeEnvironment :: Lens' UpdateComputeEnvironment Text
uceComputeEnvironment = lens _uceComputeEnvironment (\ s a -> s{_uceComputeEnvironment = a})

instance AWSRequest UpdateComputeEnvironment where
        type Rs UpdateComputeEnvironment =
             UpdateComputeEnvironmentResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 UpdateComputeEnvironmentResponse' <$>
                   (x .?> "computeEnvironmentName") <*>
                     (x .?> "computeEnvironmentArn")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateComputeEnvironment where

instance NFData UpdateComputeEnvironment where

instance ToHeaders UpdateComputeEnvironment where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateComputeEnvironment where
        toJSON UpdateComputeEnvironment'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _uceState,
                  ("computeResources" .=) <$> _uceComputeResources,
                  ("serviceRole" .=) <$> _uceServiceRole,
                  Just
                    ("computeEnvironment" .= _uceComputeEnvironment)])

instance ToPath UpdateComputeEnvironment where
        toPath = const "/v1/updatecomputeenvironment"

instance ToQuery UpdateComputeEnvironment where
        toQuery = const mempty

-- | /See:/ 'updateComputeEnvironmentResponse' smart constructor.
data UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse'
  { _ucersComputeEnvironmentName :: !(Maybe Text)
  , _ucersComputeEnvironmentARN  :: !(Maybe Text)
  , _ucersResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateComputeEnvironmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucersComputeEnvironmentName' - The name of compute environment.
--
-- * 'ucersComputeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
--
-- * 'ucersResponseStatus' - -- | The response status code.
updateComputeEnvironmentResponse
    :: Int -- ^ 'ucersResponseStatus'
    -> UpdateComputeEnvironmentResponse
updateComputeEnvironmentResponse pResponseStatus_ =
  UpdateComputeEnvironmentResponse'
    { _ucersComputeEnvironmentName = Nothing
    , _ucersComputeEnvironmentARN = Nothing
    , _ucersResponseStatus = pResponseStatus_
    }


-- | The name of compute environment.
ucersComputeEnvironmentName :: Lens' UpdateComputeEnvironmentResponse (Maybe Text)
ucersComputeEnvironmentName = lens _ucersComputeEnvironmentName (\ s a -> s{_ucersComputeEnvironmentName = a})

-- | The Amazon Resource Name (ARN) of the compute environment.
ucersComputeEnvironmentARN :: Lens' UpdateComputeEnvironmentResponse (Maybe Text)
ucersComputeEnvironmentARN = lens _ucersComputeEnvironmentARN (\ s a -> s{_ucersComputeEnvironmentARN = a})

-- | -- | The response status code.
ucersResponseStatus :: Lens' UpdateComputeEnvironmentResponse Int
ucersResponseStatus = lens _ucersResponseStatus (\ s a -> s{_ucersResponseStatus = a})

instance NFData UpdateComputeEnvironmentResponse
         where
