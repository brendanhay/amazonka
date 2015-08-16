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
-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the 'MLModel'. The endpoint contains
-- the URI of the 'MLModel'; that is, the location to send real-time
-- prediction requests for the specified 'MLModel'.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateRealtimeEndpoint.html AWS API Reference> for CreateRealtimeEndpoint.
module Network.AWS.MachineLearning.CreateRealtimeEndpoint
    (
    -- * Creating a Request
      createRealtimeEndpoint
    , CreateRealtimeEndpoint
    -- * Request Lenses
    , creMLModelId

    -- * Destructuring the Response
    , createRealtimeEndpointResponse
    , CreateRealtimeEndpointResponse
    -- * Response Lenses
    , crersRealtimeEndpointInfo
    , crersMLModelId
    , crersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRealtimeEndpoint' smart constructor.
newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint'
    { _creMLModelId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRealtimeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creMLModelId'
createRealtimeEndpoint
    :: Text -- ^ 'creMLModelId'
    -> CreateRealtimeEndpoint
createRealtimeEndpoint pMLModelId_ =
    CreateRealtimeEndpoint'
    { _creMLModelId = pMLModelId_
    }

-- | The ID assigned to the 'MLModel' during creation.
creMLModelId :: Lens' CreateRealtimeEndpoint Text
creMLModelId = lens _creMLModelId (\ s a -> s{_creMLModelId = a});

instance AWSRequest CreateRealtimeEndpoint where
        type Sv CreateRealtimeEndpoint = MachineLearning
        type Rs CreateRealtimeEndpoint =
             CreateRealtimeEndpointResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateRealtimeEndpointResponse' <$>
                   (x .?> "RealtimeEndpointInfo") <*>
                     (x .?> "MLModelId")
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateRealtimeEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.CreateRealtimeEndpoint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRealtimeEndpoint where
        toJSON CreateRealtimeEndpoint'{..}
          = object ["MLModelId" .= _creMLModelId]

instance ToPath CreateRealtimeEndpoint where
        toPath = const "/"

instance ToQuery CreateRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an CreateRealtimeEndpoint operation.
--
-- The result contains the 'MLModelId' and the endpoint information for the
-- 'MLModel'.
--
-- The endpoint information includes the URI of the 'MLModel'; that is, the
-- location to send online prediction requests for the specified 'MLModel'.
--
-- /See:/ 'createRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
    { _crersRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
    , _crersMLModelId            :: !(Maybe Text)
    , _crersStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersRealtimeEndpointInfo'
--
-- * 'crersMLModelId'
--
-- * 'crersStatus'
createRealtimeEndpointResponse
    :: Int -- ^ 'crersStatus'
    -> CreateRealtimeEndpointResponse
createRealtimeEndpointResponse pStatus_ =
    CreateRealtimeEndpointResponse'
    { _crersRealtimeEndpointInfo = Nothing
    , _crersMLModelId = Nothing
    , _crersStatus = pStatus_
    }

-- | The endpoint information of the 'MLModel'
crersRealtimeEndpointInfo :: Lens' CreateRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
crersRealtimeEndpointInfo = lens _crersRealtimeEndpointInfo (\ s a -> s{_crersRealtimeEndpointInfo = a});

-- | A user-supplied ID that uniquely identifies the 'MLModel'. This value
-- should be identical to the value of the 'MLModelId' in the request.
crersMLModelId :: Lens' CreateRealtimeEndpointResponse (Maybe Text)
crersMLModelId = lens _crersMLModelId (\ s a -> s{_crersMLModelId = a});

-- | The response status code.
crersStatus :: Lens' CreateRealtimeEndpointResponse Int
crersStatus = lens _crersStatus (\ s a -> s{_crersStatus = a});
