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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the @MLModel@ . The endpoint contains the URI of the @MLModel@ ; that is, the location to send real-time prediction requests for the specified @MLModel@ .
--
--
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
    , crersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRealtimeEndpoint' smart constructor.
newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint'
  { _creMLModelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRealtimeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creMLModelId' - The ID assigned to the @MLModel@ during creation.
createRealtimeEndpoint
    :: Text -- ^ 'creMLModelId'
    -> CreateRealtimeEndpoint
createRealtimeEndpoint pMLModelId_ =
  CreateRealtimeEndpoint' {_creMLModelId = pMLModelId_}


-- | The ID assigned to the @MLModel@ during creation.
creMLModelId :: Lens' CreateRealtimeEndpoint Text
creMLModelId = lens _creMLModelId (\ s a -> s{_creMLModelId = a})

instance AWSRequest CreateRealtimeEndpoint where
        type Rs CreateRealtimeEndpoint =
             CreateRealtimeEndpointResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 CreateRealtimeEndpointResponse' <$>
                   (x .?> "RealtimeEndpointInfo") <*>
                     (x .?> "MLModelId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateRealtimeEndpoint where

instance NFData CreateRealtimeEndpoint where

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
          = object
              (catMaybes [Just ("MLModelId" .= _creMLModelId)])

instance ToPath CreateRealtimeEndpoint where
        toPath = const "/"

instance ToQuery CreateRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an @CreateRealtimeEndpoint@ operation.
--
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
--
-- /See:/ 'createRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
  { _crersRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
  , _crersMLModelId            :: !(Maybe Text)
  , _crersResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersRealtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- * 'crersMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- * 'crersResponseStatus' - -- | The response status code.
createRealtimeEndpointResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateRealtimeEndpointResponse
createRealtimeEndpointResponse pResponseStatus_ =
  CreateRealtimeEndpointResponse'
    { _crersRealtimeEndpointInfo = Nothing
    , _crersMLModelId = Nothing
    , _crersResponseStatus = pResponseStatus_
    }


-- | The endpoint information of the @MLModel@
crersRealtimeEndpointInfo :: Lens' CreateRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
crersRealtimeEndpointInfo = lens _crersRealtimeEndpointInfo (\ s a -> s{_crersRealtimeEndpointInfo = a})

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
crersMLModelId :: Lens' CreateRealtimeEndpointResponse (Maybe Text)
crersMLModelId = lens _crersMLModelId (\ s a -> s{_crersMLModelId = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateRealtimeEndpointResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateRealtimeEndpointResponse where
