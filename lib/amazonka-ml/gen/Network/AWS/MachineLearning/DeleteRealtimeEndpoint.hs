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
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an @MLModel@ .
--
--
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
    (
    -- * Creating a Request
      deleteRealtimeEndpoint
    , DeleteRealtimeEndpoint
    -- * Request Lenses
    , dreMLModelId

    -- * Destructuring the Response
    , deleteRealtimeEndpointResponse
    , DeleteRealtimeEndpointResponse
    -- * Response Lenses
    , drersRealtimeEndpointInfo
    , drersMLModelId
    , drersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRealtimeEndpoint' smart constructor.
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
  { _dreMLModelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRealtimeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreMLModelId' - The ID assigned to the @MLModel@ during creation.
deleteRealtimeEndpoint
    :: Text -- ^ 'dreMLModelId'
    -> DeleteRealtimeEndpoint
deleteRealtimeEndpoint pMLModelId_ =
  DeleteRealtimeEndpoint' {_dreMLModelId = pMLModelId_}


-- | The ID assigned to the @MLModel@ during creation.
dreMLModelId :: Lens' DeleteRealtimeEndpoint Text
dreMLModelId = lens _dreMLModelId (\ s a -> s{_dreMLModelId = a})

instance AWSRequest DeleteRealtimeEndpoint where
        type Rs DeleteRealtimeEndpoint =
             DeleteRealtimeEndpointResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRealtimeEndpointResponse' <$>
                   (x .?> "RealtimeEndpointInfo") <*>
                     (x .?> "MLModelId")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteRealtimeEndpoint where

instance NFData DeleteRealtimeEndpoint where

instance ToHeaders DeleteRealtimeEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteRealtimeEndpoint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRealtimeEndpoint where
        toJSON DeleteRealtimeEndpoint'{..}
          = object
              (catMaybes [Just ("MLModelId" .= _dreMLModelId)])

instance ToPath DeleteRealtimeEndpoint where
        toPath = const "/"

instance ToQuery DeleteRealtimeEndpoint where
        toQuery = const mempty

-- | Represents the output of an @DeleteRealtimeEndpoint@ operation.
--
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
--
-- /See:/ 'deleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
  { _drersRealtimeEndpointInfo :: !(Maybe RealtimeEndpointInfo)
  , _drersMLModelId            :: !(Maybe Text)
  , _drersResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drersRealtimeEndpointInfo' - The endpoint information of the @MLModel@
--
-- * 'drersMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- * 'drersResponseStatus' - -- | The response status code.
deleteRealtimeEndpointResponse
    :: Int -- ^ 'drersResponseStatus'
    -> DeleteRealtimeEndpointResponse
deleteRealtimeEndpointResponse pResponseStatus_ =
  DeleteRealtimeEndpointResponse'
    { _drersRealtimeEndpointInfo = Nothing
    , _drersMLModelId = Nothing
    , _drersResponseStatus = pResponseStatus_
    }


-- | The endpoint information of the @MLModel@
drersRealtimeEndpointInfo :: Lens' DeleteRealtimeEndpointResponse (Maybe RealtimeEndpointInfo)
drersRealtimeEndpointInfo = lens _drersRealtimeEndpointInfo (\ s a -> s{_drersRealtimeEndpointInfo = a})

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
drersMLModelId :: Lens' DeleteRealtimeEndpointResponse (Maybe Text)
drersMLModelId = lens _drersMLModelId (\ s a -> s{_drersMLModelId = a})

-- | -- | The response status code.
drersResponseStatus :: Lens' DeleteRealtimeEndpointResponse Int
drersResponseStatus = lens _drersResponseStatus (\ s a -> s{_drersResponseStatus = a})

instance NFData DeleteRealtimeEndpointResponse where
