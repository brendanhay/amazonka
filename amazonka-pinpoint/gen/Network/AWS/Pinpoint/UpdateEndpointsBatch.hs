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
-- Module      : Network.AWS.Pinpoint.UpdateEndpointsBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to update a batch of endpoints.
module Network.AWS.Pinpoint.UpdateEndpointsBatch
    (
    -- * Creating a Request
      updateEndpointsBatch
    , UpdateEndpointsBatch
    -- * Request Lenses
    , uebApplicationId
    , uebEndpointBatchRequest

    -- * Destructuring the Response
    , updateEndpointsBatchResponse
    , UpdateEndpointsBatchResponse
    -- * Response Lenses
    , uebrsResponseStatus
    , uebrsMessageBody
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEndpointsBatch' smart constructor.
data UpdateEndpointsBatch = UpdateEndpointsBatch'
  { _uebApplicationId        :: !Text
  , _uebEndpointBatchRequest :: !EndpointBatchRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointsBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uebApplicationId' - Undocumented member.
--
-- * 'uebEndpointBatchRequest' - Undocumented member.
updateEndpointsBatch
    :: Text -- ^ 'uebApplicationId'
    -> EndpointBatchRequest -- ^ 'uebEndpointBatchRequest'
    -> UpdateEndpointsBatch
updateEndpointsBatch pApplicationId_ pEndpointBatchRequest_ =
  UpdateEndpointsBatch'
    { _uebApplicationId = pApplicationId_
    , _uebEndpointBatchRequest = pEndpointBatchRequest_
    }


-- | Undocumented member.
uebApplicationId :: Lens' UpdateEndpointsBatch Text
uebApplicationId = lens _uebApplicationId (\ s a -> s{_uebApplicationId = a})

-- | Undocumented member.
uebEndpointBatchRequest :: Lens' UpdateEndpointsBatch EndpointBatchRequest
uebEndpointBatchRequest = lens _uebEndpointBatchRequest (\ s a -> s{_uebEndpointBatchRequest = a})

instance AWSRequest UpdateEndpointsBatch where
        type Rs UpdateEndpointsBatch =
             UpdateEndpointsBatchResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEndpointsBatchResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateEndpointsBatch where

instance NFData UpdateEndpointsBatch where

instance ToHeaders UpdateEndpointsBatch where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEndpointsBatch where
        toJSON UpdateEndpointsBatch'{..}
          = object
              (catMaybes
                 [Just
                    ("EndpointBatchRequest" .=
                       _uebEndpointBatchRequest)])

instance ToPath UpdateEndpointsBatch where
        toPath UpdateEndpointsBatch'{..}
          = mconcat
              ["/v1/apps/", toBS _uebApplicationId, "/endpoints"]

instance ToQuery UpdateEndpointsBatch where
        toQuery = const mempty

-- | /See:/ 'updateEndpointsBatchResponse' smart constructor.
data UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse'
  { _uebrsResponseStatus :: !Int
  , _uebrsMessageBody    :: !MessageBody
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointsBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uebrsResponseStatus' - -- | The response status code.
--
-- * 'uebrsMessageBody' - Undocumented member.
updateEndpointsBatchResponse
    :: Int -- ^ 'uebrsResponseStatus'
    -> MessageBody -- ^ 'uebrsMessageBody'
    -> UpdateEndpointsBatchResponse
updateEndpointsBatchResponse pResponseStatus_ pMessageBody_ =
  UpdateEndpointsBatchResponse'
    {_uebrsResponseStatus = pResponseStatus_, _uebrsMessageBody = pMessageBody_}


-- | -- | The response status code.
uebrsResponseStatus :: Lens' UpdateEndpointsBatchResponse Int
uebrsResponseStatus = lens _uebrsResponseStatus (\ s a -> s{_uebrsResponseStatus = a})

-- | Undocumented member.
uebrsMessageBody :: Lens' UpdateEndpointsBatchResponse MessageBody
uebrsMessageBody = lens _uebrsMessageBody (\ s a -> s{_uebrsMessageBody = a})

instance NFData UpdateEndpointsBatchResponse where
