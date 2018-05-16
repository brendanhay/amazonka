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
-- Module      : Network.AWS.Pinpoint.SendMessages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send a batch of messages
module Network.AWS.Pinpoint.SendMessages
    (
    -- * Creating a Request
      sendMessages
    , SendMessages
    -- * Request Lenses
    , smApplicationId
    , smMessageRequest

    -- * Destructuring the Response
    , sendMessagesResponse
    , SendMessagesResponse
    -- * Response Lenses
    , smrsResponseStatus
    , smrsMessageResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendMessages' smart constructor.
data SendMessages = SendMessages'
  { _smApplicationId  :: !Text
  , _smMessageRequest :: !MessageRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smApplicationId' - Undocumented member.
--
-- * 'smMessageRequest' - Undocumented member.
sendMessages
    :: Text -- ^ 'smApplicationId'
    -> MessageRequest -- ^ 'smMessageRequest'
    -> SendMessages
sendMessages pApplicationId_ pMessageRequest_ =
  SendMessages'
    {_smApplicationId = pApplicationId_, _smMessageRequest = pMessageRequest_}


-- | Undocumented member.
smApplicationId :: Lens' SendMessages Text
smApplicationId = lens _smApplicationId (\ s a -> s{_smApplicationId = a})

-- | Undocumented member.
smMessageRequest :: Lens' SendMessages MessageRequest
smMessageRequest = lens _smMessageRequest (\ s a -> s{_smMessageRequest = a})

instance AWSRequest SendMessages where
        type Rs SendMessages = SendMessagesResponse
        request = postJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 SendMessagesResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable SendMessages where

instance NFData SendMessages where

instance ToHeaders SendMessages where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendMessages where
        toJSON SendMessages'{..}
          = object
              (catMaybes
                 [Just ("MessageRequest" .= _smMessageRequest)])

instance ToPath SendMessages where
        toPath SendMessages'{..}
          = mconcat
              ["/v1/apps/", toBS _smApplicationId, "/messages"]

instance ToQuery SendMessages where
        toQuery = const mempty

-- | /See:/ 'sendMessagesResponse' smart constructor.
data SendMessagesResponse = SendMessagesResponse'
  { _smrsResponseStatus  :: !Int
  , _smrsMessageResponse :: !MessageResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsResponseStatus' - -- | The response status code.
--
-- * 'smrsMessageResponse' - Undocumented member.
sendMessagesResponse
    :: Int -- ^ 'smrsResponseStatus'
    -> MessageResponse -- ^ 'smrsMessageResponse'
    -> SendMessagesResponse
sendMessagesResponse pResponseStatus_ pMessageResponse_ =
  SendMessagesResponse'
    { _smrsResponseStatus = pResponseStatus_
    , _smrsMessageResponse = pMessageResponse_
    }


-- | -- | The response status code.
smrsResponseStatus :: Lens' SendMessagesResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\ s a -> s{_smrsResponseStatus = a})

-- | Undocumented member.
smrsMessageResponse :: Lens' SendMessagesResponse MessageResponse
smrsMessageResponse = lens _smrsMessageResponse (\ s a -> s{_smrsMessageResponse = a})

instance NFData SendMessagesResponse where
