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
-- Module      : Network.AWS.Pinpoint.SendUsersMessages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send a batch of messages to users
module Network.AWS.Pinpoint.SendUsersMessages
    (
    -- * Creating a Request
      sendUsersMessages
    , SendUsersMessages
    -- * Request Lenses
    , sumsApplicationId
    , sumsSendUsersMessageRequest

    -- * Destructuring the Response
    , sendUsersMessagesResponse
    , SendUsersMessagesResponse
    -- * Response Lenses
    , sumrsResponseStatus
    , sumrsSendUsersMessageResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendUsersMessages' smart constructor.
data SendUsersMessages = SendUsersMessages'
  { _sumsApplicationId           :: !Text
  , _sumsSendUsersMessageRequest :: !SendUsersMessageRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendUsersMessages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumsApplicationId' - Undocumented member.
--
-- * 'sumsSendUsersMessageRequest' - Undocumented member.
sendUsersMessages
    :: Text -- ^ 'sumsApplicationId'
    -> SendUsersMessageRequest -- ^ 'sumsSendUsersMessageRequest'
    -> SendUsersMessages
sendUsersMessages pApplicationId_ pSendUsersMessageRequest_ =
  SendUsersMessages'
    { _sumsApplicationId = pApplicationId_
    , _sumsSendUsersMessageRequest = pSendUsersMessageRequest_
    }


-- | Undocumented member.
sumsApplicationId :: Lens' SendUsersMessages Text
sumsApplicationId = lens _sumsApplicationId (\ s a -> s{_sumsApplicationId = a})

-- | Undocumented member.
sumsSendUsersMessageRequest :: Lens' SendUsersMessages SendUsersMessageRequest
sumsSendUsersMessageRequest = lens _sumsSendUsersMessageRequest (\ s a -> s{_sumsSendUsersMessageRequest = a})

instance AWSRequest SendUsersMessages where
        type Rs SendUsersMessages = SendUsersMessagesResponse
        request = postJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 SendUsersMessagesResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable SendUsersMessages where

instance NFData SendUsersMessages where

instance ToHeaders SendUsersMessages where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendUsersMessages where
        toJSON SendUsersMessages'{..}
          = object
              (catMaybes
                 [Just
                    ("SendUsersMessageRequest" .=
                       _sumsSendUsersMessageRequest)])

instance ToPath SendUsersMessages where
        toPath SendUsersMessages'{..}
          = mconcat
              ["/v1/apps/", toBS _sumsApplicationId,
               "/users-messages"]

instance ToQuery SendUsersMessages where
        toQuery = const mempty

-- | /See:/ 'sendUsersMessagesResponse' smart constructor.
data SendUsersMessagesResponse = SendUsersMessagesResponse'
  { _sumrsResponseStatus           :: !Int
  , _sumrsSendUsersMessageResponse :: !SendUsersMessageResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendUsersMessagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumrsResponseStatus' - -- | The response status code.
--
-- * 'sumrsSendUsersMessageResponse' - Undocumented member.
sendUsersMessagesResponse
    :: Int -- ^ 'sumrsResponseStatus'
    -> SendUsersMessageResponse -- ^ 'sumrsSendUsersMessageResponse'
    -> SendUsersMessagesResponse
sendUsersMessagesResponse pResponseStatus_ pSendUsersMessageResponse_ =
  SendUsersMessagesResponse'
    { _sumrsResponseStatus = pResponseStatus_
    , _sumrsSendUsersMessageResponse = pSendUsersMessageResponse_
    }


-- | -- | The response status code.
sumrsResponseStatus :: Lens' SendUsersMessagesResponse Int
sumrsResponseStatus = lens _sumrsResponseStatus (\ s a -> s{_sumrsResponseStatus = a})

-- | Undocumented member.
sumrsSendUsersMessageResponse :: Lens' SendUsersMessagesResponse SendUsersMessageResponse
sumrsSendUsersMessageResponse = lens _sumrsSendUsersMessageResponse (\ s a -> s{_sumrsSendUsersMessageResponse = a})

instance NFData SendUsersMessagesResponse where
