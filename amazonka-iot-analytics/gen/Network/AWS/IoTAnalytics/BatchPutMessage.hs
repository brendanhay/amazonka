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
-- Module      : Network.AWS.IoTAnalytics.BatchPutMessage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends messages to a channel.
--
--
module Network.AWS.IoTAnalytics.BatchPutMessage
    (
    -- * Creating a Request
      batchPutMessage
    , BatchPutMessage
    -- * Request Lenses
    , bpmChannelName
    , bpmMessages

    -- * Destructuring the Response
    , batchPutMessageResponse
    , BatchPutMessageResponse
    -- * Response Lenses
    , bpmrsBatchPutMessageErrorEntries
    , bpmrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { _bpmChannelName :: !Text
  , _bpmMessages    :: ![Message]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchPutMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmChannelName' - The name of the channel where the messages are sent.
--
-- * 'bpmMessages' - The list of messages to be sent. Each message has format: '{ "messageId": "string", "payload": "string"}'.
batchPutMessage
    :: Text -- ^ 'bpmChannelName'
    -> BatchPutMessage
batchPutMessage pChannelName_ =
  BatchPutMessage' {_bpmChannelName = pChannelName_, _bpmMessages = mempty}


-- | The name of the channel where the messages are sent.
bpmChannelName :: Lens' BatchPutMessage Text
bpmChannelName = lens _bpmChannelName (\ s a -> s{_bpmChannelName = a})

-- | The list of messages to be sent. Each message has format: '{ "messageId": "string", "payload": "string"}'.
bpmMessages :: Lens' BatchPutMessage [Message]
bpmMessages = lens _bpmMessages (\ s a -> s{_bpmMessages = a}) . _Coerce

instance AWSRequest BatchPutMessage where
        type Rs BatchPutMessage = BatchPutMessageResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 BatchPutMessageResponse' <$>
                   (x .?> "batchPutMessageErrorEntries" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchPutMessage where

instance NFData BatchPutMessage where

instance ToHeaders BatchPutMessage where
        toHeaders = const mempty

instance ToJSON BatchPutMessage where
        toJSON BatchPutMessage'{..}
          = object
              (catMaybes
                 [Just ("channelName" .= _bpmChannelName),
                  Just ("messages" .= _bpmMessages)])

instance ToPath BatchPutMessage where
        toPath = const "/messages/batch"

instance ToQuery BatchPutMessage where
        toQuery = const mempty

-- | /See:/ 'batchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { _bpmrsBatchPutMessageErrorEntries :: !(Maybe [BatchPutMessageErrorEntry])
  , _bpmrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchPutMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmrsBatchPutMessageErrorEntries' - A list of any errors encountered when sending the messages to the channel.
--
-- * 'bpmrsResponseStatus' - -- | The response status code.
batchPutMessageResponse
    :: Int -- ^ 'bpmrsResponseStatus'
    -> BatchPutMessageResponse
batchPutMessageResponse pResponseStatus_ =
  BatchPutMessageResponse'
    { _bpmrsBatchPutMessageErrorEntries = Nothing
    , _bpmrsResponseStatus = pResponseStatus_
    }


-- | A list of any errors encountered when sending the messages to the channel.
bpmrsBatchPutMessageErrorEntries :: Lens' BatchPutMessageResponse [BatchPutMessageErrorEntry]
bpmrsBatchPutMessageErrorEntries = lens _bpmrsBatchPutMessageErrorEntries (\ s a -> s{_bpmrsBatchPutMessageErrorEntries = a}) . _Default . _Coerce

-- | -- | The response status code.
bpmrsResponseStatus :: Lens' BatchPutMessageResponse Int
bpmrsResponseStatus = lens _bpmrsResponseStatus (\ s a -> s{_bpmrsResponseStatus = a})

instance NFData BatchPutMessageResponse where
