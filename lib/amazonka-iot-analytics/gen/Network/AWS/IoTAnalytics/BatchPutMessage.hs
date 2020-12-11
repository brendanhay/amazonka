{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.BatchPutMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends messages to a channel.
module Network.AWS.IoTAnalytics.BatchPutMessage
  ( -- * Creating a request
    BatchPutMessage (..),
    mkBatchPutMessage,

    -- ** Request lenses
    bpmChannelName,
    bpmMessages,

    -- * Destructuring the response
    BatchPutMessageResponse (..),
    mkBatchPutMessageResponse,

    -- ** Response lenses
    bpmrsBatchPutMessageErrorEntries,
    bpmrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { channelName :: Lude.Text,
    messages :: [Message]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutMessage' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel where the messages are sent.
-- * 'messages' - The list of messages to be sent. Each message has the format: { "messageId": "string", "payload": "string"}.
--
-- The field names of message payloads (data) that you send to AWS IoT Analytics:
--
--     * Must contain only alphanumeric characters and undescores (_). No other special characters are allowed.
--
--
--     * Must begin with an alphabetic character or single underscore (_).
--
--
--     * Cannot contain hyphens (-).
--
--
--     * In regular expression terms: "^[A-Za-z_]([A-Za-z0-9]*|[A-Za-z0-9][A-Za-z0-9_]*)$".
--
--
--     * Cannot be more than 255 characters.
--
--
--     * Are case insensitive. (Fields named foo and FOO in the same payload are considered duplicates.)
--
--
-- For example, {"temp_01": 29} or {"_temp_01": 29} are valid, but {"temp-01": 29}, {"01_temp": 29} or {"__temp_01": 29} are invalid in message payloads.
mkBatchPutMessage ::
  -- | 'channelName'
  Lude.Text ->
  BatchPutMessage
mkBatchPutMessage pChannelName_ =
  BatchPutMessage'
    { channelName = pChannelName_,
      messages = Lude.mempty
    }

-- | The name of the channel where the messages are sent.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmChannelName :: Lens.Lens' BatchPutMessage Lude.Text
bpmChannelName = Lens.lens (channelName :: BatchPutMessage -> Lude.Text) (\s a -> s {channelName = a} :: BatchPutMessage)
{-# DEPRECATED bpmChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The list of messages to be sent. Each message has the format: { "messageId": "string", "payload": "string"}.
--
-- The field names of message payloads (data) that you send to AWS IoT Analytics:
--
--     * Must contain only alphanumeric characters and undescores (_). No other special characters are allowed.
--
--
--     * Must begin with an alphabetic character or single underscore (_).
--
--
--     * Cannot contain hyphens (-).
--
--
--     * In regular expression terms: "^[A-Za-z_]([A-Za-z0-9]*|[A-Za-z0-9][A-Za-z0-9_]*)$".
--
--
--     * Cannot be more than 255 characters.
--
--
--     * Are case insensitive. (Fields named foo and FOO in the same payload are considered duplicates.)
--
--
-- For example, {"temp_01": 29} or {"_temp_01": 29} are valid, but {"temp-01": 29}, {"01_temp": 29} or {"__temp_01": 29} are invalid in message payloads.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmMessages :: Lens.Lens' BatchPutMessage [Message]
bpmMessages = Lens.lens (messages :: BatchPutMessage -> [Message]) (\s a -> s {messages = a} :: BatchPutMessage)
{-# DEPRECATED bpmMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

instance Lude.AWSRequest BatchPutMessage where
  type Rs BatchPutMessage = BatchPutMessageResponse
  request = Req.postJSON ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchPutMessageResponse'
            Lude.<$> (x Lude..?> "batchPutMessageErrorEntries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchPutMessage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON BatchPutMessage where
  toJSON BatchPutMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("channelName" Lude..= channelName),
            Lude.Just ("messages" Lude..= messages)
          ]
      )

instance Lude.ToPath BatchPutMessage where
  toPath = Lude.const "/messages/batch"

instance Lude.ToQuery BatchPutMessage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { batchPutMessageErrorEntries ::
      Lude.Maybe [BatchPutMessageErrorEntry],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutMessageResponse' with the minimum fields required to make a request.
--
-- * 'batchPutMessageErrorEntries' - A list of any errors encountered when sending the messages to the channel.
-- * 'responseStatus' - The response status code.
mkBatchPutMessageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchPutMessageResponse
mkBatchPutMessageResponse pResponseStatus_ =
  BatchPutMessageResponse'
    { batchPutMessageErrorEntries =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of any errors encountered when sending the messages to the channel.
--
-- /Note:/ Consider using 'batchPutMessageErrorEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmrsBatchPutMessageErrorEntries :: Lens.Lens' BatchPutMessageResponse (Lude.Maybe [BatchPutMessageErrorEntry])
bpmrsBatchPutMessageErrorEntries = Lens.lens (batchPutMessageErrorEntries :: BatchPutMessageResponse -> Lude.Maybe [BatchPutMessageErrorEntry]) (\s a -> s {batchPutMessageErrorEntries = a} :: BatchPutMessageResponse)
{-# DEPRECATED bpmrsBatchPutMessageErrorEntries "Use generic-lens or generic-optics with 'batchPutMessageErrorEntries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmrsResponseStatus :: Lens.Lens' BatchPutMessageResponse Lude.Int
bpmrsResponseStatus = Lens.lens (responseStatus :: BatchPutMessageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchPutMessageResponse)
{-# DEPRECATED bpmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
