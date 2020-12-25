{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    bpmrrsBatchPutMessageErrorEntries,
    bpmrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { -- | The name of the channel where the messages are sent.
    channelName :: Types.ChannelName,
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
    messages :: [Types.Message]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutMessage' value with any optional fields omitted.
mkBatchPutMessage ::
  -- | 'channelName'
  Types.ChannelName ->
  BatchPutMessage
mkBatchPutMessage channelName =
  BatchPutMessage' {channelName, messages = Core.mempty}

-- | The name of the channel where the messages are sent.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmChannelName :: Lens.Lens' BatchPutMessage Types.ChannelName
bpmChannelName = Lens.field @"channelName"
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
bpmMessages :: Lens.Lens' BatchPutMessage [Types.Message]
bpmMessages = Lens.field @"messages"
{-# DEPRECATED bpmMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

instance Core.FromJSON BatchPutMessage where
  toJSON BatchPutMessage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelName" Core..= channelName),
            Core.Just ("messages" Core..= messages)
          ]
      )

instance Core.AWSRequest BatchPutMessage where
  type Rs BatchPutMessage = BatchPutMessageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/messages/batch",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutMessageResponse'
            Core.<$> (x Core..:? "batchPutMessageErrorEntries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { -- | A list of any errors encountered when sending the messages to the channel.
    batchPutMessageErrorEntries :: Core.Maybe [Types.BatchPutMessageErrorEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchPutMessageResponse' value with any optional fields omitted.
mkBatchPutMessageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchPutMessageResponse
mkBatchPutMessageResponse responseStatus =
  BatchPutMessageResponse'
    { batchPutMessageErrorEntries =
        Core.Nothing,
      responseStatus
    }

-- | A list of any errors encountered when sending the messages to the channel.
--
-- /Note:/ Consider using 'batchPutMessageErrorEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmrrsBatchPutMessageErrorEntries :: Lens.Lens' BatchPutMessageResponse (Core.Maybe [Types.BatchPutMessageErrorEntry])
bpmrrsBatchPutMessageErrorEntries = Lens.field @"batchPutMessageErrorEntries"
{-# DEPRECATED bpmrrsBatchPutMessageErrorEntries "Use generic-lens or generic-optics with 'batchPutMessageErrorEntries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmrrsResponseStatus :: Lens.Lens' BatchPutMessageResponse Core.Int
bpmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bpmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
