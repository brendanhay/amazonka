{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.SampleChannelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sample of messages from the specified channel ingested during the specified timeframe. Up to 10 messages can be retrieved.
module Network.AWS.IoTAnalytics.SampleChannelData
  ( -- * Creating a request
    SampleChannelData (..),
    mkSampleChannelData,

    -- ** Request lenses
    scdChannelName,
    scdEndTime,
    scdMaxMessages,
    scdStartTime,

    -- * Destructuring the response
    SampleChannelDataResponse (..),
    mkSampleChannelDataResponse,

    -- ** Response lenses
    scdrrsPayloads,
    scdrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { -- | The name of the channel whose message samples are retrieved.
    channelName :: Types.ChannelName,
    -- | The end of the time window from which sample messages are retrieved.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The number of sample messages to be retrieved. The limit is 10. The default is also 10.
    maxMessages :: Core.Maybe Core.Natural,
    -- | The start of the time window from which sample messages are retrieved.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SampleChannelData' value with any optional fields omitted.
mkSampleChannelData ::
  -- | 'channelName'
  Types.ChannelName ->
  SampleChannelData
mkSampleChannelData channelName =
  SampleChannelData'
    { channelName,
      endTime = Core.Nothing,
      maxMessages = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The name of the channel whose message samples are retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdChannelName :: Lens.Lens' SampleChannelData Types.ChannelName
scdChannelName = Lens.field @"channelName"
{-# DEPRECATED scdChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The end of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdEndTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.NominalDiffTime)
scdEndTime = Lens.field @"endTime"
{-# DEPRECATED scdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The number of sample messages to be retrieved. The limit is 10. The default is also 10.
--
-- /Note:/ Consider using 'maxMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdMaxMessages :: Lens.Lens' SampleChannelData (Core.Maybe Core.Natural)
scdMaxMessages = Lens.field @"maxMessages"
{-# DEPRECATED scdMaxMessages "Use generic-lens or generic-optics with 'maxMessages' instead." #-}

-- | The start of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdStartTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.NominalDiffTime)
scdStartTime = Lens.field @"startTime"
{-# DEPRECATED scdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.AWSRequest SampleChannelData where
  type Rs SampleChannelData = SampleChannelDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/channels/" Core.<> (Core.toText channelName)
                Core.<> ("/sample")
            ),
        Core._rqQuery =
          Core.toQueryValue "endTime" Core.<$> endTime
            Core.<> (Core.toQueryValue "maxMessages" Core.<$> maxMessages)
            Core.<> (Core.toQueryValue "startTime" Core.<$> startTime),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SampleChannelDataResponse'
            Core.<$> (x Core..:? "payloads") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { -- | The list of message samples. Each sample message is returned as a base64-encoded string.
    payloads :: Core.Maybe (Core.NonEmpty Core.Base64),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SampleChannelDataResponse' value with any optional fields omitted.
mkSampleChannelDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SampleChannelDataResponse
mkSampleChannelDataResponse responseStatus =
  SampleChannelDataResponse'
    { payloads = Core.Nothing,
      responseStatus
    }

-- | The list of message samples. Each sample message is returned as a base64-encoded string.
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrrsPayloads :: Lens.Lens' SampleChannelDataResponse (Core.Maybe (Core.NonEmpty Core.Base64))
scdrrsPayloads = Lens.field @"payloads"
{-# DEPRECATED scdrrsPayloads "Use generic-lens or generic-optics with 'payloads' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrrsResponseStatus :: Lens.Lens' SampleChannelDataResponse Core.Int
scdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
