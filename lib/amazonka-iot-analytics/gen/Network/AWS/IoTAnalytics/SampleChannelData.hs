{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SampleChannelData (..)
    , mkSampleChannelData
    -- ** Request lenses
    , scdChannelName
    , scdEndTime
    , scdMaxMessages
    , scdStartTime

    -- * Destructuring the response
    , SampleChannelDataResponse (..)
    , mkSampleChannelDataResponse
    -- ** Response lenses
    , scdrrsPayloads
    , scdrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { channelName :: Types.ChannelName
    -- ^ The name of the channel whose message samples are retrieved.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end of the time window from which sample messages are retrieved.
  , maxMessages :: Core.Maybe Core.Natural
    -- ^ The number of sample messages to be retrieved. The limit is 10. The default is also 10.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start of the time window from which sample messages are retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SampleChannelData' value with any optional fields omitted.
mkSampleChannelData
    :: Types.ChannelName -- ^ 'channelName'
    -> SampleChannelData
mkSampleChannelData channelName
  = SampleChannelData'{channelName, endTime = Core.Nothing,
                       maxMessages = Core.Nothing, startTime = Core.Nothing}

-- | The name of the channel whose message samples are retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdChannelName :: Lens.Lens' SampleChannelData Types.ChannelName
scdChannelName = Lens.field @"channelName"
{-# INLINEABLE scdChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | The end of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdEndTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.NominalDiffTime)
scdEndTime = Lens.field @"endTime"
{-# INLINEABLE scdEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The number of sample messages to be retrieved. The limit is 10. The default is also 10.
--
-- /Note:/ Consider using 'maxMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdMaxMessages :: Lens.Lens' SampleChannelData (Core.Maybe Core.Natural)
scdMaxMessages = Lens.field @"maxMessages"
{-# INLINEABLE scdMaxMessages #-}
{-# DEPRECATED maxMessages "Use generic-lens or generic-optics with 'maxMessages' instead"  #-}

-- | The start of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdStartTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.NominalDiffTime)
scdStartTime = Lens.field @"startTime"
{-# INLINEABLE scdStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery SampleChannelData where
        toQuery SampleChannelData{..}
          = Core.maybe Core.mempty (Core.toQueryPair "endTime") endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxMessages") maxMessages
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "startTime") startTime

instance Core.ToHeaders SampleChannelData where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SampleChannelData where
        type Rs SampleChannelData = SampleChannelDataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/channels/" Core.<> Core.toText channelName Core.<> "/sample",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SampleChannelDataResponse' Core.<$>
                   (x Core..:? "payloads") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { payloads :: Core.Maybe (Core.NonEmpty Core.Base64)
    -- ^ The list of message samples. Each sample message is returned as a base64-encoded string.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SampleChannelDataResponse' value with any optional fields omitted.
mkSampleChannelDataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SampleChannelDataResponse
mkSampleChannelDataResponse responseStatus
  = SampleChannelDataResponse'{payloads = Core.Nothing,
                               responseStatus}

-- | The list of message samples. Each sample message is returned as a base64-encoded string.
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrrsPayloads :: Lens.Lens' SampleChannelDataResponse (Core.Maybe (Core.NonEmpty Core.Base64))
scdrrsPayloads = Lens.field @"payloads"
{-# INLINEABLE scdrrsPayloads #-}
{-# DEPRECATED payloads "Use generic-lens or generic-optics with 'payloads' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrrsResponseStatus :: Lens.Lens' SampleChannelDataResponse Core.Int
scdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
