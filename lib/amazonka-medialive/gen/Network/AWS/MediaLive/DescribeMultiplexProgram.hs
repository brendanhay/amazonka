{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a program in a multiplex.
module Network.AWS.MediaLive.DescribeMultiplexProgram
    (
    -- * Creating a request
      DescribeMultiplexProgram (..)
    , mkDescribeMultiplexProgram
    -- ** Request lenses
    , dmpfMultiplexId
    , dmpfProgramName

    -- * Destructuring the response
    , DescribeMultiplexProgramResponse (..)
    , mkDescribeMultiplexProgramResponse
    -- ** Response lenses
    , dmprfrsChannelId
    , dmprfrsMultiplexProgramSettings
    , dmprfrsPacketIdentifiersMap
    , dmprfrsPipelineDetails
    , dmprfrsProgramName
    , dmprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'mkDescribeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex that the program belongs to.
  , programName :: Core.Text
    -- ^ The name of the program.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplexProgram' value with any optional fields omitted.
mkDescribeMultiplexProgram
    :: Core.Text -- ^ 'multiplexId'
    -> Core.Text -- ^ 'programName'
    -> DescribeMultiplexProgram
mkDescribeMultiplexProgram multiplexId programName
  = DescribeMultiplexProgram'{multiplexId, programName}

-- | The ID of the multiplex that the program belongs to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpfMultiplexId :: Lens.Lens' DescribeMultiplexProgram Core.Text
dmpfMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE dmpfMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

-- | The name of the program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpfProgramName :: Lens.Lens' DescribeMultiplexProgram Core.Text
dmpfProgramName = Lens.field @"programName"
{-# INLINEABLE dmpfProgramName #-}
{-# DEPRECATED programName "Use generic-lens or generic-optics with 'programName' instead"  #-}

instance Core.ToQuery DescribeMultiplexProgram where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMultiplexProgram where
        toHeaders DescribeMultiplexProgram{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeMultiplexProgram where
        type Rs DescribeMultiplexProgram = DescribeMultiplexProgramResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId Core.<>
                             "/programs/"
                             Core.<> Core.toText programName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMultiplexProgramResponse' Core.<$>
                   (x Core..:? "channelId") Core.<*>
                     x Core..:? "multiplexProgramSettings"
                     Core.<*> x Core..:? "packetIdentifiersMap"
                     Core.<*> x Core..:? "pipelineDetails"
                     Core.<*> x Core..:? "programName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'mkDescribeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'
  { channelId :: Core.Maybe Core.Text
    -- ^ The MediaLive channel associated with the program.
  , multiplexProgramSettings :: Core.Maybe Types.MultiplexProgramSettings
    -- ^ The settings for this multiplex program.
  , packetIdentifiersMap :: Core.Maybe Types.MultiplexProgramPacketIdentifiersMap
    -- ^ The packet identifier map for this multiplex program.
  , pipelineDetails :: Core.Maybe [Types.MultiplexProgramPipelineDetail]
    -- ^ Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
  , programName :: Core.Maybe Core.Text
    -- ^ The name of the multiplex program.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplexProgramResponse' value with any optional fields omitted.
mkDescribeMultiplexProgramResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMultiplexProgramResponse
mkDescribeMultiplexProgramResponse responseStatus
  = DescribeMultiplexProgramResponse'{channelId = Core.Nothing,
                                      multiplexProgramSettings = Core.Nothing,
                                      packetIdentifiersMap = Core.Nothing,
                                      pipelineDetails = Core.Nothing, programName = Core.Nothing,
                                      responseStatus}

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsChannelId :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
dmprfrsChannelId = Lens.field @"channelId"
{-# INLINEABLE dmprfrsChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsMultiplexProgramSettings :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramSettings)
dmprfrsMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# INLINEABLE dmprfrsMultiplexProgramSettings #-}
{-# DEPRECATED multiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead"  #-}

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsPacketIdentifiersMap :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramPacketIdentifiersMap)
dmprfrsPacketIdentifiersMap = Lens.field @"packetIdentifiersMap"
{-# INLINEABLE dmprfrsPacketIdentifiersMap #-}
{-# DEPRECATED packetIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead"  #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsPipelineDetails :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe [Types.MultiplexProgramPipelineDetail])
dmprfrsPipelineDetails = Lens.field @"pipelineDetails"
{-# INLINEABLE dmprfrsPipelineDetails #-}
{-# DEPRECATED pipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead"  #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsProgramName :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
dmprfrsProgramName = Lens.field @"programName"
{-# INLINEABLE dmprfrsProgramName #-}
{-# DEPRECATED programName "Use generic-lens or generic-optics with 'programName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsResponseStatus :: Lens.Lens' DescribeMultiplexProgramResponse Core.Int
dmprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
