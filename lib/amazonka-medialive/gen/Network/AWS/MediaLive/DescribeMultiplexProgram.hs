{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeMultiplexProgram (..),
    mkDescribeMultiplexProgram,

    -- ** Request lenses
    dmpfMultiplexId,
    dmpfProgramName,

    -- * Destructuring the response
    DescribeMultiplexProgramResponse (..),
    mkDescribeMultiplexProgramResponse,

    -- ** Response lenses
    dmprfrsChannelId,
    dmprfrsMultiplexProgramSettings,
    dmprfrsPacketIdentifiersMap,
    dmprfrsPipelineDetails,
    dmprfrsProgramName,
    dmprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'mkDescribeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'
  { -- | The ID of the multiplex that the program belongs to.
    multiplexId :: Core.Text,
    -- | The name of the program.
    programName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplexProgram' value with any optional fields omitted.
mkDescribeMultiplexProgram ::
  -- | 'multiplexId'
  Core.Text ->
  -- | 'programName'
  Core.Text ->
  DescribeMultiplexProgram
mkDescribeMultiplexProgram multiplexId programName =
  DescribeMultiplexProgram' {multiplexId, programName}

-- | The ID of the multiplex that the program belongs to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpfMultiplexId :: Lens.Lens' DescribeMultiplexProgram Core.Text
dmpfMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED dmpfMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The name of the program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpfProgramName :: Lens.Lens' DescribeMultiplexProgram Core.Text
dmpfProgramName = Lens.field @"programName"
{-# DEPRECATED dmpfProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Core.AWSRequest DescribeMultiplexProgram where
  type Rs DescribeMultiplexProgram = DescribeMultiplexProgramResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/prod/multiplexes/" Core.<> (Core.toText multiplexId)
                Core.<> ("/programs/")
                Core.<> (Core.toText programName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMultiplexProgramResponse'
            Core.<$> (x Core..:? "channelId")
            Core.<*> (x Core..:? "multiplexProgramSettings")
            Core.<*> (x Core..:? "packetIdentifiersMap")
            Core.<*> (x Core..:? "pipelineDetails")
            Core.<*> (x Core..:? "programName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'mkDescribeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'
  { -- | The MediaLive channel associated with the program.
    channelId :: Core.Maybe Core.Text,
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Core.Maybe Types.MultiplexProgramSettings,
    -- | The packet identifier map for this multiplex program.
    packetIdentifiersMap :: Core.Maybe Types.MultiplexProgramPacketIdentifiersMap,
    -- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
    pipelineDetails :: Core.Maybe [Types.MultiplexProgramPipelineDetail],
    -- | The name of the multiplex program.
    programName :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplexProgramResponse' value with any optional fields omitted.
mkDescribeMultiplexProgramResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMultiplexProgramResponse
mkDescribeMultiplexProgramResponse responseStatus =
  DescribeMultiplexProgramResponse'
    { channelId = Core.Nothing,
      multiplexProgramSettings = Core.Nothing,
      packetIdentifiersMap = Core.Nothing,
      pipelineDetails = Core.Nothing,
      programName = Core.Nothing,
      responseStatus
    }

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsChannelId :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
dmprfrsChannelId = Lens.field @"channelId"
{-# DEPRECATED dmprfrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsMultiplexProgramSettings :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramSettings)
dmprfrsMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# DEPRECATED dmprfrsMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsPacketIdentifiersMap :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramPacketIdentifiersMap)
dmprfrsPacketIdentifiersMap = Lens.field @"packetIdentifiersMap"
{-# DEPRECATED dmprfrsPacketIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead." #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsPipelineDetails :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe [Types.MultiplexProgramPipelineDetail])
dmprfrsPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED dmprfrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsProgramName :: Lens.Lens' DescribeMultiplexProgramResponse (Core.Maybe Core.Text)
dmprfrsProgramName = Lens.field @"programName"
{-# DEPRECATED dmprfrsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprfrsResponseStatus :: Lens.Lens' DescribeMultiplexProgramResponse Core.Int
dmprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
