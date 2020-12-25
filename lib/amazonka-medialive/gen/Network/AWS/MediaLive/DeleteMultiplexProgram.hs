{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a program from a multiplex.
module Network.AWS.MediaLive.DeleteMultiplexProgram
  ( -- * Creating a request
    DeleteMultiplexProgram (..),
    mkDeleteMultiplexProgram,

    -- ** Request lenses
    dmpMultiplexId,
    dmpProgramName,

    -- * Destructuring the response
    DeleteMultiplexProgramResponse (..),
    mkDeleteMultiplexProgramResponse,

    -- ** Response lenses
    dmprrsChannelId,
    dmprrsMultiplexProgramSettings,
    dmprrsPacketIdentifiersMap,
    dmprrsPipelineDetails,
    dmprrsProgramName,
    dmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexProgramRequest
--
-- /See:/ 'mkDeleteMultiplexProgram' smart constructor.
data DeleteMultiplexProgram = DeleteMultiplexProgram'
  { -- | The ID of the multiplex that the program belongs to.
    multiplexId :: Core.Text,
    -- | The multiplex program name.
    programName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMultiplexProgram' value with any optional fields omitted.
mkDeleteMultiplexProgram ::
  -- | 'multiplexId'
  Core.Text ->
  -- | 'programName'
  Core.Text ->
  DeleteMultiplexProgram
mkDeleteMultiplexProgram multiplexId programName =
  DeleteMultiplexProgram' {multiplexId, programName}

-- | The ID of the multiplex that the program belongs to.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpMultiplexId :: Lens.Lens' DeleteMultiplexProgram Core.Text
dmpMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED dmpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The multiplex program name.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpProgramName :: Lens.Lens' DeleteMultiplexProgram Core.Text
dmpProgramName = Lens.field @"programName"
{-# DEPRECATED dmpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Core.AWSRequest DeleteMultiplexProgram where
  type Rs DeleteMultiplexProgram = DeleteMultiplexProgramResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
          DeleteMultiplexProgramResponse'
            Core.<$> (x Core..:? "channelId")
            Core.<*> (x Core..:? "multiplexProgramSettings")
            Core.<*> (x Core..:? "packetIdentifiersMap")
            Core.<*> (x Core..:? "pipelineDetails")
            Core.<*> (x Core..:? "programName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DeleteMultiplexProgramResponse
--
-- /See:/ 'mkDeleteMultiplexProgramResponse' smart constructor.
data DeleteMultiplexProgramResponse = DeleteMultiplexProgramResponse'
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

-- | Creates a 'DeleteMultiplexProgramResponse' value with any optional fields omitted.
mkDeleteMultiplexProgramResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMultiplexProgramResponse
mkDeleteMultiplexProgramResponse responseStatus =
  DeleteMultiplexProgramResponse'
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
dmprrsChannelId :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Core.Text)
dmprrsChannelId = Lens.field @"channelId"
{-# DEPRECATED dmprrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsMultiplexProgramSettings :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramSettings)
dmprrsMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# DEPRECATED dmprrsMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsPacketIdentifiersMap :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Types.MultiplexProgramPacketIdentifiersMap)
dmprrsPacketIdentifiersMap = Lens.field @"packetIdentifiersMap"
{-# DEPRECATED dmprrsPacketIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead." #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsPipelineDetails :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe [Types.MultiplexProgramPipelineDetail])
dmprrsPipelineDetails = Lens.field @"pipelineDetails"
{-# DEPRECATED dmprrsPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsProgramName :: Lens.Lens' DeleteMultiplexProgramResponse (Core.Maybe Core.Text)
dmprrsProgramName = Lens.field @"programName"
{-# DEPRECATED dmprrsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsResponseStatus :: Lens.Lens' DeleteMultiplexProgramResponse Core.Int
dmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
