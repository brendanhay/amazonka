{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new program in the multiplex.
module Network.AWS.MediaLive.CreateMultiplexProgram
  ( -- * Creating a request
    CreateMultiplexProgram (..),
    mkCreateMultiplexProgram,

    -- ** Request lenses
    cmpMultiplexId,
    cmpRequestId,
    cmpMultiplexProgramSettings,
    cmpProgramName,

    -- * Destructuring the response
    CreateMultiplexProgramResponse (..),
    mkCreateMultiplexProgramResponse,

    -- ** Response lenses
    cmprrsMultiplexProgram,
    cmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a program in a multiplex.
--
-- /See:/ 'mkCreateMultiplexProgram' smart constructor.
data CreateMultiplexProgram = CreateMultiplexProgram'
  { -- | ID of the multiplex where the program is to be created.
    multiplexId :: Core.Text,
    -- | Unique request ID. This prevents retries from creating multiple
    --
    -- resources.
    requestId :: Core.Text,
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Types.MultiplexProgramSettings,
    -- | Name of multiplex program.
    programName :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexProgram' value with any optional fields omitted.
mkCreateMultiplexProgram ::
  -- | 'multiplexId'
  Core.Text ->
  -- | 'requestId'
  Core.Text ->
  -- | 'multiplexProgramSettings'
  Types.MultiplexProgramSettings ->
  -- | 'programName'
  Core.Text ->
  CreateMultiplexProgram
mkCreateMultiplexProgram
  multiplexId
  requestId
  multiplexProgramSettings
  programName =
    CreateMultiplexProgram'
      { multiplexId,
        requestId,
        multiplexProgramSettings,
        programName
      }

-- | ID of the multiplex where the program is to be created.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexId :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED cmpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpRequestId :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpRequestId = Lens.field @"requestId"
{-# DEPRECATED cmpRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpMultiplexProgramSettings :: Lens.Lens' CreateMultiplexProgram Types.MultiplexProgramSettings
cmpMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# DEPRECATED cmpMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

-- | Name of multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpProgramName :: Lens.Lens' CreateMultiplexProgram Core.Text
cmpProgramName = Lens.field @"programName"
{-# DEPRECATED cmpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

instance Core.FromJSON CreateMultiplexProgram where
  toJSON CreateMultiplexProgram {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("requestId" Core..= requestId),
            Core.Just
              ("multiplexProgramSettings" Core..= multiplexProgramSettings),
            Core.Just ("programName" Core..= programName)
          ]
      )

instance Core.AWSRequest CreateMultiplexProgram where
  type Rs CreateMultiplexProgram = CreateMultiplexProgramResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/multiplexes/" Core.<> (Core.toText multiplexId)
                Core.<> ("/programs")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultiplexProgramResponse'
            Core.<$> (x Core..:? "multiplexProgram")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for CreateMultiplexProgramResponse
--
-- /See:/ 'mkCreateMultiplexProgramResponse' smart constructor.
data CreateMultiplexProgramResponse = CreateMultiplexProgramResponse'
  { -- | The newly created multiplex program.
    multiplexProgram :: Core.Maybe Types.MultiplexProgram,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexProgramResponse' value with any optional fields omitted.
mkCreateMultiplexProgramResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMultiplexProgramResponse
mkCreateMultiplexProgramResponse responseStatus =
  CreateMultiplexProgramResponse'
    { multiplexProgram = Core.Nothing,
      responseStatus
    }

-- | The newly created multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsMultiplexProgram :: Lens.Lens' CreateMultiplexProgramResponse (Core.Maybe Types.MultiplexProgram)
cmprrsMultiplexProgram = Lens.field @"multiplexProgram"
{-# DEPRECATED cmprrsMultiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmprrsResponseStatus :: Lens.Lens' CreateMultiplexProgramResponse Core.Int
cmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
