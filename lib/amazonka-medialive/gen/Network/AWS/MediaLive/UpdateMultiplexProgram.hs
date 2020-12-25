{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a program in a multiplex.
module Network.AWS.MediaLive.UpdateMultiplexProgram
  ( -- * Creating a request
    UpdateMultiplexProgram (..),
    mkUpdateMultiplexProgram,

    -- ** Request lenses
    umpMultiplexId,
    umpProgramName,
    umpMultiplexProgramSettings,

    -- * Destructuring the response
    UpdateMultiplexProgramResponse (..),
    mkUpdateMultiplexProgramResponse,

    -- ** Response lenses
    umprrsMultiplexProgram,
    umprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a program in a multiplex.
--
-- /See:/ 'mkUpdateMultiplexProgram' smart constructor.
data UpdateMultiplexProgram = UpdateMultiplexProgram'
  { -- | The ID of the multiplex of the program to update.
    multiplexId :: Core.Text,
    -- | The name of the program to update.
    programName :: Core.Text,
    -- | The new settings for a multiplex program.
    multiplexProgramSettings :: Core.Maybe Types.MultiplexProgramSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexProgram' value with any optional fields omitted.
mkUpdateMultiplexProgram ::
  -- | 'multiplexId'
  Core.Text ->
  -- | 'programName'
  Core.Text ->
  UpdateMultiplexProgram
mkUpdateMultiplexProgram multiplexId programName =
  UpdateMultiplexProgram'
    { multiplexId,
      programName,
      multiplexProgramSettings = Core.Nothing
    }

-- | The ID of the multiplex of the program to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexId :: Lens.Lens' UpdateMultiplexProgram Core.Text
umpMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED umpMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The name of the program to update.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpProgramName :: Lens.Lens' UpdateMultiplexProgram Core.Text
umpProgramName = Lens.field @"programName"
{-# DEPRECATED umpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The new settings for a multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umpMultiplexProgramSettings :: Lens.Lens' UpdateMultiplexProgram (Core.Maybe Types.MultiplexProgramSettings)
umpMultiplexProgramSettings = Lens.field @"multiplexProgramSettings"
{-# DEPRECATED umpMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

instance Core.FromJSON UpdateMultiplexProgram where
  toJSON UpdateMultiplexProgram {..} =
    Core.object
      ( Core.catMaybes
          [ ("multiplexProgramSettings" Core..=)
              Core.<$> multiplexProgramSettings
          ]
      )

instance Core.AWSRequest UpdateMultiplexProgram where
  type Rs UpdateMultiplexProgram = UpdateMultiplexProgramResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/prod/multiplexes/" Core.<> (Core.toText multiplexId)
                Core.<> ("/programs/")
                Core.<> (Core.toText programName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMultiplexProgramResponse'
            Core.<$> (x Core..:? "multiplexProgram")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateMultiplexProgramResponse
--
-- /See:/ 'mkUpdateMultiplexProgramResponse' smart constructor.
data UpdateMultiplexProgramResponse = UpdateMultiplexProgramResponse'
  { -- | The updated multiplex program.
    multiplexProgram :: Core.Maybe Types.MultiplexProgram,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexProgramResponse' value with any optional fields omitted.
mkUpdateMultiplexProgramResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMultiplexProgramResponse
mkUpdateMultiplexProgramResponse responseStatus =
  UpdateMultiplexProgramResponse'
    { multiplexProgram = Core.Nothing,
      responseStatus
    }

-- | The updated multiplex program.
--
-- /Note:/ Consider using 'multiplexProgram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprrsMultiplexProgram :: Lens.Lens' UpdateMultiplexProgramResponse (Core.Maybe Types.MultiplexProgram)
umprrsMultiplexProgram = Lens.field @"multiplexProgram"
{-# DEPRECATED umprrsMultiplexProgram "Use generic-lens or generic-optics with 'multiplexProgram' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umprrsResponseStatus :: Lens.Lens' UpdateMultiplexProgramResponse Core.Int
umprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
