{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a multiplex.
module Network.AWS.MediaLive.UpdateMultiplex
  ( -- * Creating a request
    UpdateMultiplex (..),
    mkUpdateMultiplex,

    -- ** Request lenses
    umMultiplexId,
    umMultiplexSettings,
    umName,

    -- * Destructuring the response
    UpdateMultiplexResponse (..),
    mkUpdateMultiplexResponse,

    -- ** Response lenses
    umrrsMultiplex,
    umrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a multiplex.
--
-- /See:/ 'mkUpdateMultiplex' smart constructor.
data UpdateMultiplex = UpdateMultiplex'
  { -- | ID of the multiplex to update.
    multiplexId :: Core.Text,
    -- | The new settings for a multiplex.
    multiplexSettings :: Core.Maybe Types.MultiplexSettings,
    -- | Name of the multiplex.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplex' value with any optional fields omitted.
mkUpdateMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  UpdateMultiplex
mkUpdateMultiplex multiplexId =
  UpdateMultiplex'
    { multiplexId,
      multiplexSettings = Core.Nothing,
      name = Core.Nothing
    }

-- | ID of the multiplex to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexId :: Lens.Lens' UpdateMultiplex Core.Text
umMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED umMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

-- | The new settings for a multiplex.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexSettings :: Lens.Lens' UpdateMultiplex (Core.Maybe Types.MultiplexSettings)
umMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED umMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | Name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umName :: Lens.Lens' UpdateMultiplex (Core.Maybe Core.Text)
umName = Lens.field @"name"
{-# DEPRECATED umName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateMultiplex where
  toJSON UpdateMultiplex {..} =
    Core.object
      ( Core.catMaybes
          [ ("multiplexSettings" Core..=) Core.<$> multiplexSettings,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateMultiplex where
  type Rs UpdateMultiplex = UpdateMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/prod/multiplexes/" Core.<> (Core.toText multiplexId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMultiplexResponse'
            Core.<$> (x Core..:? "multiplex") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'mkUpdateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { -- | The updated multiplex.
    multiplex :: Core.Maybe Types.Multiplex,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexResponse' value with any optional fields omitted.
mkUpdateMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMultiplexResponse
mkUpdateMultiplexResponse responseStatus =
  UpdateMultiplexResponse'
    { multiplex = Core.Nothing,
      responseStatus
    }

-- | The updated multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrrsMultiplex :: Lens.Lens' UpdateMultiplexResponse (Core.Maybe Types.Multiplex)
umrrsMultiplex = Lens.field @"multiplex"
{-# DEPRECATED umrrsMultiplex "Use generic-lens or generic-optics with 'multiplex' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrrsResponseStatus :: Lens.Lens' UpdateMultiplexResponse Core.Int
umrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
