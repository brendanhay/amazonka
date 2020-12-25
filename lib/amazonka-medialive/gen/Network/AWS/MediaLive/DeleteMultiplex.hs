{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a multiplex. The multiplex must be idle.
module Network.AWS.MediaLive.DeleteMultiplex
  ( -- * Creating a request
    DeleteMultiplex (..),
    mkDeleteMultiplex,

    -- ** Request lenses
    dMultiplexId,

    -- * Destructuring the response
    DeleteMultiplexResponse (..),
    mkDeleteMultiplexResponse,

    -- ** Response lenses
    dmrfrsArn,
    dmrfrsAvailabilityZones,
    dmrfrsDestinations,
    dmrfrsId,
    dmrfrsMultiplexSettings,
    dmrfrsName,
    dmrfrsPipelinesRunningCount,
    dmrfrsProgramCount,
    dmrfrsState,
    dmrfrsTags,
    dmrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'mkDeleteMultiplex' smart constructor.
newtype DeleteMultiplex = DeleteMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMultiplex' value with any optional fields omitted.
mkDeleteMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  DeleteMultiplex
mkDeleteMultiplex multiplexId = DeleteMultiplex' {multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMultiplexId :: Lens.Lens' DeleteMultiplex Core.Text
dMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED dMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Core.AWSRequest DeleteMultiplex where
  type Rs DeleteMultiplex = DeleteMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/prod/multiplexes/" Core.<> (Core.toText multiplexId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMultiplexResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "availabilityZones")
            Core.<*> (x Core..:? "destinations")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "multiplexSettings")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "pipelinesRunningCount")
            Core.<*> (x Core..:? "programCount")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'mkDeleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
  { -- | The unique arn of the multiplex.
    arn :: Core.Maybe Core.Text,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | A list of the multiplex output destinations.
    destinations :: Core.Maybe [Types.MultiplexOutputDestination],
    -- | The unique id of the multiplex.
    id :: Core.Maybe Core.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Core.Maybe Types.MultiplexSettings,
    -- | The name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The number of programs in the multiplex.
    programCount :: Core.Maybe Core.Int,
    -- | The current state of the multiplex.
    state :: Core.Maybe Types.MultiplexState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMultiplexResponse' value with any optional fields omitted.
mkDeleteMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMultiplexResponse
mkDeleteMultiplexResponse responseStatus =
  DeleteMultiplexResponse'
    { arn = Core.Nothing,
      availabilityZones = Core.Nothing,
      destinations = Core.Nothing,
      id = Core.Nothing,
      multiplexSettings = Core.Nothing,
      name = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      programCount = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsArn :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsArn = Lens.field @"arn"
{-# DEPRECATED dmrfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsAvailabilityZones :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [Core.Text])
dmrfrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dmrfrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsDestinations :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
dmrfrsDestinations = Lens.field @"destinations"
{-# DEPRECATED dmrfrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsId :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsId = Lens.field @"id"
{-# DEPRECATED dmrfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsMultiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Types.MultiplexSettings)
dmrfrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED dmrfrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsName :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsName = Lens.field @"name"
{-# DEPRECATED dmrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsPipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
dmrfrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED dmrfrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsProgramCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
dmrfrsProgramCount = Lens.field @"programCount"
{-# DEPRECATED dmrfrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsState :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Types.MultiplexState)
dmrfrsState = Lens.field @"state"
{-# DEPRECATED dmrfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsTags :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dmrfrsTags = Lens.field @"tags"
{-# DEPRECATED dmrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsResponseStatus :: Lens.Lens' DeleteMultiplexResponse Core.Int
dmrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
