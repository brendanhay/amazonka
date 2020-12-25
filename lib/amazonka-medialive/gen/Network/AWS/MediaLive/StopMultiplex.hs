{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StopMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running multiplex. If the multiplex isn't running, this action has no effect.
module Network.AWS.MediaLive.StopMultiplex
  ( -- * Creating a request
    StopMultiplex (..),
    mkStopMultiplex,

    -- ** Request lenses
    smMultiplexId,

    -- * Destructuring the response
    StopMultiplexResponse (..),
    mkStopMultiplexResponse,

    -- ** Response lenses
    smrrsArn,
    smrrsAvailabilityZones,
    smrrsDestinations,
    smrrsId,
    smrrsMultiplexSettings,
    smrrsName,
    smrrsPipelinesRunningCount,
    smrrsProgramCount,
    smrrsState,
    smrrsTags,
    smrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StopMultiplexRequest
--
-- /See:/ 'mkStopMultiplex' smart constructor.
newtype StopMultiplex = StopMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopMultiplex' value with any optional fields omitted.
mkStopMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  StopMultiplex
mkStopMultiplex multiplexId = StopMultiplex' {multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smMultiplexId :: Lens.Lens' StopMultiplex Core.Text
smMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED smMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Core.FromJSON StopMultiplex where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StopMultiplex where
  type Rs StopMultiplex = StopMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/multiplexes/" Core.<> (Core.toText multiplexId)
                Core.<> ("/stop")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopMultiplexResponse'
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

-- | Placeholder documentation for StopMultiplexResponse
--
-- /See:/ 'mkStopMultiplexResponse' smart constructor.
data StopMultiplexResponse = StopMultiplexResponse'
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

-- | Creates a 'StopMultiplexResponse' value with any optional fields omitted.
mkStopMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopMultiplexResponse
mkStopMultiplexResponse responseStatus =
  StopMultiplexResponse'
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
smrrsArn :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
smrrsArn = Lens.field @"arn"
{-# DEPRECATED smrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsAvailabilityZones :: Lens.Lens' StopMultiplexResponse (Core.Maybe [Core.Text])
smrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED smrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsDestinations :: Lens.Lens' StopMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
smrrsDestinations = Lens.field @"destinations"
{-# DEPRECATED smrrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsId :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
smrrsId = Lens.field @"id"
{-# DEPRECATED smrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsMultiplexSettings :: Lens.Lens' StopMultiplexResponse (Core.Maybe Types.MultiplexSettings)
smrrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED smrrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsName :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
smrrsName = Lens.field @"name"
{-# DEPRECATED smrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsPipelinesRunningCount :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Int)
smrrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED smrrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsProgramCount :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Int)
smrrsProgramCount = Lens.field @"programCount"
{-# DEPRECATED smrrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsState :: Lens.Lens' StopMultiplexResponse (Core.Maybe Types.MultiplexState)
smrrsState = Lens.field @"state"
{-# DEPRECATED smrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsTags :: Lens.Lens' StopMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
smrrsTags = Lens.field @"tags"
{-# DEPRECATED smrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrrsResponseStatus :: Lens.Lens' StopMultiplexResponse Core.Int
smrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
