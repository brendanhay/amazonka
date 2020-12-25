{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.StartMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start (run) the multiplex. Starting the multiplex does not start the channels. You must explicitly start each channel.
module Network.AWS.MediaLive.StartMultiplex
  ( -- * Creating a request
    StartMultiplex (..),
    mkStartMultiplex,

    -- ** Request lenses
    sMultiplexId,

    -- * Destructuring the response
    StartMultiplexResponse (..),
    mkStartMultiplexResponse,

    -- ** Response lenses
    smrfrsArn,
    smrfrsAvailabilityZones,
    smrfrsDestinations,
    smrfrsId,
    smrfrsMultiplexSettings,
    smrfrsName,
    smrfrsPipelinesRunningCount,
    smrfrsProgramCount,
    smrfrsState,
    smrfrsTags,
    smrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'mkStartMultiplex' smart constructor.
newtype StartMultiplex = StartMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartMultiplex' value with any optional fields omitted.
mkStartMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  StartMultiplex
mkStartMultiplex multiplexId = StartMultiplex' {multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMultiplexId :: Lens.Lens' StartMultiplex Core.Text
sMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED sMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Core.FromJSON StartMultiplex where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StartMultiplex where
  type Rs StartMultiplex = StartMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/prod/multiplexes/" Core.<> (Core.toText multiplexId)
                Core.<> ("/start")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMultiplexResponse'
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

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'mkStartMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
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

-- | Creates a 'StartMultiplexResponse' value with any optional fields omitted.
mkStartMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartMultiplexResponse
mkStartMultiplexResponse responseStatus =
  StartMultiplexResponse'
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
smrfrsArn :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsArn = Lens.field @"arn"
{-# DEPRECATED smrfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsAvailabilityZones :: Lens.Lens' StartMultiplexResponse (Core.Maybe [Core.Text])
smrfrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED smrfrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsDestinations :: Lens.Lens' StartMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
smrfrsDestinations = Lens.field @"destinations"
{-# DEPRECATED smrfrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsId :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsId = Lens.field @"id"
{-# DEPRECATED smrfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsMultiplexSettings :: Lens.Lens' StartMultiplexResponse (Core.Maybe Types.MultiplexSettings)
smrfrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED smrfrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsName :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsName = Lens.field @"name"
{-# DEPRECATED smrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsPipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
smrfrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED smrfrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsProgramCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
smrfrsProgramCount = Lens.field @"programCount"
{-# DEPRECATED smrfrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsState :: Lens.Lens' StartMultiplexResponse (Core.Maybe Types.MultiplexState)
smrfrsState = Lens.field @"state"
{-# DEPRECATED smrfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsTags :: Lens.Lens' StartMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
smrfrsTags = Lens.field @"tags"
{-# DEPRECATED smrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsResponseStatus :: Lens.Lens' StartMultiplexResponse Core.Int
smrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
