{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a multiplex.
module Network.AWS.MediaLive.DescribeMultiplex
  ( -- * Creating a request
    DescribeMultiplex (..),
    mkDescribeMultiplex,

    -- ** Request lenses
    dmMultiplexId,

    -- * Destructuring the response
    DescribeMultiplexResponse (..),
    mkDescribeMultiplexResponse,

    -- ** Response lenses
    dmrrsArn,
    dmrrsAvailabilityZones,
    dmrrsDestinations,
    dmrrsId,
    dmrrsMultiplexSettings,
    dmrrsName,
    dmrrsPipelinesRunningCount,
    dmrrsProgramCount,
    dmrrsState,
    dmrrsTags,
    dmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'mkDescribeMultiplex' smart constructor.
newtype DescribeMultiplex = DescribeMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplex' value with any optional fields omitted.
mkDescribeMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  DescribeMultiplex
mkDescribeMultiplex multiplexId = DescribeMultiplex' {multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmMultiplexId :: Lens.Lens' DescribeMultiplex Core.Text
dmMultiplexId = Lens.field @"multiplexId"
{-# DEPRECATED dmMultiplexId "Use generic-lens or generic-optics with 'multiplexId' instead." #-}

instance Core.AWSRequest DescribeMultiplex where
  type Rs DescribeMultiplex = DescribeMultiplexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
          DescribeMultiplexResponse'
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

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'mkDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
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

-- | Creates a 'DescribeMultiplexResponse' value with any optional fields omitted.
mkDescribeMultiplexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMultiplexResponse
mkDescribeMultiplexResponse responseStatus =
  DescribeMultiplexResponse'
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
dmrrsArn :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsArn = Lens.field @"arn"
{-# DEPRECATED dmrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAvailabilityZones :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [Core.Text])
dmrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dmrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsDestinations :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
dmrrsDestinations = Lens.field @"destinations"
{-# DEPRECATED dmrrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsId :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsId = Lens.field @"id"
{-# DEPRECATED dmrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsMultiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Types.MultiplexSettings)
dmrrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# DEPRECATED dmrrsMultiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead." #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsName :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsName = Lens.field @"name"
{-# DEPRECATED dmrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsPipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
dmrrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# DEPRECATED dmrrsPipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead." #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsProgramCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
dmrrsProgramCount = Lens.field @"programCount"
{-# DEPRECATED dmrrsProgramCount "Use generic-lens or generic-optics with 'programCount' instead." #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsState :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Types.MultiplexState)
dmrrsState = Lens.field @"state"
{-# DEPRECATED dmrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsTags :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dmrrsTags = Lens.field @"tags"
{-# DEPRECATED dmrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DescribeMultiplexResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
