{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeMultiplex (..)
    , mkDescribeMultiplex
    -- ** Request lenses
    , dmMultiplexId

    -- * Destructuring the response
    , DescribeMultiplexResponse (..)
    , mkDescribeMultiplexResponse
    -- ** Response lenses
    , dmrrsArn
    , dmrrsAvailabilityZones
    , dmrrsDestinations
    , dmrrsId
    , dmrrsMultiplexSettings
    , dmrrsName
    , dmrrsPipelinesRunningCount
    , dmrrsProgramCount
    , dmrrsState
    , dmrrsTags
    , dmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'mkDescribeMultiplex' smart constructor.
newtype DescribeMultiplex = DescribeMultiplex'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplex' value with any optional fields omitted.
mkDescribeMultiplex
    :: Core.Text -- ^ 'multiplexId'
    -> DescribeMultiplex
mkDescribeMultiplex multiplexId = DescribeMultiplex'{multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmMultiplexId :: Lens.Lens' DescribeMultiplex Core.Text
dmMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE dmMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

instance Core.ToQuery DescribeMultiplex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMultiplex where
        toHeaders DescribeMultiplex{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeMultiplex where
        type Rs DescribeMultiplex = DescribeMultiplexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMultiplexResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "availabilityZones" Core.<*>
                     x Core..:? "destinations"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "multiplexSettings"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "pipelinesRunningCount"
                     Core.<*> x Core..:? "programCount"
                     Core.<*> x Core..:? "state"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'mkDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The unique arn of the multiplex.
  , availabilityZones :: Core.Maybe [Core.Text]
    -- ^ A list of availability zones for the multiplex.
  , destinations :: Core.Maybe [Types.MultiplexOutputDestination]
    -- ^ A list of the multiplex output destinations.
  , id :: Core.Maybe Core.Text
    -- ^ The unique id of the multiplex.
  , multiplexSettings :: Core.Maybe Types.MultiplexSettings
    -- ^ Configuration for a multiplex event.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the multiplex.
  , pipelinesRunningCount :: Core.Maybe Core.Int
    -- ^ The number of currently healthy pipelines.
  , programCount :: Core.Maybe Core.Int
    -- ^ The number of programs in the multiplex.
  , state :: Core.Maybe Types.MultiplexState
    -- ^ The current state of the multiplex.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMultiplexResponse' value with any optional fields omitted.
mkDescribeMultiplexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMultiplexResponse
mkDescribeMultiplexResponse responseStatus
  = DescribeMultiplexResponse'{arn = Core.Nothing,
                               availabilityZones = Core.Nothing, destinations = Core.Nothing,
                               id = Core.Nothing, multiplexSettings = Core.Nothing,
                               name = Core.Nothing, pipelinesRunningCount = Core.Nothing,
                               programCount = Core.Nothing, state = Core.Nothing,
                               tags = Core.Nothing, responseStatus}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsArn :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsArn = Lens.field @"arn"
{-# INLINEABLE dmrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsAvailabilityZones :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [Core.Text])
dmrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dmrrsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsDestinations :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
dmrrsDestinations = Lens.field @"destinations"
{-# INLINEABLE dmrrsDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsId :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsId = Lens.field @"id"
{-# INLINEABLE dmrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsMultiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Types.MultiplexSettings)
dmrrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE dmrrsMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsName :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
dmrrsName = Lens.field @"name"
{-# INLINEABLE dmrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsPipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
dmrrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# INLINEABLE dmrrsPipelinesRunningCount #-}
{-# DEPRECATED pipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead"  #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsProgramCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
dmrrsProgramCount = Lens.field @"programCount"
{-# INLINEABLE dmrrsProgramCount #-}
{-# DEPRECATED programCount "Use generic-lens or generic-optics with 'programCount' instead"  #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsState :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Types.MultiplexState)
dmrrsState = Lens.field @"state"
{-# INLINEABLE dmrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsTags :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dmrrsTags = Lens.field @"tags"
{-# INLINEABLE dmrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DescribeMultiplexResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
