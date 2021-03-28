{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteMultiplex (..)
    , mkDeleteMultiplex
    -- ** Request lenses
    , dMultiplexId

    -- * Destructuring the response
    , DeleteMultiplexResponse (..)
    , mkDeleteMultiplexResponse
    -- ** Response lenses
    , dmrfrsArn
    , dmrfrsAvailabilityZones
    , dmrfrsDestinations
    , dmrfrsId
    , dmrfrsMultiplexSettings
    , dmrfrsName
    , dmrfrsPipelinesRunningCount
    , dmrfrsProgramCount
    , dmrfrsState
    , dmrfrsTags
    , dmrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'mkDeleteMultiplex' smart constructor.
newtype DeleteMultiplex = DeleteMultiplex'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMultiplex' value with any optional fields omitted.
mkDeleteMultiplex
    :: Core.Text -- ^ 'multiplexId'
    -> DeleteMultiplex
mkDeleteMultiplex multiplexId = DeleteMultiplex'{multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMultiplexId :: Lens.Lens' DeleteMultiplex Core.Text
dMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE dMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

instance Core.ToQuery DeleteMultiplex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMultiplex where
        toHeaders DeleteMultiplex{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteMultiplex where
        type Rs DeleteMultiplex = DeleteMultiplexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteMultiplexResponse' Core.<$>
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

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'mkDeleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
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

-- | Creates a 'DeleteMultiplexResponse' value with any optional fields omitted.
mkDeleteMultiplexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMultiplexResponse
mkDeleteMultiplexResponse responseStatus
  = DeleteMultiplexResponse'{arn = Core.Nothing,
                             availabilityZones = Core.Nothing, destinations = Core.Nothing,
                             id = Core.Nothing, multiplexSettings = Core.Nothing,
                             name = Core.Nothing, pipelinesRunningCount = Core.Nothing,
                             programCount = Core.Nothing, state = Core.Nothing,
                             tags = Core.Nothing, responseStatus}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsArn :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsArn = Lens.field @"arn"
{-# INLINEABLE dmrfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsAvailabilityZones :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [Core.Text])
dmrfrsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dmrfrsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsDestinations :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
dmrfrsDestinations = Lens.field @"destinations"
{-# INLINEABLE dmrfrsDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsId :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsId = Lens.field @"id"
{-# INLINEABLE dmrfrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsMultiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Types.MultiplexSettings)
dmrfrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE dmrfrsMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsName :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
dmrfrsName = Lens.field @"name"
{-# INLINEABLE dmrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsPipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
dmrfrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# INLINEABLE dmrfrsPipelinesRunningCount #-}
{-# DEPRECATED pipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead"  #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsProgramCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
dmrfrsProgramCount = Lens.field @"programCount"
{-# INLINEABLE dmrfrsProgramCount #-}
{-# DEPRECATED programCount "Use generic-lens or generic-optics with 'programCount' instead"  #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsState :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Types.MultiplexState)
dmrfrsState = Lens.field @"state"
{-# INLINEABLE dmrfrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsTags :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dmrfrsTags = Lens.field @"tags"
{-# INLINEABLE dmrfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrfrsResponseStatus :: Lens.Lens' DeleteMultiplexResponse Core.Int
dmrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
