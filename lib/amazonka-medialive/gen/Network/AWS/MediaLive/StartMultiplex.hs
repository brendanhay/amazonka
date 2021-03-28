{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartMultiplex (..)
    , mkStartMultiplex
    -- ** Request lenses
    , sMultiplexId

    -- * Destructuring the response
    , StartMultiplexResponse (..)
    , mkStartMultiplexResponse
    -- ** Response lenses
    , smrfrsArn
    , smrfrsAvailabilityZones
    , smrfrsDestinations
    , smrfrsId
    , smrfrsMultiplexSettings
    , smrfrsName
    , smrfrsPipelinesRunningCount
    , smrfrsProgramCount
    , smrfrsState
    , smrfrsTags
    , smrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'mkStartMultiplex' smart constructor.
newtype StartMultiplex = StartMultiplex'
  { multiplexId :: Core.Text
    -- ^ The ID of the multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartMultiplex' value with any optional fields omitted.
mkStartMultiplex
    :: Core.Text -- ^ 'multiplexId'
    -> StartMultiplex
mkStartMultiplex multiplexId = StartMultiplex'{multiplexId}

-- | The ID of the multiplex.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMultiplexId :: Lens.Lens' StartMultiplex Core.Text
sMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE sMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

instance Core.ToQuery StartMultiplex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMultiplex where
        toHeaders StartMultiplex{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMultiplex where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StartMultiplex where
        type Rs StartMultiplex = StartMultiplexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId Core.<>
                             "/start",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMultiplexResponse' Core.<$>
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

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'mkStartMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
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

-- | Creates a 'StartMultiplexResponse' value with any optional fields omitted.
mkStartMultiplexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMultiplexResponse
mkStartMultiplexResponse responseStatus
  = StartMultiplexResponse'{arn = Core.Nothing,
                            availabilityZones = Core.Nothing, destinations = Core.Nothing,
                            id = Core.Nothing, multiplexSettings = Core.Nothing,
                            name = Core.Nothing, pipelinesRunningCount = Core.Nothing,
                            programCount = Core.Nothing, state = Core.Nothing,
                            tags = Core.Nothing, responseStatus}

-- | The unique arn of the multiplex.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsArn :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsArn = Lens.field @"arn"
{-# INLINEABLE smrfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of availability zones for the multiplex.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsAvailabilityZones :: Lens.Lens' StartMultiplexResponse (Core.Maybe [Core.Text])
smrfrsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE smrfrsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | A list of the multiplex output destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsDestinations :: Lens.Lens' StartMultiplexResponse (Core.Maybe [Types.MultiplexOutputDestination])
smrfrsDestinations = Lens.field @"destinations"
{-# INLINEABLE smrfrsDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | The unique id of the multiplex.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsId :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsId = Lens.field @"id"
{-# INLINEABLE smrfrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsMultiplexSettings :: Lens.Lens' StartMultiplexResponse (Core.Maybe Types.MultiplexSettings)
smrfrsMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE smrfrsMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | The name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsName :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
smrfrsName = Lens.field @"name"
{-# INLINEABLE smrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of currently healthy pipelines.
--
-- /Note:/ Consider using 'pipelinesRunningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsPipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
smrfrsPipelinesRunningCount = Lens.field @"pipelinesRunningCount"
{-# INLINEABLE smrfrsPipelinesRunningCount #-}
{-# DEPRECATED pipelinesRunningCount "Use generic-lens or generic-optics with 'pipelinesRunningCount' instead"  #-}

-- | The number of programs in the multiplex.
--
-- /Note:/ Consider using 'programCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsProgramCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
smrfrsProgramCount = Lens.field @"programCount"
{-# INLINEABLE smrfrsProgramCount #-}
{-# DEPRECATED programCount "Use generic-lens or generic-optics with 'programCount' instead"  #-}

-- | The current state of the multiplex.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsState :: Lens.Lens' StartMultiplexResponse (Core.Maybe Types.MultiplexState)
smrfrsState = Lens.field @"state"
{-# INLINEABLE smrfrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsTags :: Lens.Lens' StartMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
smrfrsTags = Lens.field @"tags"
{-# INLINEABLE smrfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrfrsResponseStatus :: Lens.Lens' StartMultiplexResponse Core.Int
smrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
