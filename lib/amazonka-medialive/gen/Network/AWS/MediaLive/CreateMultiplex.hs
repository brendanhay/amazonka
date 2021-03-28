{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new multiplex.
module Network.AWS.MediaLive.CreateMultiplex
    (
    -- * Creating a request
      CreateMultiplex (..)
    , mkCreateMultiplex
    -- ** Request lenses
    , cmRequestId
    , cmMultiplexSettings
    , cmAvailabilityZones
    , cmName
    , cmTags

    -- * Destructuring the response
    , CreateMultiplexResponse (..)
    , mkCreateMultiplexResponse
    -- ** Response lenses
    , cmrrsMultiplex
    , cmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a multiplex.
--
-- /See:/ 'mkCreateMultiplex' smart constructor.
data CreateMultiplex = CreateMultiplex'
  { requestId :: Core.Text
    -- ^ Unique request ID. This prevents retries from creating multiple
--
-- resources.
  , multiplexSettings :: Types.MultiplexSettings
    -- ^ Configuration for a multiplex event.
  , availabilityZones :: [Core.Text]
    -- ^ A list of availability zones for the multiplex. You must specify exactly two.
  , name :: Core.Text
    -- ^ Name of multiplex.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplex' value with any optional fields omitted.
mkCreateMultiplex
    :: Core.Text -- ^ 'requestId'
    -> Types.MultiplexSettings -- ^ 'multiplexSettings'
    -> Core.Text -- ^ 'name'
    -> CreateMultiplex
mkCreateMultiplex requestId multiplexSettings name
  = CreateMultiplex'{requestId, multiplexSettings,
                     availabilityZones = Core.mempty, name, tags = Core.Nothing}

-- | Unique request ID. This prevents retries from creating multiple
--
-- resources.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRequestId :: Lens.Lens' CreateMultiplex Core.Text
cmRequestId = Lens.field @"requestId"
{-# INLINEABLE cmRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | Configuration for a multiplex event.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmMultiplexSettings :: Lens.Lens' CreateMultiplex Types.MultiplexSettings
cmMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE cmMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | A list of availability zones for the multiplex. You must specify exactly two.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAvailabilityZones :: Lens.Lens' CreateMultiplex [Core.Text]
cmAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE cmAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | Name of multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CreateMultiplex Core.Text
cmName = Lens.field @"name"
{-# INLINEABLE cmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTags :: Lens.Lens' CreateMultiplex (Core.Maybe (Core.HashMap Core.Text Core.Text))
cmTags = Lens.field @"tags"
{-# INLINEABLE cmTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateMultiplex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMultiplex where
        toHeaders CreateMultiplex{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateMultiplex where
        toJSON CreateMultiplex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("requestId" Core..= requestId),
                  Core.Just ("multiplexSettings" Core..= multiplexSettings),
                  Core.Just ("availabilityZones" Core..= availabilityZones),
                  Core.Just ("name" Core..= name), ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateMultiplex where
        type Rs CreateMultiplex = CreateMultiplexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/prod/multiplexes",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMultiplexResponse' Core.<$>
                   (x Core..:? "multiplex") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for CreateMultiplexResponse
--
-- /See:/ 'mkCreateMultiplexResponse' smart constructor.
data CreateMultiplexResponse = CreateMultiplexResponse'
  { multiplex :: Core.Maybe Types.Multiplex
    -- ^ The newly created multiplex.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMultiplexResponse' value with any optional fields omitted.
mkCreateMultiplexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMultiplexResponse
mkCreateMultiplexResponse responseStatus
  = CreateMultiplexResponse'{multiplex = Core.Nothing,
                             responseStatus}

-- | The newly created multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsMultiplex :: Lens.Lens' CreateMultiplexResponse (Core.Maybe Types.Multiplex)
cmrrsMultiplex = Lens.field @"multiplex"
{-# INLINEABLE cmrrsMultiplex #-}
{-# DEPRECATED multiplex "Use generic-lens or generic-optics with 'multiplex' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CreateMultiplexResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
