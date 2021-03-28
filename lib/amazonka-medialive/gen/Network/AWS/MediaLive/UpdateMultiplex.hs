{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateMultiplex (..)
    , mkUpdateMultiplex
    -- ** Request lenses
    , umMultiplexId
    , umMultiplexSettings
    , umName

    -- * Destructuring the response
    , UpdateMultiplexResponse (..)
    , mkUpdateMultiplexResponse
    -- ** Response lenses
    , umrrsMultiplex
    , umrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a multiplex.
--
-- /See:/ 'mkUpdateMultiplex' smart constructor.
data UpdateMultiplex = UpdateMultiplex'
  { multiplexId :: Core.Text
    -- ^ ID of the multiplex to update.
  , multiplexSettings :: Core.Maybe Types.MultiplexSettings
    -- ^ The new settings for a multiplex.
  , name :: Core.Maybe Core.Text
    -- ^ Name of the multiplex.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplex' value with any optional fields omitted.
mkUpdateMultiplex
    :: Core.Text -- ^ 'multiplexId'
    -> UpdateMultiplex
mkUpdateMultiplex multiplexId
  = UpdateMultiplex'{multiplexId, multiplexSettings = Core.Nothing,
                     name = Core.Nothing}

-- | ID of the multiplex to update.
--
-- /Note:/ Consider using 'multiplexId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexId :: Lens.Lens' UpdateMultiplex Core.Text
umMultiplexId = Lens.field @"multiplexId"
{-# INLINEABLE umMultiplexId #-}
{-# DEPRECATED multiplexId "Use generic-lens or generic-optics with 'multiplexId' instead"  #-}

-- | The new settings for a multiplex.
--
-- /Note:/ Consider using 'multiplexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umMultiplexSettings :: Lens.Lens' UpdateMultiplex (Core.Maybe Types.MultiplexSettings)
umMultiplexSettings = Lens.field @"multiplexSettings"
{-# INLINEABLE umMultiplexSettings #-}
{-# DEPRECATED multiplexSettings "Use generic-lens or generic-optics with 'multiplexSettings' instead"  #-}

-- | Name of the multiplex.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umName :: Lens.Lens' UpdateMultiplex (Core.Maybe Core.Text)
umName = Lens.field @"name"
{-# INLINEABLE umName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateMultiplex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMultiplex where
        toHeaders UpdateMultiplex{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMultiplex where
        toJSON UpdateMultiplex{..}
          = Core.object
              (Core.catMaybes
                 [("multiplexSettings" Core..=) Core.<$> multiplexSettings,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateMultiplex where
        type Rs UpdateMultiplex = UpdateMultiplexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/prod/multiplexes/" Core.<> Core.toText multiplexId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMultiplexResponse' Core.<$>
                   (x Core..:? "multiplex") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'mkUpdateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { multiplex :: Core.Maybe Types.Multiplex
    -- ^ The updated multiplex.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMultiplexResponse' value with any optional fields omitted.
mkUpdateMultiplexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMultiplexResponse
mkUpdateMultiplexResponse responseStatus
  = UpdateMultiplexResponse'{multiplex = Core.Nothing,
                             responseStatus}

-- | The updated multiplex.
--
-- /Note:/ Consider using 'multiplex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrrsMultiplex :: Lens.Lens' UpdateMultiplexResponse (Core.Maybe Types.Multiplex)
umrrsMultiplex = Lens.field @"multiplex"
{-# INLINEABLE umrrsMultiplex #-}
{-# DEPRECATED multiplex "Use generic-lens or generic-optics with 'multiplex' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrrsResponseStatus :: Lens.Lens' UpdateMultiplexResponse Core.Int
umrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
