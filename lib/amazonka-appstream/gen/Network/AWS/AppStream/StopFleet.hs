{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StopFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
module Network.AWS.AppStream.StopFleet
    (
    -- * Creating a request
      StopFleet (..)
    , mkStopFleet
    -- ** Request lenses
    , sfName

    -- * Destructuring the response
    , StopFleetResponse (..)
    , mkStopFleetResponse
    -- ** Response lenses
    , sfrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopFleet' smart constructor.
newtype StopFleet = StopFleet'
  { name :: Core.Text
    -- ^ The name of the fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopFleet' value with any optional fields omitted.
mkStopFleet
    :: Core.Text -- ^ 'name'
    -> StopFleet
mkStopFleet name = StopFleet'{name}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' StopFleet Core.Text
sfName = Lens.field @"name"
{-# INLINEABLE sfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopFleet where
        toHeaders StopFleet{..}
          = Core.pure ("X-Amz-Target", "PhotonAdminProxyService.StopFleet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopFleet where
        toJSON StopFleet{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopFleet where
        type Rs StopFleet = StopFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopFleetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopFleetResponse' smart constructor.
newtype StopFleetResponse = StopFleetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopFleetResponse' value with any optional fields omitted.
mkStopFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopFleetResponse
mkStopFleetResponse responseStatus
  = StopFleetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrrsResponseStatus :: Lens.Lens' StopFleetResponse Core.Int
sfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
