{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fleet.
module Network.AWS.AppStream.DeleteFleet
    (
    -- * Creating a request
      DeleteFleet (..)
    , mkDeleteFleet
    -- ** Request lenses
    , dfName

    -- * Destructuring the response
    , DeleteFleetResponse (..)
    , mkDeleteFleetResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet'
  { name :: Core.Text
    -- ^ The name of the fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleet' value with any optional fields omitted.
mkDeleteFleet
    :: Core.Text -- ^ 'name'
    -> DeleteFleet
mkDeleteFleet name = DeleteFleet'{name}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFleet Core.Text
dfName = Lens.field @"name"
{-# INLINEABLE dfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFleet where
        toHeaders DeleteFleet{..}
          = Core.pure ("X-Amz-Target", "PhotonAdminProxyService.DeleteFleet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteFleet where
        toJSON DeleteFleet{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteFleet where
        type Rs DeleteFleet = DeleteFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteFleetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFleetResponse' smart constructor.
newtype DeleteFleetResponse = DeleteFleetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetResponse' value with any optional fields omitted.
mkDeleteFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFleetResponse
mkDeleteFleetResponse responseStatus
  = DeleteFleetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteFleetResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
