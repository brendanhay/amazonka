{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DisassociateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified fleet from the specified stack.
module Network.AWS.AppStream.DisassociateFleet
    (
    -- * Creating a request
      DisassociateFleet (..)
    , mkDisassociateFleet
    -- ** Request lenses
    , dfFleetName
    , dfStackName

    -- * Destructuring the response
    , DisassociateFleetResponse (..)
    , mkDisassociateFleetResponse
    -- ** Response lenses
    , dfrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateFleet' smart constructor.
data DisassociateFleet = DisassociateFleet'
  { fleetName :: Core.Text
    -- ^ The name of the fleet.
  , stackName :: Core.Text
    -- ^ The name of the stack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFleet' value with any optional fields omitted.
mkDisassociateFleet
    :: Core.Text -- ^ 'fleetName'
    -> Core.Text -- ^ 'stackName'
    -> DisassociateFleet
mkDisassociateFleet fleetName stackName
  = DisassociateFleet'{fleetName, stackName}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetName :: Lens.Lens' DisassociateFleet Core.Text
dfFleetName = Lens.field @"fleetName"
{-# INLINEABLE dfFleetName #-}
{-# DEPRECATED fleetName "Use generic-lens or generic-optics with 'fleetName' instead"  #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStackName :: Lens.Lens' DisassociateFleet Core.Text
dfStackName = Lens.field @"stackName"
{-# INLINEABLE dfStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

instance Core.ToQuery DisassociateFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateFleet where
        toHeaders DisassociateFleet{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.DisassociateFleet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateFleet where
        toJSON DisassociateFleet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetName" Core..= fleetName),
                  Core.Just ("StackName" Core..= stackName)])

instance Core.AWSRequest DisassociateFleet where
        type Rs DisassociateFleet = DisassociateFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateFleetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateFleetResponse' smart constructor.
newtype DisassociateFleetResponse = DisassociateFleetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFleetResponse' value with any optional fields omitted.
mkDisassociateFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateFleetResponse
mkDisassociateFleetResponse responseStatus
  = DisassociateFleetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DisassociateFleetResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
