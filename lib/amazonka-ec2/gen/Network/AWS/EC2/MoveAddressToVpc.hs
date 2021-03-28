{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.MoveAddressToVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an Elastic IP address from the EC2-Classic platform to the EC2-VPC platform. The Elastic IP address must be allocated to your account for more than 24 hours, and it must not be associated with an instance. After the Elastic IP address is moved, it is no longer available for use in the EC2-Classic platform, unless you move it back using the 'RestoreAddressToClassic' request. You cannot move an Elastic IP address that was originally allocated for use in the EC2-VPC platform to the EC2-Classic platform. 
module Network.AWS.EC2.MoveAddressToVpc
    (
    -- * Creating a request
      MoveAddressToVpc (..)
    , mkMoveAddressToVpc
    -- ** Request lenses
    , matvPublicIp
    , matvDryRun

    -- * Destructuring the response
    , MoveAddressToVpcResponse (..)
    , mkMoveAddressToVpcResponse
    -- ** Response lenses
    , matvrrsAllocationId
    , matvrrsStatus
    , matvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkMoveAddressToVpc' smart constructor.
data MoveAddressToVpc = MoveAddressToVpc'
  { publicIp :: Core.Text
    -- ^ The Elastic IP address.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MoveAddressToVpc' value with any optional fields omitted.
mkMoveAddressToVpc
    :: Core.Text -- ^ 'publicIp'
    -> MoveAddressToVpc
mkMoveAddressToVpc publicIp
  = MoveAddressToVpc'{publicIp, dryRun = Core.Nothing}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvPublicIp :: Lens.Lens' MoveAddressToVpc Core.Text
matvPublicIp = Lens.field @"publicIp"
{-# INLINEABLE matvPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvDryRun :: Lens.Lens' MoveAddressToVpc (Core.Maybe Core.Bool)
matvDryRun = Lens.field @"dryRun"
{-# INLINEABLE matvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery MoveAddressToVpc where
        toQuery MoveAddressToVpc{..}
          = Core.toQueryPair "Action" ("MoveAddressToVpc" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PublicIp" publicIp
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders MoveAddressToVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest MoveAddressToVpc where
        type Rs MoveAddressToVpc = MoveAddressToVpcResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 MoveAddressToVpcResponse' Core.<$>
                   (x Core..@? "allocationId") Core.<*> x Core..@? "status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkMoveAddressToVpcResponse' smart constructor.
data MoveAddressToVpcResponse = MoveAddressToVpcResponse'
  { allocationId :: Core.Maybe Core.Text
    -- ^ The allocation ID for the Elastic IP address.
  , status :: Core.Maybe Types.AddressStatus
    -- ^ The status of the move of the IP address.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MoveAddressToVpcResponse' value with any optional fields omitted.
mkMoveAddressToVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> MoveAddressToVpcResponse
mkMoveAddressToVpcResponse responseStatus
  = MoveAddressToVpcResponse'{allocationId = Core.Nothing,
                              status = Core.Nothing, responseStatus}

-- | The allocation ID for the Elastic IP address.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrrsAllocationId :: Lens.Lens' MoveAddressToVpcResponse (Core.Maybe Core.Text)
matvrrsAllocationId = Lens.field @"allocationId"
{-# INLINEABLE matvrrsAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | The status of the move of the IP address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrrsStatus :: Lens.Lens' MoveAddressToVpcResponse (Core.Maybe Types.AddressStatus)
matvrrsStatus = Lens.field @"status"
{-# INLINEABLE matvrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
matvrrsResponseStatus :: Lens.Lens' MoveAddressToVpcResponse Core.Int
matvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE matvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
