{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RestoreAddressToClassic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an Elastic IP address that was previously moved to the EC2-VPC platform back to the EC2-Classic platform. You cannot move an Elastic IP address that was originally allocated for use in EC2-VPC. The Elastic IP address must not be associated with an instance or network interface.
module Network.AWS.EC2.RestoreAddressToClassic
    (
    -- * Creating a request
      RestoreAddressToClassic (..)
    , mkRestoreAddressToClassic
    -- ** Request lenses
    , ratcPublicIp
    , ratcDryRun

    -- * Destructuring the response
    , RestoreAddressToClassicResponse (..)
    , mkRestoreAddressToClassicResponse
    -- ** Response lenses
    , ratcrrsPublicIp
    , ratcrrsStatus
    , ratcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreAddressToClassic' smart constructor.
data RestoreAddressToClassic = RestoreAddressToClassic'
  { publicIp :: Core.Text
    -- ^ The Elastic IP address.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreAddressToClassic' value with any optional fields omitted.
mkRestoreAddressToClassic
    :: Core.Text -- ^ 'publicIp'
    -> RestoreAddressToClassic
mkRestoreAddressToClassic publicIp
  = RestoreAddressToClassic'{publicIp, dryRun = Core.Nothing}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcPublicIp :: Lens.Lens' RestoreAddressToClassic Core.Text
ratcPublicIp = Lens.field @"publicIp"
{-# INLINEABLE ratcPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcDryRun :: Lens.Lens' RestoreAddressToClassic (Core.Maybe Core.Bool)
ratcDryRun = Lens.field @"dryRun"
{-# INLINEABLE ratcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RestoreAddressToClassic where
        toQuery RestoreAddressToClassic{..}
          = Core.toQueryPair "Action"
              ("RestoreAddressToClassic" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PublicIp" publicIp
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RestoreAddressToClassic where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RestoreAddressToClassic where
        type Rs RestoreAddressToClassic = RestoreAddressToClassicResponse
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
                 RestoreAddressToClassicResponse' Core.<$>
                   (x Core..@? "publicIp") Core.<*> x Core..@? "status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreAddressToClassicResponse' smart constructor.
data RestoreAddressToClassicResponse = RestoreAddressToClassicResponse'
  { publicIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP address.
  , status :: Core.Maybe Types.AddressStatus
    -- ^ The move status for the IP address.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreAddressToClassicResponse' value with any optional fields omitted.
mkRestoreAddressToClassicResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreAddressToClassicResponse
mkRestoreAddressToClassicResponse responseStatus
  = RestoreAddressToClassicResponse'{publicIp = Core.Nothing,
                                     status = Core.Nothing, responseStatus}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsPublicIp :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe Core.Text)
ratcrrsPublicIp = Lens.field @"publicIp"
{-# INLINEABLE ratcrrsPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | The move status for the IP address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsStatus :: Lens.Lens' RestoreAddressToClassicResponse (Core.Maybe Types.AddressStatus)
ratcrrsStatus = Lens.field @"status"
{-# INLINEABLE ratcrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratcrrsResponseStatus :: Lens.Lens' RestoreAddressToClassicResponse Core.Int
ratcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ratcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
