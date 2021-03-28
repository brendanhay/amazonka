{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the Capacity Reservation settings for a stopped instance. Use this action to configure an instance to target a specific Capacity Reservation, run in any @open@ Capacity Reservation with matching attributes, or run On-Demand Instance capacity.
module Network.AWS.EC2.ModifyInstanceCapacityReservationAttributes
    (
    -- * Creating a request
      ModifyInstanceCapacityReservationAttributes (..)
    , mkModifyInstanceCapacityReservationAttributes
    -- ** Request lenses
    , micraInstanceId
    , micraCapacityReservationSpecification
    , micraDryRun

    -- * Destructuring the response
    , ModifyInstanceCapacityReservationAttributesResponse (..)
    , mkModifyInstanceCapacityReservationAttributesResponse
    -- ** Response lenses
    , micrarrsReturn
    , micrarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributes' smart constructor.
data ModifyInstanceCapacityReservationAttributes = ModifyInstanceCapacityReservationAttributes'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance to be modified.
  , capacityReservationSpecification :: Types.CapacityReservationSpecification
    -- ^ Information about the Capacity Reservation targeting option.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCapacityReservationAttributes' value with any optional fields omitted.
mkModifyInstanceCapacityReservationAttributes
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.CapacityReservationSpecification -- ^ 'capacityReservationSpecification'
    -> ModifyInstanceCapacityReservationAttributes
mkModifyInstanceCapacityReservationAttributes instanceId
  capacityReservationSpecification
  = ModifyInstanceCapacityReservationAttributes'{instanceId,
                                                 capacityReservationSpecification,
                                                 dryRun = Core.Nothing}

-- | The ID of the instance to be modified.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraInstanceId :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Types.InstanceId
micraInstanceId = Lens.field @"instanceId"
{-# INLINEABLE micraInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraCapacityReservationSpecification :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Types.CapacityReservationSpecification
micraCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# INLINEABLE micraCapacityReservationSpecification #-}
{-# DEPRECATED capacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraDryRun :: Lens.Lens' ModifyInstanceCapacityReservationAttributes (Core.Maybe Core.Bool)
micraDryRun = Lens.field @"dryRun"
{-# INLINEABLE micraDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyInstanceCapacityReservationAttributes
         where
        toQuery ModifyInstanceCapacityReservationAttributes{..}
          = Core.toQueryPair "Action"
              ("ModifyInstanceCapacityReservationAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<>
              Core.toQueryPair "CapacityReservationSpecification"
                capacityReservationSpecification
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyInstanceCapacityReservationAttributes
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           ModifyInstanceCapacityReservationAttributes
         where
        type Rs ModifyInstanceCapacityReservationAttributes =
             ModifyInstanceCapacityReservationAttributesResponse
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
                 ModifyInstanceCapacityReservationAttributesResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributesResponse' smart constructor.
data ModifyInstanceCapacityReservationAttributesResponse = ModifyInstanceCapacityReservationAttributesResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCapacityReservationAttributesResponse' value with any optional fields omitted.
mkModifyInstanceCapacityReservationAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyInstanceCapacityReservationAttributesResponse
mkModifyInstanceCapacityReservationAttributesResponse
  responseStatus
  = ModifyInstanceCapacityReservationAttributesResponse'{return =
                                                           Core.Nothing,
                                                         responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarrsReturn :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse (Core.Maybe Core.Bool)
micrarrsReturn = Lens.field @"return"
{-# INLINEABLE micrarrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarrsResponseStatus :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse Core.Int
micrarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE micrarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
