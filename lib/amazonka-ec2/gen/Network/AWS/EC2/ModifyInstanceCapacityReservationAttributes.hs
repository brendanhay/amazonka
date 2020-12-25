{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyInstanceCapacityReservationAttributes (..),
    mkModifyInstanceCapacityReservationAttributes,

    -- ** Request lenses
    micraInstanceId,
    micraCapacityReservationSpecification,
    micraDryRun,

    -- * Destructuring the response
    ModifyInstanceCapacityReservationAttributesResponse (..),
    mkModifyInstanceCapacityReservationAttributesResponse,

    -- ** Response lenses
    micrarrsReturn,
    micrarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributes' smart constructor.
data ModifyInstanceCapacityReservationAttributes = ModifyInstanceCapacityReservationAttributes'
  { -- | The ID of the instance to be modified.
    instanceId :: Types.InstanceId,
    -- | Information about the Capacity Reservation targeting option.
    capacityReservationSpecification :: Types.CapacityReservationSpecification,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCapacityReservationAttributes' value with any optional fields omitted.
mkModifyInstanceCapacityReservationAttributes ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'capacityReservationSpecification'
  Types.CapacityReservationSpecification ->
  ModifyInstanceCapacityReservationAttributes
mkModifyInstanceCapacityReservationAttributes
  instanceId
  capacityReservationSpecification =
    ModifyInstanceCapacityReservationAttributes'
      { instanceId,
        capacityReservationSpecification,
        dryRun = Core.Nothing
      }

-- | The ID of the instance to be modified.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraInstanceId :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Types.InstanceId
micraInstanceId = Lens.field @"instanceId"
{-# DEPRECATED micraInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Information about the Capacity Reservation targeting option.
--
-- /Note:/ Consider using 'capacityReservationSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraCapacityReservationSpecification :: Lens.Lens' ModifyInstanceCapacityReservationAttributes Types.CapacityReservationSpecification
micraCapacityReservationSpecification = Lens.field @"capacityReservationSpecification"
{-# DEPRECATED micraCapacityReservationSpecification "Use generic-lens or generic-optics with 'capacityReservationSpecification' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micraDryRun :: Lens.Lens' ModifyInstanceCapacityReservationAttributes (Core.Maybe Core.Bool)
micraDryRun = Lens.field @"dryRun"
{-# DEPRECATED micraDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Core.AWSRequest
    ModifyInstanceCapacityReservationAttributes
  where
  type
    Rs ModifyInstanceCapacityReservationAttributes =
      ModifyInstanceCapacityReservationAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure
                ("Action", "ModifyInstanceCapacityReservationAttributes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> ( Core.toQueryValue
                            "CapacityReservationSpecification"
                            capacityReservationSpecification
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceCapacityReservationAttributesResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyInstanceCapacityReservationAttributesResponse' smart constructor.
data ModifyInstanceCapacityReservationAttributesResponse = ModifyInstanceCapacityReservationAttributesResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceCapacityReservationAttributesResponse' value with any optional fields omitted.
mkModifyInstanceCapacityReservationAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyInstanceCapacityReservationAttributesResponse
mkModifyInstanceCapacityReservationAttributesResponse
  responseStatus =
    ModifyInstanceCapacityReservationAttributesResponse'
      { return =
          Core.Nothing,
        responseStatus
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarrsReturn :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse (Core.Maybe Core.Bool)
micrarrsReturn = Lens.field @"return"
{-# DEPRECATED micrarrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micrarrsResponseStatus :: Lens.Lens' ModifyInstanceCapacityReservationAttributesResponse Core.Int
micrarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED micrarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
