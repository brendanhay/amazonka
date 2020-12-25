{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifySpotFleetRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Spot Fleet request.
--
-- You can only modify a Spot Fleet request of type @maintain@ .
-- While the Spot Fleet request is being modified, it is in the @modifying@ state.
-- To scale up your Spot Fleet, increase its target capacity. The Spot Fleet launches the additional Spot Instances according to the allocation strategy for the Spot Fleet request. If the allocation strategy is @lowestPrice@ , the Spot Fleet launches instances using the Spot Instance pool with the lowest price. If the allocation strategy is @diversified@ , the Spot Fleet distributes the instances across the Spot Instance pools. If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
-- To scale down your Spot Fleet, decrease its target capacity. First, the Spot Fleet cancels any open requests that exceed the new target capacity. You can request that the Spot Fleet terminate Spot Instances until the size of the fleet no longer exceeds the new target capacity. If the allocation strategy is @lowestPrice@ , the Spot Fleet terminates the instances with the highest price per unit. If the allocation strategy is @capacityOptimized@ , the Spot Fleet terminates the instances in the Spot Instance pools that have the least available Spot Instance capacity. If the allocation strategy is @diversified@ , the Spot Fleet terminates instances across the Spot Instance pools. Alternatively, you can request that the Spot Fleet keep the fleet at its current size, but not replace any Spot Instances that are interrupted or that you terminate manually.
-- If you are finished with your Spot Fleet for now, but will use it again later, you can set the target capacity to 0.
module Network.AWS.EC2.ModifySpotFleetRequest
  ( -- * Creating a request
    ModifySpotFleetRequest (..),
    mkModifySpotFleetRequest,

    -- ** Request lenses
    msfrSpotFleetRequestId,
    msfrExcessCapacityTerminationPolicy,
    msfrLaunchTemplateConfigs,
    msfrOnDemandTargetCapacity,
    msfrTargetCapacity,

    -- * Destructuring the response
    ModifySpotFleetRequestResponse (..),
    mkModifySpotFleetRequestResponse,

    -- ** Response lenses
    msfrrrsReturn,
    msfrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifySpotFleetRequest.
--
-- /See:/ 'mkModifySpotFleetRequest' smart constructor.
data ModifySpotFleetRequest = ModifySpotFleetRequest'
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Types.SpotFleetRequestId,
    -- | Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
    excessCapacityTerminationPolicy :: Core.Maybe Types.ExcessCapacityTerminationPolicy,
    -- | The launch template and overrides. You can only use this parameter if you specified a launch template (@LaunchTemplateConfigs@ ) in your Spot Fleet request. If you specified @LaunchSpecifications@ in your Spot Fleet request, then omit this parameter.
    launchTemplateConfigs :: Core.Maybe [Types.LaunchTemplateConfig],
    -- | The number of On-Demand Instances in the fleet.
    onDemandTargetCapacity :: Core.Maybe Core.Int,
    -- | The size of the fleet.
    targetCapacity :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySpotFleetRequest' value with any optional fields omitted.
mkModifySpotFleetRequest ::
  -- | 'spotFleetRequestId'
  Types.SpotFleetRequestId ->
  ModifySpotFleetRequest
mkModifySpotFleetRequest spotFleetRequestId =
  ModifySpotFleetRequest'
    { spotFleetRequestId,
      excessCapacityTerminationPolicy = Core.Nothing,
      launchTemplateConfigs = Core.Nothing,
      onDemandTargetCapacity = Core.Nothing,
      targetCapacity = Core.Nothing
    }

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrSpotFleetRequestId :: Lens.Lens' ModifySpotFleetRequest Types.SpotFleetRequestId
msfrSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED msfrSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrExcessCapacityTerminationPolicy :: Lens.Lens' ModifySpotFleetRequest (Core.Maybe Types.ExcessCapacityTerminationPolicy)
msfrExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# DEPRECATED msfrExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The launch template and overrides. You can only use this parameter if you specified a launch template (@LaunchTemplateConfigs@ ) in your Spot Fleet request. If you specified @LaunchSpecifications@ in your Spot Fleet request, then omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrLaunchTemplateConfigs :: Lens.Lens' ModifySpotFleetRequest (Core.Maybe [Types.LaunchTemplateConfig])
msfrLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# DEPRECATED msfrLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The number of On-Demand Instances in the fleet.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrOnDemandTargetCapacity :: Lens.Lens' ModifySpotFleetRequest (Core.Maybe Core.Int)
msfrOnDemandTargetCapacity = Lens.field @"onDemandTargetCapacity"
{-# DEPRECATED msfrOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | The size of the fleet.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrTargetCapacity :: Lens.Lens' ModifySpotFleetRequest (Core.Maybe Core.Int)
msfrTargetCapacity = Lens.field @"targetCapacity"
{-# DEPRECATED msfrTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

instance Core.AWSRequest ModifySpotFleetRequest where
  type Rs ModifySpotFleetRequest = ModifySpotFleetRequestResponse
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
            ( Core.pure ("Action", "ModifySpotFleetRequest")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SpotFleetRequestId" spotFleetRequestId)
                Core.<> ( Core.toQueryValue "ExcessCapacityTerminationPolicy"
                            Core.<$> excessCapacityTerminationPolicy
                        )
                Core.<> ( Core.toQueryList "LaunchTemplateConfig"
                            Core.<$> launchTemplateConfigs
                        )
                Core.<> ( Core.toQueryValue "OnDemandTargetCapacity"
                            Core.<$> onDemandTargetCapacity
                        )
                Core.<> (Core.toQueryValue "TargetCapacity" Core.<$> targetCapacity)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifySpotFleetRequestResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ModifySpotFleetRequest.
--
-- /See:/ 'mkModifySpotFleetRequestResponse' smart constructor.
data ModifySpotFleetRequestResponse = ModifySpotFleetRequestResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySpotFleetRequestResponse' value with any optional fields omitted.
mkModifySpotFleetRequestResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifySpotFleetRequestResponse
mkModifySpotFleetRequestResponse responseStatus =
  ModifySpotFleetRequestResponse'
    { return = Core.Nothing,
      responseStatus
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrrrsReturn :: Lens.Lens' ModifySpotFleetRequestResponse (Core.Maybe Core.Bool)
msfrrrsReturn = Lens.field @"return"
{-# DEPRECATED msfrrrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrrrsResponseStatus :: Lens.Lens' ModifySpotFleetRequestResponse Core.Int
msfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED msfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
