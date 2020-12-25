{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified EC2 Fleet.
--
-- You can only modify an EC2 Fleet request of type @maintain@ .
-- While the EC2 Fleet is being modified, it is in the @modifying@ state.
-- To scale up your EC2 Fleet, increase its target capacity. The EC2 Fleet launches the additional Spot Instances according to the allocation strategy for the EC2 Fleet request. If the allocation strategy is @lowest-price@ , the EC2 Fleet launches instances using the Spot Instance pool with the lowest price. If the allocation strategy is @diversified@ , the EC2 Fleet distributes the instances across the Spot Instance pools. If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
-- To scale down your EC2 Fleet, decrease its target capacity. First, the EC2 Fleet cancels any open requests that exceed the new target capacity. You can request that the EC2 Fleet terminate Spot Instances until the size of the fleet no longer exceeds the new target capacity. If the allocation strategy is @lowest-price@ , the EC2 Fleet terminates the instances with the highest price per unit. If the allocation strategy is @capacity-optimized@ , the EC2 Fleet terminates the instances in the Spot Instance pools that have the least available Spot Instance capacity. If the allocation strategy is @diversified@ , the EC2 Fleet terminates instances across the Spot Instance pools. Alternatively, you can request that the EC2 Fleet keep the fleet at its current size, but not replace any Spot Instances that are interrupted or that you terminate manually.
-- If you are finished with your EC2 Fleet for now, but will use it again later, you can set the target capacity to 0.
module Network.AWS.EC2.ModifyFleet
  ( -- * Creating a request
    ModifyFleet (..),
    mkModifyFleet,

    -- ** Request lenses
    mfFleetId,
    mfDryRun,
    mfExcessCapacityTerminationPolicy,
    mfLaunchTemplateConfigs,
    mfTargetCapacitySpecification,

    -- * Destructuring the response
    ModifyFleetResponse (..),
    mkModifyFleetResponse,

    -- ** Response lenses
    mfrrsReturn,
    mfrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { -- | The ID of the EC2 Fleet.
    fleetId :: Types.FleetId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
    excessCapacityTerminationPolicy :: Core.Maybe Types.FleetExcessCapacityTerminationPolicy,
    -- | The launch template and overrides.
    launchTemplateConfigs :: Core.Maybe [Types.FleetLaunchTemplateConfigRequest],
    -- | The size of the EC2 Fleet.
    targetCapacitySpecification :: Core.Maybe Types.TargetCapacitySpecificationRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFleet' value with any optional fields omitted.
mkModifyFleet ::
  -- | 'fleetId'
  Types.FleetId ->
  ModifyFleet
mkModifyFleet fleetId =
  ModifyFleet'
    { fleetId,
      dryRun = Core.Nothing,
      excessCapacityTerminationPolicy = Core.Nothing,
      launchTemplateConfigs = Core.Nothing,
      targetCapacitySpecification = Core.Nothing
    }

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFleetId :: Lens.Lens' ModifyFleet Types.FleetId
mfFleetId = Lens.field @"fleetId"
{-# DEPRECATED mfFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfDryRun :: Lens.Lens' ModifyFleet (Core.Maybe Core.Bool)
mfDryRun = Lens.field @"dryRun"
{-# DEPRECATED mfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfExcessCapacityTerminationPolicy :: Lens.Lens' ModifyFleet (Core.Maybe Types.FleetExcessCapacityTerminationPolicy)
mfExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# DEPRECATED mfExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The launch template and overrides.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfLaunchTemplateConfigs :: Lens.Lens' ModifyFleet (Core.Maybe [Types.FleetLaunchTemplateConfigRequest])
mfLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# DEPRECATED mfLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The size of the EC2 Fleet.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfTargetCapacitySpecification :: Lens.Lens' ModifyFleet (Core.Maybe Types.TargetCapacitySpecificationRequest)
mfTargetCapacitySpecification = Lens.field @"targetCapacitySpecification"
{-# DEPRECATED mfTargetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead." #-}

instance Core.AWSRequest ModifyFleet where
  type Rs ModifyFleet = ModifyFleetResponse
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
            ( Core.pure ("Action", "ModifyFleet")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "FleetId" fleetId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryValue "ExcessCapacityTerminationPolicy"
                            Core.<$> excessCapacityTerminationPolicy
                        )
                Core.<> ( Core.toQueryList "LaunchTemplateConfig"
                            Core.<$> launchTemplateConfigs
                        )
                Core.<> ( Core.toQueryValue "TargetCapacitySpecification"
                            Core.<$> targetCapacitySpecification
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFleetResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyFleetResponse' value with any optional fields omitted.
mkModifyFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyFleetResponse
mkModifyFleetResponse responseStatus =
  ModifyFleetResponse' {return = Core.Nothing, responseStatus}

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfrrsReturn :: Lens.Lens' ModifyFleetResponse (Core.Maybe Core.Bool)
mfrrsReturn = Lens.field @"return"
{-# DEPRECATED mfrrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfrrsResponseStatus :: Lens.Lens' ModifyFleetResponse Core.Int
mfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
