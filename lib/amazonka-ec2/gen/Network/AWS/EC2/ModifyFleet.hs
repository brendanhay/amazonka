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
    mfTargetCapacitySpecification,
    mfExcessCapacityTerminationPolicy,
    mfLaunchTemplateConfigs,
    mfFleetId,
    mfDryRun,

    -- * Destructuring the response
    ModifyFleetResponse (..),
    mkModifyFleetResponse,

    -- ** Response lenses
    mfrsReturn,
    mfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { -- | The size of the EC2 Fleet.
    targetCapacitySpecification :: Lude.Maybe TargetCapacitySpecificationRequest,
    -- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
    excessCapacityTerminationPolicy :: Lude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | The launch template and overrides.
    launchTemplateConfigs :: Lude.Maybe [FleetLaunchTemplateConfigRequest],
    -- | The ID of the EC2 Fleet.
    fleetId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyFleet' with the minimum fields required to make a request.
--
-- * 'targetCapacitySpecification' - The size of the EC2 Fleet.
-- * 'excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
-- * 'launchTemplateConfigs' - The launch template and overrides.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyFleet ::
  -- | 'fleetId'
  Lude.Text ->
  ModifyFleet
mkModifyFleet pFleetId_ =
  ModifyFleet'
    { targetCapacitySpecification = Lude.Nothing,
      excessCapacityTerminationPolicy = Lude.Nothing,
      launchTemplateConfigs = Lude.Nothing,
      fleetId = pFleetId_,
      dryRun = Lude.Nothing
    }

-- | The size of the EC2 Fleet.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfTargetCapacitySpecification :: Lens.Lens' ModifyFleet (Lude.Maybe TargetCapacitySpecificationRequest)
mfTargetCapacitySpecification = Lens.lens (targetCapacitySpecification :: ModifyFleet -> Lude.Maybe TargetCapacitySpecificationRequest) (\s a -> s {targetCapacitySpecification = a} :: ModifyFleet)
{-# DEPRECATED mfTargetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead." #-}

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfExcessCapacityTerminationPolicy :: Lens.Lens' ModifyFleet (Lude.Maybe FleetExcessCapacityTerminationPolicy)
mfExcessCapacityTerminationPolicy = Lens.lens (excessCapacityTerminationPolicy :: ModifyFleet -> Lude.Maybe FleetExcessCapacityTerminationPolicy) (\s a -> s {excessCapacityTerminationPolicy = a} :: ModifyFleet)
{-# DEPRECATED mfExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The launch template and overrides.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfLaunchTemplateConfigs :: Lens.Lens' ModifyFleet (Lude.Maybe [FleetLaunchTemplateConfigRequest])
mfLaunchTemplateConfigs = Lens.lens (launchTemplateConfigs :: ModifyFleet -> Lude.Maybe [FleetLaunchTemplateConfigRequest]) (\s a -> s {launchTemplateConfigs = a} :: ModifyFleet)
{-# DEPRECATED mfLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfFleetId :: Lens.Lens' ModifyFleet Lude.Text
mfFleetId = Lens.lens (fleetId :: ModifyFleet -> Lude.Text) (\s a -> s {fleetId = a} :: ModifyFleet)
{-# DEPRECATED mfFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfDryRun :: Lens.Lens' ModifyFleet (Lude.Maybe Lude.Bool)
mfDryRun = Lens.lens (dryRun :: ModifyFleet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyFleet)
{-# DEPRECATED mfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyFleet where
  type Rs ModifyFleet = ModifyFleetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyFleetResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyFleet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyFleet where
  toQuery ModifyFleet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyFleet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TargetCapacitySpecification" Lude.=: targetCapacitySpecification,
        "ExcessCapacityTerminationPolicy"
          Lude.=: excessCapacityTerminationPolicy,
        Lude.toQuery
          ( Lude.toQueryList "LaunchTemplateConfig"
              Lude.<$> launchTemplateConfigs
          ),
        "FleetId" Lude.=: fleetId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyFleetResponse' with the minimum fields required to make a request.
--
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
-- * 'responseStatus' - The response status code.
mkModifyFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyFleetResponse
mkModifyFleetResponse pResponseStatus_ =
  ModifyFleetResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfrsReturn :: Lens.Lens' ModifyFleetResponse (Lude.Maybe Lude.Bool)
mfrsReturn = Lens.lens (return :: ModifyFleetResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyFleetResponse)
{-# DEPRECATED mfrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfrsResponseStatus :: Lens.Lens' ModifyFleetResponse Lude.Int
mfrsResponseStatus = Lens.lens (responseStatus :: ModifyFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyFleetResponse)
{-# DEPRECATED mfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
