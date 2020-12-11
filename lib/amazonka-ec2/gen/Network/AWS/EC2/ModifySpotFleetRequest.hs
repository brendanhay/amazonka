{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    msfrTargetCapacity,
    msfrExcessCapacityTerminationPolicy,
    msfrOnDemandTargetCapacity,
    msfrLaunchTemplateConfigs,
    msfrSpotFleetRequestId,

    -- * Destructuring the response
    ModifySpotFleetRequestResponse (..),
    mkModifySpotFleetRequestResponse,

    -- ** Response lenses
    msfrrsReturn,
    msfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifySpotFleetRequest.
--
-- /See:/ 'mkModifySpotFleetRequest' smart constructor.
data ModifySpotFleetRequest = ModifySpotFleetRequest'
  { targetCapacity ::
      Lude.Maybe Lude.Int,
    excessCapacityTerminationPolicy ::
      Lude.Maybe ExcessCapacityTerminationPolicy,
    onDemandTargetCapacity :: Lude.Maybe Lude.Int,
    launchTemplateConfigs ::
      Lude.Maybe [LaunchTemplateConfig],
    spotFleetRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySpotFleetRequest' with the minimum fields required to make a request.
--
-- * 'excessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
-- * 'launchTemplateConfigs' - The launch template and overrides. You can only use this parameter if you specified a launch template (@LaunchTemplateConfigs@ ) in your Spot Fleet request. If you specified @LaunchSpecifications@ in your Spot Fleet request, then omit this parameter.
-- * 'onDemandTargetCapacity' - The number of On-Demand Instances in the fleet.
-- * 'spotFleetRequestId' - The ID of the Spot Fleet request.
-- * 'targetCapacity' - The size of the fleet.
mkModifySpotFleetRequest ::
  -- | 'spotFleetRequestId'
  Lude.Text ->
  ModifySpotFleetRequest
mkModifySpotFleetRequest pSpotFleetRequestId_ =
  ModifySpotFleetRequest'
    { targetCapacity = Lude.Nothing,
      excessCapacityTerminationPolicy = Lude.Nothing,
      onDemandTargetCapacity = Lude.Nothing,
      launchTemplateConfigs = Lude.Nothing,
      spotFleetRequestId = pSpotFleetRequestId_
    }

-- | The size of the fleet.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrTargetCapacity :: Lens.Lens' ModifySpotFleetRequest (Lude.Maybe Lude.Int)
msfrTargetCapacity = Lens.lens (targetCapacity :: ModifySpotFleetRequest -> Lude.Maybe Lude.Int) (\s a -> s {targetCapacity = a} :: ModifySpotFleetRequest)
{-# DEPRECATED msfrTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

-- | Indicates whether running Spot Instances should be terminated if the target capacity of the Spot Fleet request is decreased below the current size of the Spot Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrExcessCapacityTerminationPolicy :: Lens.Lens' ModifySpotFleetRequest (Lude.Maybe ExcessCapacityTerminationPolicy)
msfrExcessCapacityTerminationPolicy = Lens.lens (excessCapacityTerminationPolicy :: ModifySpotFleetRequest -> Lude.Maybe ExcessCapacityTerminationPolicy) (\s a -> s {excessCapacityTerminationPolicy = a} :: ModifySpotFleetRequest)
{-# DEPRECATED msfrExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The number of On-Demand Instances in the fleet.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrOnDemandTargetCapacity :: Lens.Lens' ModifySpotFleetRequest (Lude.Maybe Lude.Int)
msfrOnDemandTargetCapacity = Lens.lens (onDemandTargetCapacity :: ModifySpotFleetRequest -> Lude.Maybe Lude.Int) (\s a -> s {onDemandTargetCapacity = a} :: ModifySpotFleetRequest)
{-# DEPRECATED msfrOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | The launch template and overrides. You can only use this parameter if you specified a launch template (@LaunchTemplateConfigs@ ) in your Spot Fleet request. If you specified @LaunchSpecifications@ in your Spot Fleet request, then omit this parameter.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrLaunchTemplateConfigs :: Lens.Lens' ModifySpotFleetRequest (Lude.Maybe [LaunchTemplateConfig])
msfrLaunchTemplateConfigs = Lens.lens (launchTemplateConfigs :: ModifySpotFleetRequest -> Lude.Maybe [LaunchTemplateConfig]) (\s a -> s {launchTemplateConfigs = a} :: ModifySpotFleetRequest)
{-# DEPRECATED msfrLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrSpotFleetRequestId :: Lens.Lens' ModifySpotFleetRequest Lude.Text
msfrSpotFleetRequestId = Lens.lens (spotFleetRequestId :: ModifySpotFleetRequest -> Lude.Text) (\s a -> s {spotFleetRequestId = a} :: ModifySpotFleetRequest)
{-# DEPRECATED msfrSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

instance Lude.AWSRequest ModifySpotFleetRequest where
  type Rs ModifySpotFleetRequest = ModifySpotFleetRequestResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifySpotFleetRequestResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifySpotFleetRequest where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifySpotFleetRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySpotFleetRequest where
  toQuery ModifySpotFleetRequest' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifySpotFleetRequest" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TargetCapacity" Lude.=: targetCapacity,
        "ExcessCapacityTerminationPolicy"
          Lude.=: excessCapacityTerminationPolicy,
        "OnDemandTargetCapacity" Lude.=: onDemandTargetCapacity,
        Lude.toQuery
          ( Lude.toQueryList "LaunchTemplateConfig"
              Lude.<$> launchTemplateConfigs
          ),
        "SpotFleetRequestId" Lude.=: spotFleetRequestId
      ]

-- | Contains the output of ModifySpotFleetRequest.
--
-- /See:/ 'mkModifySpotFleetRequestResponse' smart constructor.
data ModifySpotFleetRequestResponse = ModifySpotFleetRequestResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySpotFleetRequestResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
mkModifySpotFleetRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifySpotFleetRequestResponse
mkModifySpotFleetRequestResponse pResponseStatus_ =
  ModifySpotFleetRequestResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrrsReturn :: Lens.Lens' ModifySpotFleetRequestResponse (Lude.Maybe Lude.Bool)
msfrrsReturn = Lens.lens (return :: ModifySpotFleetRequestResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifySpotFleetRequestResponse)
{-# DEPRECATED msfrrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msfrrsResponseStatus :: Lens.Lens' ModifySpotFleetRequestResponse Lude.Int
msfrrsResponseStatus = Lens.lens (responseStatus :: ModifySpotFleetRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifySpotFleetRequestResponse)
{-# DEPRECATED msfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
