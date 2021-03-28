{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet scaling policy. Once deleted, the policy is no longer in force and GameLift removes all record of it. To delete a scaling policy, specify both the scaling policy name and the fleet ID it is associated with.
--
-- To temporarily suspend scaling policies, call 'StopFleetActions' . This operation suspends all policies for the fleet.
--
--     * 'DescribeFleetCapacity' 
--
--
--     * 'UpdateFleetCapacity' 
--
--
--     * 'DescribeEC2InstanceLimits' 
--
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions' 
--
--
--     * 'StopFleetActions' 
--
--
--
--
module Network.AWS.GameLift.DeleteScalingPolicy
    (
    -- * Creating a request
      DeleteScalingPolicy (..)
    , mkDeleteScalingPolicy
    -- ** Request lenses
    , dspName
    , dspFleetId

    -- * Destructuring the response
    , DeleteScalingPolicyResponse (..)
    , mkDeleteScalingPolicyResponse
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteScalingPolicy' smart constructor.
data DeleteScalingPolicy = DeleteScalingPolicy'
  { name :: Types.NonZeroAndMaxString
    -- ^ A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
  , fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScalingPolicy' value with any optional fields omitted.
mkDeleteScalingPolicy
    :: Types.NonZeroAndMaxString -- ^ 'name'
    -> Types.FleetIdOrArn -- ^ 'fleetId'
    -> DeleteScalingPolicy
mkDeleteScalingPolicy name fleetId
  = DeleteScalingPolicy'{name, fleetId}

-- | A descriptive label that is associated with a scaling policy. Policy names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspName :: Lens.Lens' DeleteScalingPolicy Types.NonZeroAndMaxString
dspName = Lens.field @"name"
{-# INLINEABLE dspName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspFleetId :: Lens.Lens' DeleteScalingPolicy Types.FleetIdOrArn
dspFleetId = Lens.field @"fleetId"
{-# INLINEABLE dspFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

instance Core.ToQuery DeleteScalingPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteScalingPolicy where
        toHeaders DeleteScalingPolicy{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteScalingPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteScalingPolicy where
        toJSON DeleteScalingPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("FleetId" Core..= fleetId)])

instance Core.AWSRequest DeleteScalingPolicy where
        type Rs DeleteScalingPolicy = DeleteScalingPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteScalingPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteScalingPolicyResponse' smart constructor.
data DeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScalingPolicyResponse' value with any optional fields omitted.
mkDeleteScalingPolicyResponse
    :: DeleteScalingPolicyResponse
mkDeleteScalingPolicyResponse = DeleteScalingPolicyResponse'
