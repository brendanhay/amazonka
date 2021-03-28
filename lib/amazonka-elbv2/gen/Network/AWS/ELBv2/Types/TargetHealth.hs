{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetHealth
  ( TargetHealth (..)
  -- * Smart constructor
  , mkTargetHealth
  -- * Lenses
  , thDescription
  , thReason
  , thState
  ) where

import qualified Network.AWS.ELBv2.Types.Description as Types
import qualified Network.AWS.ELBv2.Types.TargetHealthReasonEnum as Types
import qualified Network.AWS.ELBv2.Types.TargetHealthStateEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the current health of a target.
--
-- /See:/ 'mkTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { description :: Core.Maybe Types.Description
    -- ^ A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
  , reason :: Core.Maybe Types.TargetHealthReasonEnum
    -- ^ The reason code.
--
-- If the target state is @healthy@ , a reason code is not provided.
-- If the target state is @initial@ , the reason code can be one of the following values:
--
--     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.
--
--
--     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status.
--
--
-- If the target state is @unhealthy@ , the reason code can be one of the following values:
--
--     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code. Applies only to Application Load Balancers and Gateway Load Balancers.
--
--
--     * @Target.Timeout@ - The health check requests timed out. Applies only to Application Load Balancers and Gateway Load Balancers.
--
--
--     * @Target.FailedHealthChecks@ - The load balancer received an error while establishing a connection to the target or the target response was malformed.
--
--
--     * @Elb.InternalError@ - The health checks failed due to an internal error. Applies only to Application Load Balancers.
--
--
-- If the target state is @unused@ , the reason code can be one of the following values:
--
--     * @Target.NotRegistered@ - The target is not registered with the target group.
--
--
--     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.
--
--
--     * @Target.InvalidState@ - The target is in the stopped or terminated state.
--
--
--     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.
--
--
-- If the target state is @draining@ , the reason code can be the following value:
--
--     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired.
--
--
-- If the target state is @unavailable@ , the reason code can be the following value:
--
--     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group. Applies only to Application Load Balancers.
--
--
--     * @Elb.InternalError@ - Target health is unavailable due to an internal error. Applies only to Network Load Balancers.
--
--
  , state :: Core.Maybe Types.TargetHealthStateEnum
    -- ^ The state of the target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetHealth' value with any optional fields omitted.
mkTargetHealth
    :: TargetHealth
mkTargetHealth
  = TargetHealth'{description = Core.Nothing, reason = Core.Nothing,
                  state = Core.Nothing}

-- | A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thDescription :: Lens.Lens' TargetHealth (Core.Maybe Types.Description)
thDescription = Lens.field @"description"
{-# INLINEABLE thDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The reason code.
--
-- If the target state is @healthy@ , a reason code is not provided.
-- If the target state is @initial@ , the reason code can be one of the following values:
--
--     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.
--
--
--     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status.
--
--
-- If the target state is @unhealthy@ , the reason code can be one of the following values:
--
--     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code. Applies only to Application Load Balancers and Gateway Load Balancers.
--
--
--     * @Target.Timeout@ - The health check requests timed out. Applies only to Application Load Balancers and Gateway Load Balancers.
--
--
--     * @Target.FailedHealthChecks@ - The load balancer received an error while establishing a connection to the target or the target response was malformed.
--
--
--     * @Elb.InternalError@ - The health checks failed due to an internal error. Applies only to Application Load Balancers.
--
--
-- If the target state is @unused@ , the reason code can be one of the following values:
--
--     * @Target.NotRegistered@ - The target is not registered with the target group.
--
--
--     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.
--
--
--     * @Target.InvalidState@ - The target is in the stopped or terminated state.
--
--
--     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.
--
--
-- If the target state is @draining@ , the reason code can be the following value:
--
--     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired.
--
--
-- If the target state is @unavailable@ , the reason code can be the following value:
--
--     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group. Applies only to Application Load Balancers.
--
--
--     * @Elb.InternalError@ - Target health is unavailable due to an internal error. Applies only to Network Load Balancers.
--
--
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thReason :: Lens.Lens' TargetHealth (Core.Maybe Types.TargetHealthReasonEnum)
thReason = Lens.field @"reason"
{-# INLINEABLE thReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The state of the target.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thState :: Lens.Lens' TargetHealth (Core.Maybe Types.TargetHealthStateEnum)
thState = Lens.field @"state"
{-# INLINEABLE thState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML TargetHealth where
        parseXML x
          = TargetHealth' Core.<$>
              (x Core..@? "Description") Core.<*> x Core..@? "Reason" Core.<*>
                x Core..@? "State"
