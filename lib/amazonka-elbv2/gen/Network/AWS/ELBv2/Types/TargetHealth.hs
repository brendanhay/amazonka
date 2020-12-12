{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealth
  ( TargetHealth (..),

    -- * Smart constructor
    mkTargetHealth,

    -- * Lenses
    thState,
    thReason,
    thDescription,
  )
where

import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the current health of a target.
--
-- /See:/ 'mkTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { state ::
      Lude.Maybe TargetHealthStateEnum,
    reason :: Lude.Maybe TargetHealthReasonEnum,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- * 'description' - A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
-- * 'reason' - The reason code.
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
-- * 'state' - The state of the target.
mkTargetHealth ::
  TargetHealth
mkTargetHealth =
  TargetHealth'
    { state = Lude.Nothing,
      reason = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The state of the target.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thState :: Lens.Lens' TargetHealth (Lude.Maybe TargetHealthStateEnum)
thState = Lens.lens (state :: TargetHealth -> Lude.Maybe TargetHealthStateEnum) (\s a -> s {state = a} :: TargetHealth)
{-# DEPRECATED thState "Use generic-lens or generic-optics with 'state' instead." #-}

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
thReason :: Lens.Lens' TargetHealth (Lude.Maybe TargetHealthReasonEnum)
thReason = Lens.lens (reason :: TargetHealth -> Lude.Maybe TargetHealthReasonEnum) (\s a -> s {reason = a} :: TargetHealth)
{-# DEPRECATED thReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thDescription :: Lens.Lens' TargetHealth (Lude.Maybe Lude.Text)
thDescription = Lens.lens (description :: TargetHealth -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TargetHealth)
{-# DEPRECATED thDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Lude.<$> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "Reason")
      Lude.<*> (x Lude..@? "Description")
