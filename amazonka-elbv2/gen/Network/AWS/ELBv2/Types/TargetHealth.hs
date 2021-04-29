{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealth where

import Network.AWS.ELBv2.Types.TargetHealthReasonEnum
import Network.AWS.ELBv2.Types.TargetHealthStateEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the current health of a target.
--
-- /See:/ 'newTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { -- | The state of the target.
    state :: Prelude.Maybe TargetHealthStateEnum,
    -- | The reason code.
    --
    -- If the target state is @healthy@, a reason code is not provided.
    --
    -- If the target state is @initial@, the reason code can be one of the
    -- following values:
    --
    -- -   @Elb.RegistrationInProgress@ - The target is in the process of being
    --     registered with the load balancer.
    --
    -- -   @Elb.InitialHealthChecking@ - The load balancer is still sending the
    --     target the minimum number of health checks required to determine its
    --     health status.
    --
    -- If the target state is @unhealthy@, the reason code can be one of the
    -- following values:
    --
    -- -   @Target.ResponseCodeMismatch@ - The health checks did not return an
    --     expected HTTP code. Applies only to Application Load Balancers and
    --     Gateway Load Balancers.
    --
    -- -   @Target.Timeout@ - The health check requests timed out. Applies only
    --     to Application Load Balancers and Gateway Load Balancers.
    --
    -- -   @Target.FailedHealthChecks@ - The load balancer received an error
    --     while establishing a connection to the target or the target response
    --     was malformed.
    --
    -- -   @Elb.InternalError@ - The health checks failed due to an internal
    --     error. Applies only to Application Load Balancers.
    --
    -- If the target state is @unused@, the reason code can be one of the
    -- following values:
    --
    -- -   @Target.NotRegistered@ - The target is not registered with the
    --     target group.
    --
    -- -   @Target.NotInUse@ - The target group is not used by any load
    --     balancer or the target is in an Availability Zone that is not
    --     enabled for its load balancer.
    --
    -- -   @Target.InvalidState@ - The target is in the stopped or terminated
    --     state.
    --
    -- -   @Target.IpUnusable@ - The target IP address is reserved for use by a
    --     load balancer.
    --
    -- If the target state is @draining@, the reason code can be the following
    -- value:
    --
    -- -   @Target.DeregistrationInProgress@ - The target is in the process of
    --     being deregistered and the deregistration delay period has not
    --     expired.
    --
    -- If the target state is @unavailable@, the reason code can be the
    -- following value:
    --
    -- -   @Target.HealthCheckDisabled@ - Health checks are disabled for the
    --     target group. Applies only to Application Load Balancers.
    --
    -- -   @Elb.InternalError@ - Target health is unavailable due to an
    --     internal error. Applies only to Network Load Balancers.
    reason :: Prelude.Maybe TargetHealthReasonEnum,
    -- | A description of the target health that provides additional details. If
    -- the state is @healthy@, a description is not provided.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'targetHealth_state' - The state of the target.
--
-- 'reason', 'targetHealth_reason' - The reason code.
--
-- If the target state is @healthy@, a reason code is not provided.
--
-- If the target state is @initial@, the reason code can be one of the
-- following values:
--
-- -   @Elb.RegistrationInProgress@ - The target is in the process of being
--     registered with the load balancer.
--
-- -   @Elb.InitialHealthChecking@ - The load balancer is still sending the
--     target the minimum number of health checks required to determine its
--     health status.
--
-- If the target state is @unhealthy@, the reason code can be one of the
-- following values:
--
-- -   @Target.ResponseCodeMismatch@ - The health checks did not return an
--     expected HTTP code. Applies only to Application Load Balancers and
--     Gateway Load Balancers.
--
-- -   @Target.Timeout@ - The health check requests timed out. Applies only
--     to Application Load Balancers and Gateway Load Balancers.
--
-- -   @Target.FailedHealthChecks@ - The load balancer received an error
--     while establishing a connection to the target or the target response
--     was malformed.
--
-- -   @Elb.InternalError@ - The health checks failed due to an internal
--     error. Applies only to Application Load Balancers.
--
-- If the target state is @unused@, the reason code can be one of the
-- following values:
--
-- -   @Target.NotRegistered@ - The target is not registered with the
--     target group.
--
-- -   @Target.NotInUse@ - The target group is not used by any load
--     balancer or the target is in an Availability Zone that is not
--     enabled for its load balancer.
--
-- -   @Target.InvalidState@ - The target is in the stopped or terminated
--     state.
--
-- -   @Target.IpUnusable@ - The target IP address is reserved for use by a
--     load balancer.
--
-- If the target state is @draining@, the reason code can be the following
-- value:
--
-- -   @Target.DeregistrationInProgress@ - The target is in the process of
--     being deregistered and the deregistration delay period has not
--     expired.
--
-- If the target state is @unavailable@, the reason code can be the
-- following value:
--
-- -   @Target.HealthCheckDisabled@ - Health checks are disabled for the
--     target group. Applies only to Application Load Balancers.
--
-- -   @Elb.InternalError@ - Target health is unavailable due to an
--     internal error. Applies only to Network Load Balancers.
--
-- 'description', 'targetHealth_description' - A description of the target health that provides additional details. If
-- the state is @healthy@, a description is not provided.
newTargetHealth ::
  TargetHealth
newTargetHealth =
  TargetHealth'
    { state = Prelude.Nothing,
      reason = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The state of the target.
targetHealth_state :: Lens.Lens' TargetHealth (Prelude.Maybe TargetHealthStateEnum)
targetHealth_state = Lens.lens (\TargetHealth' {state} -> state) (\s@TargetHealth' {} a -> s {state = a} :: TargetHealth)

-- | The reason code.
--
-- If the target state is @healthy@, a reason code is not provided.
--
-- If the target state is @initial@, the reason code can be one of the
-- following values:
--
-- -   @Elb.RegistrationInProgress@ - The target is in the process of being
--     registered with the load balancer.
--
-- -   @Elb.InitialHealthChecking@ - The load balancer is still sending the
--     target the minimum number of health checks required to determine its
--     health status.
--
-- If the target state is @unhealthy@, the reason code can be one of the
-- following values:
--
-- -   @Target.ResponseCodeMismatch@ - The health checks did not return an
--     expected HTTP code. Applies only to Application Load Balancers and
--     Gateway Load Balancers.
--
-- -   @Target.Timeout@ - The health check requests timed out. Applies only
--     to Application Load Balancers and Gateway Load Balancers.
--
-- -   @Target.FailedHealthChecks@ - The load balancer received an error
--     while establishing a connection to the target or the target response
--     was malformed.
--
-- -   @Elb.InternalError@ - The health checks failed due to an internal
--     error. Applies only to Application Load Balancers.
--
-- If the target state is @unused@, the reason code can be one of the
-- following values:
--
-- -   @Target.NotRegistered@ - The target is not registered with the
--     target group.
--
-- -   @Target.NotInUse@ - The target group is not used by any load
--     balancer or the target is in an Availability Zone that is not
--     enabled for its load balancer.
--
-- -   @Target.InvalidState@ - The target is in the stopped or terminated
--     state.
--
-- -   @Target.IpUnusable@ - The target IP address is reserved for use by a
--     load balancer.
--
-- If the target state is @draining@, the reason code can be the following
-- value:
--
-- -   @Target.DeregistrationInProgress@ - The target is in the process of
--     being deregistered and the deregistration delay period has not
--     expired.
--
-- If the target state is @unavailable@, the reason code can be the
-- following value:
--
-- -   @Target.HealthCheckDisabled@ - Health checks are disabled for the
--     target group. Applies only to Application Load Balancers.
--
-- -   @Elb.InternalError@ - Target health is unavailable due to an
--     internal error. Applies only to Network Load Balancers.
targetHealth_reason :: Lens.Lens' TargetHealth (Prelude.Maybe TargetHealthReasonEnum)
targetHealth_reason = Lens.lens (\TargetHealth' {reason} -> reason) (\s@TargetHealth' {} a -> s {reason = a} :: TargetHealth)

-- | A description of the target health that provides additional details. If
-- the state is @healthy@, a description is not provided.
targetHealth_description :: Lens.Lens' TargetHealth (Prelude.Maybe Prelude.Text)
targetHealth_description = Lens.lens (\TargetHealth' {description} -> description) (\s@TargetHealth' {} a -> s {description = a} :: TargetHealth)

instance Prelude.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Prelude.<$> (x Prelude..@? "State")
      Prelude.<*> (x Prelude..@? "Reason")
      Prelude.<*> (x Prelude..@? "Description")

instance Prelude.Hashable TargetHealth

instance Prelude.NFData TargetHealth
