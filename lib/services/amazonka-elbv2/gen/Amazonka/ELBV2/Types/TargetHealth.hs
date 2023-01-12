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
-- Module      : Amazonka.ELBV2.Types.TargetHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.TargetHealthReasonEnum
import Amazonka.ELBV2.Types.TargetHealthStateEnum
import qualified Amazonka.Prelude as Prelude

-- | Information about the current health of a target.
--
-- /See:/ 'newTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { -- | A description of the target health that provides additional details. If
    -- the state is @healthy@, a description is not provided.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | The state of the target.
    state :: Prelude.Maybe TargetHealthStateEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'targetHealth_description' - A description of the target health that provides additional details. If
-- the state is @healthy@, a description is not provided.
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
-- 'state', 'targetHealth_state' - The state of the target.
newTargetHealth ::
  TargetHealth
newTargetHealth =
  TargetHealth'
    { description = Prelude.Nothing,
      reason = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | A description of the target health that provides additional details. If
-- the state is @healthy@, a description is not provided.
targetHealth_description :: Lens.Lens' TargetHealth (Prelude.Maybe Prelude.Text)
targetHealth_description = Lens.lens (\TargetHealth' {description} -> description) (\s@TargetHealth' {} a -> s {description = a} :: TargetHealth)

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

-- | The state of the target.
targetHealth_state :: Lens.Lens' TargetHealth (Prelude.Maybe TargetHealthStateEnum)
targetHealth_state = Lens.lens (\TargetHealth' {state} -> state) (\s@TargetHealth' {} a -> s {state = a} :: TargetHealth)

instance Data.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Prelude.<$> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "Reason")
      Prelude.<*> (x Data..@? "State")

instance Prelude.Hashable TargetHealth where
  hashWithSalt _salt TargetHealth' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` state

instance Prelude.NFData TargetHealth where
  rnf TargetHealth' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf state
