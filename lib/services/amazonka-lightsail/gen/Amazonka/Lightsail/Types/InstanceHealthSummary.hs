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
-- Module      : Amazonka.Lightsail.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceHealthSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.InstanceHealthReason
import Amazonka.Lightsail.Types.InstanceHealthState
import qualified Amazonka.Prelude as Prelude

-- | Describes information about the health of the instance.
--
-- /See:/ 'newInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { -- | Describes the overall instance health. Valid values are below.
    instanceHealth :: Prelude.Maybe InstanceHealthState,
    -- | More information about the instance health. If the @instanceHealth@ is
    -- @healthy@, then an @instanceHealthReason@ value is not provided.
    --
    -- If __@instanceHealth@__ is @initial@, the __@instanceHealthReason@__
    -- value can be one of the following:
    --
    -- -   __@Lb.RegistrationInProgress@__ - The target instance is in the
    --     process of being registered with the load balancer.
    --
    -- -   __@Lb.InitialHealthChecking@__ - The Lightsail load balancer is
    --     still sending the target instance the minimum number of health
    --     checks required to determine its health status.
    --
    -- If __@instanceHealth@__ is @unhealthy@, the __@instanceHealthReason@__
    -- value can be one of the following:
    --
    -- -   __@Instance.ResponseCodeMismatch@__ - The health checks did not
    --     return an expected HTTP code.
    --
    -- -   __@Instance.Timeout@__ - The health check requests timed out.
    --
    -- -   __@Instance.FailedHealthChecks@__ - The health checks failed because
    --     the connection to the target instance timed out, the target instance
    --     response was malformed, or the target instance failed the health
    --     check for an unknown reason.
    --
    -- -   __@Lb.InternalError@__ - The health checks failed due to an internal
    --     error.
    --
    -- If __@instanceHealth@__ is @unused@, the __@instanceHealthReason@__
    -- value can be one of the following:
    --
    -- -   __@Instance.NotRegistered@__ - The target instance is not registered
    --     with the target group.
    --
    -- -   __@Instance.NotInUse@__ - The target group is not used by any load
    --     balancer, or the target instance is in an Availability Zone that is
    --     not enabled for its load balancer.
    --
    -- -   __@Instance.IpUnusable@__ - The target IP address is reserved for
    --     use by a Lightsail load balancer.
    --
    -- -   __@Instance.InvalidState@__ - The target is in the stopped or
    --     terminated state.
    --
    -- If __@instanceHealth@__ is @draining@, the __@instanceHealthReason@__
    -- value can be one of the following:
    --
    -- -   __@Instance.DeregistrationInProgress@__ - The target instance is in
    --     the process of being deregistered and the deregistration delay
    --     period has not expired.
    instanceHealthReason :: Prelude.Maybe InstanceHealthReason,
    -- | The name of the Lightsail instance for which you are requesting health
    -- check data.
    instanceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceHealthSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceHealth', 'instanceHealthSummary_instanceHealth' - Describes the overall instance health. Valid values are below.
--
-- 'instanceHealthReason', 'instanceHealthSummary_instanceHealthReason' - More information about the instance health. If the @instanceHealth@ is
-- @healthy@, then an @instanceHealthReason@ value is not provided.
--
-- If __@instanceHealth@__ is @initial@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Lb.RegistrationInProgress@__ - The target instance is in the
--     process of being registered with the load balancer.
--
-- -   __@Lb.InitialHealthChecking@__ - The Lightsail load balancer is
--     still sending the target instance the minimum number of health
--     checks required to determine its health status.
--
-- If __@instanceHealth@__ is @unhealthy@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.ResponseCodeMismatch@__ - The health checks did not
--     return an expected HTTP code.
--
-- -   __@Instance.Timeout@__ - The health check requests timed out.
--
-- -   __@Instance.FailedHealthChecks@__ - The health checks failed because
--     the connection to the target instance timed out, the target instance
--     response was malformed, or the target instance failed the health
--     check for an unknown reason.
--
-- -   __@Lb.InternalError@__ - The health checks failed due to an internal
--     error.
--
-- If __@instanceHealth@__ is @unused@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.NotRegistered@__ - The target instance is not registered
--     with the target group.
--
-- -   __@Instance.NotInUse@__ - The target group is not used by any load
--     balancer, or the target instance is in an Availability Zone that is
--     not enabled for its load balancer.
--
-- -   __@Instance.IpUnusable@__ - The target IP address is reserved for
--     use by a Lightsail load balancer.
--
-- -   __@Instance.InvalidState@__ - The target is in the stopped or
--     terminated state.
--
-- If __@instanceHealth@__ is @draining@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.DeregistrationInProgress@__ - The target instance is in
--     the process of being deregistered and the deregistration delay
--     period has not expired.
--
-- 'instanceName', 'instanceHealthSummary_instanceName' - The name of the Lightsail instance for which you are requesting health
-- check data.
newInstanceHealthSummary ::
  InstanceHealthSummary
newInstanceHealthSummary =
  InstanceHealthSummary'
    { instanceHealth =
        Prelude.Nothing,
      instanceHealthReason = Prelude.Nothing,
      instanceName = Prelude.Nothing
    }

-- | Describes the overall instance health. Valid values are below.
instanceHealthSummary_instanceHealth :: Lens.Lens' InstanceHealthSummary (Prelude.Maybe InstanceHealthState)
instanceHealthSummary_instanceHealth = Lens.lens (\InstanceHealthSummary' {instanceHealth} -> instanceHealth) (\s@InstanceHealthSummary' {} a -> s {instanceHealth = a} :: InstanceHealthSummary)

-- | More information about the instance health. If the @instanceHealth@ is
-- @healthy@, then an @instanceHealthReason@ value is not provided.
--
-- If __@instanceHealth@__ is @initial@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Lb.RegistrationInProgress@__ - The target instance is in the
--     process of being registered with the load balancer.
--
-- -   __@Lb.InitialHealthChecking@__ - The Lightsail load balancer is
--     still sending the target instance the minimum number of health
--     checks required to determine its health status.
--
-- If __@instanceHealth@__ is @unhealthy@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.ResponseCodeMismatch@__ - The health checks did not
--     return an expected HTTP code.
--
-- -   __@Instance.Timeout@__ - The health check requests timed out.
--
-- -   __@Instance.FailedHealthChecks@__ - The health checks failed because
--     the connection to the target instance timed out, the target instance
--     response was malformed, or the target instance failed the health
--     check for an unknown reason.
--
-- -   __@Lb.InternalError@__ - The health checks failed due to an internal
--     error.
--
-- If __@instanceHealth@__ is @unused@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.NotRegistered@__ - The target instance is not registered
--     with the target group.
--
-- -   __@Instance.NotInUse@__ - The target group is not used by any load
--     balancer, or the target instance is in an Availability Zone that is
--     not enabled for its load balancer.
--
-- -   __@Instance.IpUnusable@__ - The target IP address is reserved for
--     use by a Lightsail load balancer.
--
-- -   __@Instance.InvalidState@__ - The target is in the stopped or
--     terminated state.
--
-- If __@instanceHealth@__ is @draining@, the __@instanceHealthReason@__
-- value can be one of the following:
--
-- -   __@Instance.DeregistrationInProgress@__ - The target instance is in
--     the process of being deregistered and the deregistration delay
--     period has not expired.
instanceHealthSummary_instanceHealthReason :: Lens.Lens' InstanceHealthSummary (Prelude.Maybe InstanceHealthReason)
instanceHealthSummary_instanceHealthReason = Lens.lens (\InstanceHealthSummary' {instanceHealthReason} -> instanceHealthReason) (\s@InstanceHealthSummary' {} a -> s {instanceHealthReason = a} :: InstanceHealthSummary)

-- | The name of the Lightsail instance for which you are requesting health
-- check data.
instanceHealthSummary_instanceName :: Lens.Lens' InstanceHealthSummary (Prelude.Maybe Prelude.Text)
instanceHealthSummary_instanceName = Lens.lens (\InstanceHealthSummary' {instanceName} -> instanceName) (\s@InstanceHealthSummary' {} a -> s {instanceName = a} :: InstanceHealthSummary)

instance Data.FromJSON InstanceHealthSummary where
  parseJSON =
    Data.withObject
      "InstanceHealthSummary"
      ( \x ->
          InstanceHealthSummary'
            Prelude.<$> (x Data..:? "instanceHealth")
            Prelude.<*> (x Data..:? "instanceHealthReason")
            Prelude.<*> (x Data..:? "instanceName")
      )

instance Prelude.Hashable InstanceHealthSummary where
  hashWithSalt _salt InstanceHealthSummary' {..} =
    _salt
      `Prelude.hashWithSalt` instanceHealth
      `Prelude.hashWithSalt` instanceHealthReason
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData InstanceHealthSummary where
  rnf InstanceHealthSummary' {..} =
    Prelude.rnf instanceHealth `Prelude.seq`
      Prelude.rnf instanceHealthReason `Prelude.seq`
        Prelude.rnf instanceName
