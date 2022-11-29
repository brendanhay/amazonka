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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Determines whether a service deployment fails if a service cannot reach
-- a steady state.
--
-- /See:/ 'newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' smart constructor.
data AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails = AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails'
  { -- | Whether to enable the deployment circuit breaker logic for the service.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | Whether to roll back the service if a service deployment fails. If
    -- rollback is enabled, when a service deployment fails, the service is
    -- rolled back to the last deployment that completed successfully.
    rollback :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable' - Whether to enable the deployment circuit breaker logic for the service.
--
-- 'rollback', 'awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback' - Whether to roll back the service if a service deployment fails. If
-- rollback is enabled, when a service deployment fails, the service is
-- rolled back to the last deployment that completed successfully.
newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails ::
  AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
newAwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails =
  AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails'
    { enable =
        Prelude.Nothing,
      rollback =
        Prelude.Nothing
    }

-- | Whether to enable the deployment circuit breaker logic for the service.
awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable :: Lens.Lens' AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_enable = Lens.lens (\AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {enable} -> enable) (\s@AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {} a -> s {enable = a} :: AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails)

-- | Whether to roll back the service if a service deployment fails. If
-- rollback is enabled, when a service deployment fails, the service is
-- rolled back to the last deployment that completed successfully.
awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback :: Lens.Lens' AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails (Prelude.Maybe Prelude.Bool)
awsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails_rollback = Lens.lens (\AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {rollback} -> rollback) (\s@AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {} a -> s {rollback = a} :: AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails)

instance
  Core.FromJSON
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails"
      ( \x ->
          AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails'
            Prelude.<$> (x Core..:? "Enable")
              Prelude.<*> (x Core..:? "Rollback")
      )

instance
  Prelude.Hashable
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
  where
  hashWithSalt
    _salt
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {..} =
      _salt `Prelude.hashWithSalt` enable
        `Prelude.hashWithSalt` rollback

instance
  Prelude.NFData
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
  where
  rnf
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {..} =
      Prelude.rnf enable
        `Prelude.seq` Prelude.rnf rollback

instance
  Core.ToJSON
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails
  where
  toJSON
    AwsEcsServiceDeploymentConfigurationDeploymentCircuitBreakerDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Enable" Core..=) Prelude.<$> enable,
              ("Rollback" Core..=) Prelude.<$> rollback
            ]
        )
