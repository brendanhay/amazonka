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
-- Module      : Amazonka.GreengrassV2.Types.DeploymentPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DeploymentPolicies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicy
import Amazonka.GreengrassV2.Types.DeploymentConfigurationValidationPolicy
import Amazonka.GreengrassV2.Types.DeploymentFailureHandlingPolicy
import qualified Amazonka.Prelude as Prelude

-- | Contains information about policies that define how a deployment updates
-- components and handles failure.
--
-- /See:/ 'newDeploymentPolicies' smart constructor.
data DeploymentPolicies = DeploymentPolicies'
  { -- | The component update policy for the configuration deployment. This
    -- policy defines when it\'s safe to deploy the configuration to devices.
    componentUpdatePolicy :: Prelude.Maybe DeploymentComponentUpdatePolicy,
    -- | The configuration validation policy for the configuration deployment.
    -- This policy defines how long each component has to validate its
    -- configure updates.
    configurationValidationPolicy :: Prelude.Maybe DeploymentConfigurationValidationPolicy,
    -- | The failure handling policy for the configuration deployment. This
    -- policy defines what to do if the deployment fails.
    --
    -- Default: @ROLLBACK@
    failureHandlingPolicy :: Prelude.Maybe DeploymentFailureHandlingPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentUpdatePolicy', 'deploymentPolicies_componentUpdatePolicy' - The component update policy for the configuration deployment. This
-- policy defines when it\'s safe to deploy the configuration to devices.
--
-- 'configurationValidationPolicy', 'deploymentPolicies_configurationValidationPolicy' - The configuration validation policy for the configuration deployment.
-- This policy defines how long each component has to validate its
-- configure updates.
--
-- 'failureHandlingPolicy', 'deploymentPolicies_failureHandlingPolicy' - The failure handling policy for the configuration deployment. This
-- policy defines what to do if the deployment fails.
--
-- Default: @ROLLBACK@
newDeploymentPolicies ::
  DeploymentPolicies
newDeploymentPolicies =
  DeploymentPolicies'
    { componentUpdatePolicy =
        Prelude.Nothing,
      configurationValidationPolicy = Prelude.Nothing,
      failureHandlingPolicy = Prelude.Nothing
    }

-- | The component update policy for the configuration deployment. This
-- policy defines when it\'s safe to deploy the configuration to devices.
deploymentPolicies_componentUpdatePolicy :: Lens.Lens' DeploymentPolicies (Prelude.Maybe DeploymentComponentUpdatePolicy)
deploymentPolicies_componentUpdatePolicy = Lens.lens (\DeploymentPolicies' {componentUpdatePolicy} -> componentUpdatePolicy) (\s@DeploymentPolicies' {} a -> s {componentUpdatePolicy = a} :: DeploymentPolicies)

-- | The configuration validation policy for the configuration deployment.
-- This policy defines how long each component has to validate its
-- configure updates.
deploymentPolicies_configurationValidationPolicy :: Lens.Lens' DeploymentPolicies (Prelude.Maybe DeploymentConfigurationValidationPolicy)
deploymentPolicies_configurationValidationPolicy = Lens.lens (\DeploymentPolicies' {configurationValidationPolicy} -> configurationValidationPolicy) (\s@DeploymentPolicies' {} a -> s {configurationValidationPolicy = a} :: DeploymentPolicies)

-- | The failure handling policy for the configuration deployment. This
-- policy defines what to do if the deployment fails.
--
-- Default: @ROLLBACK@
deploymentPolicies_failureHandlingPolicy :: Lens.Lens' DeploymentPolicies (Prelude.Maybe DeploymentFailureHandlingPolicy)
deploymentPolicies_failureHandlingPolicy = Lens.lens (\DeploymentPolicies' {failureHandlingPolicy} -> failureHandlingPolicy) (\s@DeploymentPolicies' {} a -> s {failureHandlingPolicy = a} :: DeploymentPolicies)

instance Data.FromJSON DeploymentPolicies where
  parseJSON =
    Data.withObject
      "DeploymentPolicies"
      ( \x ->
          DeploymentPolicies'
            Prelude.<$> (x Data..:? "componentUpdatePolicy")
            Prelude.<*> (x Data..:? "configurationValidationPolicy")
            Prelude.<*> (x Data..:? "failureHandlingPolicy")
      )

instance Prelude.Hashable DeploymentPolicies where
  hashWithSalt _salt DeploymentPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` componentUpdatePolicy
      `Prelude.hashWithSalt` configurationValidationPolicy
      `Prelude.hashWithSalt` failureHandlingPolicy

instance Prelude.NFData DeploymentPolicies where
  rnf DeploymentPolicies' {..} =
    Prelude.rnf componentUpdatePolicy
      `Prelude.seq` Prelude.rnf configurationValidationPolicy
      `Prelude.seq` Prelude.rnf failureHandlingPolicy

instance Data.ToJSON DeploymentPolicies where
  toJSON DeploymentPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentUpdatePolicy" Data..=)
              Prelude.<$> componentUpdatePolicy,
            ("configurationValidationPolicy" Data..=)
              Prelude.<$> configurationValidationPolicy,
            ("failureHandlingPolicy" Data..=)
              Prelude.<$> failureHandlingPolicy
          ]
      )
