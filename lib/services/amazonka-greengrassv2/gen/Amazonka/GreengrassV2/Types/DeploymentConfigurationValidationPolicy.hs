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
-- Module      : Amazonka.GreengrassV2.Types.DeploymentConfigurationValidationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DeploymentConfigurationValidationPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about how long a component on a core device can
-- validate its configuration updates before it times out. Components can
-- use the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-subscribetovalidateconfigurationupdates SubscribeToValidateConfigurationUpdates>
-- IPC operation to receive notifications when a deployment specifies a
-- configuration update. Then, components can respond with the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-sendconfigurationvalidityreport SendConfigurationValidityReport>
-- IPC operation. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- /See:/ 'newDeploymentConfigurationValidationPolicy' smart constructor.
data DeploymentConfigurationValidationPolicy = DeploymentConfigurationValidationPolicy'
  { -- | The amount of time in seconds that a component can validate its
    -- configuration updates. If the validation time exceeds this timeout, then
    -- the deployment proceeds for the device.
    --
    -- Default: @30@
    timeoutInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfigurationValidationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutInSeconds', 'deploymentConfigurationValidationPolicy_timeoutInSeconds' - The amount of time in seconds that a component can validate its
-- configuration updates. If the validation time exceeds this timeout, then
-- the deployment proceeds for the device.
--
-- Default: @30@
newDeploymentConfigurationValidationPolicy ::
  DeploymentConfigurationValidationPolicy
newDeploymentConfigurationValidationPolicy =
  DeploymentConfigurationValidationPolicy'
    { timeoutInSeconds =
        Prelude.Nothing
    }

-- | The amount of time in seconds that a component can validate its
-- configuration updates. If the validation time exceeds this timeout, then
-- the deployment proceeds for the device.
--
-- Default: @30@
deploymentConfigurationValidationPolicy_timeoutInSeconds :: Lens.Lens' DeploymentConfigurationValidationPolicy (Prelude.Maybe Prelude.Int)
deploymentConfigurationValidationPolicy_timeoutInSeconds = Lens.lens (\DeploymentConfigurationValidationPolicy' {timeoutInSeconds} -> timeoutInSeconds) (\s@DeploymentConfigurationValidationPolicy' {} a -> s {timeoutInSeconds = a} :: DeploymentConfigurationValidationPolicy)

instance
  Data.FromJSON
    DeploymentConfigurationValidationPolicy
  where
  parseJSON =
    Data.withObject
      "DeploymentConfigurationValidationPolicy"
      ( \x ->
          DeploymentConfigurationValidationPolicy'
            Prelude.<$> (x Data..:? "timeoutInSeconds")
      )

instance
  Prelude.Hashable
    DeploymentConfigurationValidationPolicy
  where
  hashWithSalt
    _salt
    DeploymentConfigurationValidationPolicy' {..} =
      _salt `Prelude.hashWithSalt` timeoutInSeconds

instance
  Prelude.NFData
    DeploymentConfigurationValidationPolicy
  where
  rnf DeploymentConfigurationValidationPolicy' {..} =
    Prelude.rnf timeoutInSeconds

instance
  Data.ToJSON
    DeploymentConfigurationValidationPolicy
  where
  toJSON DeploymentConfigurationValidationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds
          ]
      )
