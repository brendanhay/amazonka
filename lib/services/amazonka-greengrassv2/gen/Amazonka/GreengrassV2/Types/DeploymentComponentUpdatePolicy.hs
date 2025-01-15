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
-- Module      : Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicyAction
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a deployment\'s policy that defines when
-- components are safe to update.
--
-- Each component on a device can report whether or not it\'s ready to
-- update. After a component and its dependencies are ready, they can apply
-- the update in the deployment. You can configure whether or not the
-- deployment notifies components of an update and waits for a response.
-- You specify the amount of time each component has to respond to the
-- update notification.
--
-- /See:/ 'newDeploymentComponentUpdatePolicy' smart constructor.
data DeploymentComponentUpdatePolicy = DeploymentComponentUpdatePolicy'
  { -- | Whether or not to notify components and wait for components to become
    -- safe to update. Choose from the following options:
    --
    -- -   @NOTIFY_COMPONENTS@ – The deployment notifies each component before
    --     it stops and updates that component. Components can use the
    --     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-subscribetocomponentupdates SubscribeToComponentUpdates>
    --     IPC operation to receive these notifications. Then, components can
    --     respond with the
    --     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-defercomponentupdate DeferComponentUpdate>
    --     IPC operation. For more information, see
    --     <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
    --     in the /IoT Greengrass V2 Developer Guide/.
    --
    -- -   @SKIP_NOTIFY_COMPONENTS@ – The deployment doesn\'t notify components
    --     or wait for them to be safe to update.
    --
    -- Default: @NOTIFY_COMPONENTS@
    action :: Prelude.Maybe DeploymentComponentUpdatePolicyAction,
    -- | The amount of time in seconds that each component on a device has to
    -- report that it\'s safe to update. If the component waits for longer than
    -- this timeout, then the deployment proceeds on the device.
    --
    -- Default: @60@
    timeoutInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentComponentUpdatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'deploymentComponentUpdatePolicy_action' - Whether or not to notify components and wait for components to become
-- safe to update. Choose from the following options:
--
-- -   @NOTIFY_COMPONENTS@ – The deployment notifies each component before
--     it stops and updates that component. Components can use the
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-subscribetocomponentupdates SubscribeToComponentUpdates>
--     IPC operation to receive these notifications. Then, components can
--     respond with the
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-defercomponentupdate DeferComponentUpdate>
--     IPC operation. For more information, see
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
--     in the /IoT Greengrass V2 Developer Guide/.
--
-- -   @SKIP_NOTIFY_COMPONENTS@ – The deployment doesn\'t notify components
--     or wait for them to be safe to update.
--
-- Default: @NOTIFY_COMPONENTS@
--
-- 'timeoutInSeconds', 'deploymentComponentUpdatePolicy_timeoutInSeconds' - The amount of time in seconds that each component on a device has to
-- report that it\'s safe to update. If the component waits for longer than
-- this timeout, then the deployment proceeds on the device.
--
-- Default: @60@
newDeploymentComponentUpdatePolicy ::
  DeploymentComponentUpdatePolicy
newDeploymentComponentUpdatePolicy =
  DeploymentComponentUpdatePolicy'
    { action =
        Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | Whether or not to notify components and wait for components to become
-- safe to update. Choose from the following options:
--
-- -   @NOTIFY_COMPONENTS@ – The deployment notifies each component before
--     it stops and updates that component. Components can use the
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-subscribetocomponentupdates SubscribeToComponentUpdates>
--     IPC operation to receive these notifications. Then, components can
--     respond with the
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/interprocess-communication.html#ipc-operation-defercomponentupdate DeferComponentUpdate>
--     IPC operation. For more information, see
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/create-deployments.html Create deployments>
--     in the /IoT Greengrass V2 Developer Guide/.
--
-- -   @SKIP_NOTIFY_COMPONENTS@ – The deployment doesn\'t notify components
--     or wait for them to be safe to update.
--
-- Default: @NOTIFY_COMPONENTS@
deploymentComponentUpdatePolicy_action :: Lens.Lens' DeploymentComponentUpdatePolicy (Prelude.Maybe DeploymentComponentUpdatePolicyAction)
deploymentComponentUpdatePolicy_action = Lens.lens (\DeploymentComponentUpdatePolicy' {action} -> action) (\s@DeploymentComponentUpdatePolicy' {} a -> s {action = a} :: DeploymentComponentUpdatePolicy)

-- | The amount of time in seconds that each component on a device has to
-- report that it\'s safe to update. If the component waits for longer than
-- this timeout, then the deployment proceeds on the device.
--
-- Default: @60@
deploymentComponentUpdatePolicy_timeoutInSeconds :: Lens.Lens' DeploymentComponentUpdatePolicy (Prelude.Maybe Prelude.Int)
deploymentComponentUpdatePolicy_timeoutInSeconds = Lens.lens (\DeploymentComponentUpdatePolicy' {timeoutInSeconds} -> timeoutInSeconds) (\s@DeploymentComponentUpdatePolicy' {} a -> s {timeoutInSeconds = a} :: DeploymentComponentUpdatePolicy)

instance
  Data.FromJSON
    DeploymentComponentUpdatePolicy
  where
  parseJSON =
    Data.withObject
      "DeploymentComponentUpdatePolicy"
      ( \x ->
          DeploymentComponentUpdatePolicy'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "timeoutInSeconds")
      )

instance
  Prelude.Hashable
    DeploymentComponentUpdatePolicy
  where
  hashWithSalt
    _salt
    DeploymentComponentUpdatePolicy' {..} =
      _salt
        `Prelude.hashWithSalt` action
        `Prelude.hashWithSalt` timeoutInSeconds

instance
  Prelude.NFData
    DeploymentComponentUpdatePolicy
  where
  rnf DeploymentComponentUpdatePolicy' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf timeoutInSeconds

instance Data.ToJSON DeploymentComponentUpdatePolicy where
  toJSON DeploymentComponentUpdatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("action" Data..=) Prelude.<$> action,
            ("timeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds
          ]
      )
