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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentReadyOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentReadyOption where

import Amazonka.CodeDeploy.Types.DeploymentReadyAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about how traffic is rerouted to instances in a replacement
-- environment in a blue\/green deployment.
--
-- /See:/ 'newDeploymentReadyOption' smart constructor.
data DeploymentReadyOption = DeploymentReadyOption'
  { -- | The number of minutes to wait before the status of a blue\/green
    -- deployment is changed to Stopped if rerouting is not started manually.
    -- Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@.
    waitTimeInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Information about when to reroute traffic from an original environment
    -- to a replacement environment in a blue\/green deployment.
    --
    -- -   CONTINUE_DEPLOYMENT: Register new instances with the load balancer
    --     immediately after the new application revision is installed on the
    --     instances in the replacement environment.
    --
    -- -   STOP_DEPLOYMENT: Do not register new instances with a load balancer
    --     unless traffic rerouting is started using ContinueDeployment. If
    --     traffic rerouting is not started before the end of the specified
    --     wait period, the deployment status is changed to Stopped.
    actionOnTimeout :: Prelude.Maybe DeploymentReadyAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentReadyOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waitTimeInMinutes', 'deploymentReadyOption_waitTimeInMinutes' - The number of minutes to wait before the status of a blue\/green
-- deployment is changed to Stopped if rerouting is not started manually.
-- Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@.
--
-- 'actionOnTimeout', 'deploymentReadyOption_actionOnTimeout' - Information about when to reroute traffic from an original environment
-- to a replacement environment in a blue\/green deployment.
--
-- -   CONTINUE_DEPLOYMENT: Register new instances with the load balancer
--     immediately after the new application revision is installed on the
--     instances in the replacement environment.
--
-- -   STOP_DEPLOYMENT: Do not register new instances with a load balancer
--     unless traffic rerouting is started using ContinueDeployment. If
--     traffic rerouting is not started before the end of the specified
--     wait period, the deployment status is changed to Stopped.
newDeploymentReadyOption ::
  DeploymentReadyOption
newDeploymentReadyOption =
  DeploymentReadyOption'
    { waitTimeInMinutes =
        Prelude.Nothing,
      actionOnTimeout = Prelude.Nothing
    }

-- | The number of minutes to wait before the status of a blue\/green
-- deployment is changed to Stopped if rerouting is not started manually.
-- Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@.
deploymentReadyOption_waitTimeInMinutes :: Lens.Lens' DeploymentReadyOption (Prelude.Maybe Prelude.Int)
deploymentReadyOption_waitTimeInMinutes = Lens.lens (\DeploymentReadyOption' {waitTimeInMinutes} -> waitTimeInMinutes) (\s@DeploymentReadyOption' {} a -> s {waitTimeInMinutes = a} :: DeploymentReadyOption)

-- | Information about when to reroute traffic from an original environment
-- to a replacement environment in a blue\/green deployment.
--
-- -   CONTINUE_DEPLOYMENT: Register new instances with the load balancer
--     immediately after the new application revision is installed on the
--     instances in the replacement environment.
--
-- -   STOP_DEPLOYMENT: Do not register new instances with a load balancer
--     unless traffic rerouting is started using ContinueDeployment. If
--     traffic rerouting is not started before the end of the specified
--     wait period, the deployment status is changed to Stopped.
deploymentReadyOption_actionOnTimeout :: Lens.Lens' DeploymentReadyOption (Prelude.Maybe DeploymentReadyAction)
deploymentReadyOption_actionOnTimeout = Lens.lens (\DeploymentReadyOption' {actionOnTimeout} -> actionOnTimeout) (\s@DeploymentReadyOption' {} a -> s {actionOnTimeout = a} :: DeploymentReadyOption)

instance Core.FromJSON DeploymentReadyOption where
  parseJSON =
    Core.withObject
      "DeploymentReadyOption"
      ( \x ->
          DeploymentReadyOption'
            Prelude.<$> (x Core..:? "waitTimeInMinutes")
            Prelude.<*> (x Core..:? "actionOnTimeout")
      )

instance Prelude.Hashable DeploymentReadyOption where
  hashWithSalt _salt DeploymentReadyOption' {..} =
    _salt `Prelude.hashWithSalt` waitTimeInMinutes
      `Prelude.hashWithSalt` actionOnTimeout

instance Prelude.NFData DeploymentReadyOption where
  rnf DeploymentReadyOption' {..} =
    Prelude.rnf waitTimeInMinutes
      `Prelude.seq` Prelude.rnf actionOnTimeout

instance Core.ToJSON DeploymentReadyOption where
  toJSON DeploymentReadyOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("waitTimeInMinutes" Core..=)
              Prelude.<$> waitTimeInMinutes,
            ("actionOnTimeout" Core..=)
              Prelude.<$> actionOnTimeout
          ]
      )
