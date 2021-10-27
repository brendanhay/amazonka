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
-- Module      : Network.AWS.AppConfig.Types.DeploymentStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppConfig.Types.DeploymentStrategy where

import Network.AWS.AppConfig.Types.GrowthType
import Network.AWS.AppConfig.Types.ReplicateTo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newDeploymentStrategy' smart constructor.
data DeploymentStrategy = DeploymentStrategy'
  { -- | The percentage of targets that received a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | Save the deployment strategy to a Systems Manager (SSM) document.
    replicateTo :: Prelude.Maybe ReplicateTo,
    -- | The name of the deployment strategy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The deployment strategy ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time AppConfig monitored for alarms before considering the
    -- deployment to be complete and no longer eligible for automatic roll
    -- back.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The description of the deployment strategy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to define how percentage grew over time.
    growthType :: Prelude.Maybe GrowthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'growthFactor', 'deploymentStrategy_growthFactor' - The percentage of targets that received a deployed configuration during
-- each interval.
--
-- 'replicateTo', 'deploymentStrategy_replicateTo' - Save the deployment strategy to a Systems Manager (SSM) document.
--
-- 'name', 'deploymentStrategy_name' - The name of the deployment strategy.
--
-- 'id', 'deploymentStrategy_id' - The deployment strategy ID.
--
-- 'deploymentDurationInMinutes', 'deploymentStrategy_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'finalBakeTimeInMinutes', 'deploymentStrategy_finalBakeTimeInMinutes' - The amount of time AppConfig monitored for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
--
-- 'description', 'deploymentStrategy_description' - The description of the deployment strategy.
--
-- 'growthType', 'deploymentStrategy_growthType' - The algorithm used to define how percentage grew over time.
newDeploymentStrategy ::
  DeploymentStrategy
newDeploymentStrategy =
  DeploymentStrategy'
    { growthFactor = Prelude.Nothing,
      replicateTo = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      description = Prelude.Nothing,
      growthType = Prelude.Nothing
    }

-- | The percentage of targets that received a deployed configuration during
-- each interval.
deploymentStrategy_growthFactor :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Double)
deploymentStrategy_growthFactor = Lens.lens (\DeploymentStrategy' {growthFactor} -> growthFactor) (\s@DeploymentStrategy' {} a -> s {growthFactor = a} :: DeploymentStrategy)

-- | Save the deployment strategy to a Systems Manager (SSM) document.
deploymentStrategy_replicateTo :: Lens.Lens' DeploymentStrategy (Prelude.Maybe ReplicateTo)
deploymentStrategy_replicateTo = Lens.lens (\DeploymentStrategy' {replicateTo} -> replicateTo) (\s@DeploymentStrategy' {} a -> s {replicateTo = a} :: DeploymentStrategy)

-- | The name of the deployment strategy.
deploymentStrategy_name :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_name = Lens.lens (\DeploymentStrategy' {name} -> name) (\s@DeploymentStrategy' {} a -> s {name = a} :: DeploymentStrategy)

-- | The deployment strategy ID.
deploymentStrategy_id :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_id = Lens.lens (\DeploymentStrategy' {id} -> id) (\s@DeploymentStrategy' {} a -> s {id = a} :: DeploymentStrategy)

-- | Total amount of time the deployment lasted.
deploymentStrategy_deploymentDurationInMinutes :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Natural)
deploymentStrategy_deploymentDurationInMinutes = Lens.lens (\DeploymentStrategy' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@DeploymentStrategy' {} a -> s {deploymentDurationInMinutes = a} :: DeploymentStrategy)

-- | The amount of time AppConfig monitored for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
deploymentStrategy_finalBakeTimeInMinutes :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Natural)
deploymentStrategy_finalBakeTimeInMinutes = Lens.lens (\DeploymentStrategy' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@DeploymentStrategy' {} a -> s {finalBakeTimeInMinutes = a} :: DeploymentStrategy)

-- | The description of the deployment strategy.
deploymentStrategy_description :: Lens.Lens' DeploymentStrategy (Prelude.Maybe Prelude.Text)
deploymentStrategy_description = Lens.lens (\DeploymentStrategy' {description} -> description) (\s@DeploymentStrategy' {} a -> s {description = a} :: DeploymentStrategy)

-- | The algorithm used to define how percentage grew over time.
deploymentStrategy_growthType :: Lens.Lens' DeploymentStrategy (Prelude.Maybe GrowthType)
deploymentStrategy_growthType = Lens.lens (\DeploymentStrategy' {growthType} -> growthType) (\s@DeploymentStrategy' {} a -> s {growthType = a} :: DeploymentStrategy)

instance Core.FromJSON DeploymentStrategy where
  parseJSON =
    Core.withObject
      "DeploymentStrategy"
      ( \x ->
          DeploymentStrategy'
            Prelude.<$> (x Core..:? "GrowthFactor")
            Prelude.<*> (x Core..:? "ReplicateTo")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Core..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "GrowthType")
      )

instance Prelude.Hashable DeploymentStrategy

instance Prelude.NFData DeploymentStrategy
