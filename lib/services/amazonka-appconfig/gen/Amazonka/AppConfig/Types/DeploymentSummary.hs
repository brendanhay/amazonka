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
-- Module      : Amazonka.AppConfig.Types.DeploymentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentSummary where

import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.GrowthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the deployment.
--
-- /See:/ 'newDeploymentSummary' smart constructor.
data DeploymentSummary = DeploymentSummary'
  { -- | The algorithm used to define how percentage grows over time.
    growthType :: Prelude.Maybe GrowthType,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The amount of time that AppConfig monitors for alarms before considering
    -- the deployment to be complete and no longer eligible for automatic
    -- rollback.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Time the deployment started.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The version of the configuration.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | Time the deployment completed.
    completedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'growthType', 'deploymentSummary_growthType' - The algorithm used to define how percentage grows over time.
--
-- 'state', 'deploymentSummary_state' - The state of the deployment.
--
-- 'deploymentDurationInMinutes', 'deploymentSummary_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'deploymentNumber', 'deploymentSummary_deploymentNumber' - The sequence number of the deployment.
--
-- 'finalBakeTimeInMinutes', 'deploymentSummary_finalBakeTimeInMinutes' - The amount of time that AppConfig monitors for alarms before considering
-- the deployment to be complete and no longer eligible for automatic
-- rollback.
--
-- 'startedAt', 'deploymentSummary_startedAt' - Time the deployment started.
--
-- 'configurationName', 'deploymentSummary_configurationName' - The name of the configuration.
--
-- 'growthFactor', 'deploymentSummary_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'configurationVersion', 'deploymentSummary_configurationVersion' - The version of the configuration.
--
-- 'percentageComplete', 'deploymentSummary_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'completedAt', 'deploymentSummary_completedAt' - Time the deployment completed.
newDeploymentSummary ::
  DeploymentSummary
newDeploymentSummary =
  DeploymentSummary'
    { growthType = Prelude.Nothing,
      state = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      growthFactor = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      completedAt = Prelude.Nothing
    }

-- | The algorithm used to define how percentage grows over time.
deploymentSummary_growthType :: Lens.Lens' DeploymentSummary (Prelude.Maybe GrowthType)
deploymentSummary_growthType = Lens.lens (\DeploymentSummary' {growthType} -> growthType) (\s@DeploymentSummary' {} a -> s {growthType = a} :: DeploymentSummary)

-- | The state of the deployment.
deploymentSummary_state :: Lens.Lens' DeploymentSummary (Prelude.Maybe DeploymentState)
deploymentSummary_state = Lens.lens (\DeploymentSummary' {state} -> state) (\s@DeploymentSummary' {} a -> s {state = a} :: DeploymentSummary)

-- | Total amount of time the deployment lasted.
deploymentSummary_deploymentDurationInMinutes :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Natural)
deploymentSummary_deploymentDurationInMinutes = Lens.lens (\DeploymentSummary' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@DeploymentSummary' {} a -> s {deploymentDurationInMinutes = a} :: DeploymentSummary)

-- | The sequence number of the deployment.
deploymentSummary_deploymentNumber :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Int)
deploymentSummary_deploymentNumber = Lens.lens (\DeploymentSummary' {deploymentNumber} -> deploymentNumber) (\s@DeploymentSummary' {} a -> s {deploymentNumber = a} :: DeploymentSummary)

-- | The amount of time that AppConfig monitors for alarms before considering
-- the deployment to be complete and no longer eligible for automatic
-- rollback.
deploymentSummary_finalBakeTimeInMinutes :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Natural)
deploymentSummary_finalBakeTimeInMinutes = Lens.lens (\DeploymentSummary' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@DeploymentSummary' {} a -> s {finalBakeTimeInMinutes = a} :: DeploymentSummary)

-- | Time the deployment started.
deploymentSummary_startedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_startedAt = Lens.lens (\DeploymentSummary' {startedAt} -> startedAt) (\s@DeploymentSummary' {} a -> s {startedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the configuration.
deploymentSummary_configurationName :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationName = Lens.lens (\DeploymentSummary' {configurationName} -> configurationName) (\s@DeploymentSummary' {} a -> s {configurationName = a} :: DeploymentSummary)

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deploymentSummary_growthFactor :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_growthFactor = Lens.lens (\DeploymentSummary' {growthFactor} -> growthFactor) (\s@DeploymentSummary' {} a -> s {growthFactor = a} :: DeploymentSummary)

-- | The version of the configuration.
deploymentSummary_configurationVersion :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationVersion = Lens.lens (\DeploymentSummary' {configurationVersion} -> configurationVersion) (\s@DeploymentSummary' {} a -> s {configurationVersion = a} :: DeploymentSummary)

-- | The percentage of targets for which the deployment is available.
deploymentSummary_percentageComplete :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_percentageComplete = Lens.lens (\DeploymentSummary' {percentageComplete} -> percentageComplete) (\s@DeploymentSummary' {} a -> s {percentageComplete = a} :: DeploymentSummary)

-- | Time the deployment completed.
deploymentSummary_completedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_completedAt = Lens.lens (\DeploymentSummary' {completedAt} -> completedAt) (\s@DeploymentSummary' {} a -> s {completedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DeploymentSummary where
  parseJSON =
    Core.withObject
      "DeploymentSummary"
      ( \x ->
          DeploymentSummary'
            Prelude.<$> (x Core..:? "GrowthType")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Core..:? "DeploymentNumber")
            Prelude.<*> (x Core..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Core..:? "StartedAt")
            Prelude.<*> (x Core..:? "ConfigurationName")
            Prelude.<*> (x Core..:? "GrowthFactor")
            Prelude.<*> (x Core..:? "ConfigurationVersion")
            Prelude.<*> (x Core..:? "PercentageComplete")
            Prelude.<*> (x Core..:? "CompletedAt")
      )

instance Prelude.Hashable DeploymentSummary where
  hashWithSalt _salt DeploymentSummary' {..} =
    _salt `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` deploymentNumber
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` growthFactor
      `Prelude.hashWithSalt` configurationVersion
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` completedAt

instance Prelude.NFData DeploymentSummary where
  rnf DeploymentSummary' {..} =
    Prelude.rnf growthType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf deploymentDurationInMinutes
      `Prelude.seq` Prelude.rnf deploymentNumber
      `Prelude.seq` Prelude.rnf finalBakeTimeInMinutes
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf growthFactor
      `Prelude.seq` Prelude.rnf configurationVersion
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf completedAt
