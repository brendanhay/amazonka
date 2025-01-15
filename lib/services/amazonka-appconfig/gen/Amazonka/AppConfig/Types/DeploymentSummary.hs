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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentSummary where

import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.GrowthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the deployment.
--
-- /See:/ 'newDeploymentSummary' smart constructor.
data DeploymentSummary = DeploymentSummary'
  { -- | Time the deployment completed.
    completedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The version of the configuration.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The amount of time that AppConfig monitors for alarms before considering
    -- the deployment to be complete and no longer eligible for automatic
    -- rollback.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The algorithm used to define how percentage grows over time.
    growthType :: Prelude.Maybe GrowthType,
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | Time the deployment started.
    startedAt :: Prelude.Maybe Data.ISO8601,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState
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
-- 'completedAt', 'deploymentSummary_completedAt' - Time the deployment completed.
--
-- 'configurationName', 'deploymentSummary_configurationName' - The name of the configuration.
--
-- 'configurationVersion', 'deploymentSummary_configurationVersion' - The version of the configuration.
--
-- 'deploymentDurationInMinutes', 'deploymentSummary_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'deploymentNumber', 'deploymentSummary_deploymentNumber' - The sequence number of the deployment.
--
-- 'finalBakeTimeInMinutes', 'deploymentSummary_finalBakeTimeInMinutes' - The amount of time that AppConfig monitors for alarms before considering
-- the deployment to be complete and no longer eligible for automatic
-- rollback.
--
-- 'growthFactor', 'deploymentSummary_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'growthType', 'deploymentSummary_growthType' - The algorithm used to define how percentage grows over time.
--
-- 'percentageComplete', 'deploymentSummary_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'startedAt', 'deploymentSummary_startedAt' - Time the deployment started.
--
-- 'state', 'deploymentSummary_state' - The state of the deployment.
newDeploymentSummary ::
  DeploymentSummary
newDeploymentSummary =
  DeploymentSummary'
    { completedAt = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      growthFactor = Prelude.Nothing,
      growthType = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | Time the deployment completed.
deploymentSummary_completedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_completedAt = Lens.lens (\DeploymentSummary' {completedAt} -> completedAt) (\s@DeploymentSummary' {} a -> s {completedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the configuration.
deploymentSummary_configurationName :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationName = Lens.lens (\DeploymentSummary' {configurationName} -> configurationName) (\s@DeploymentSummary' {} a -> s {configurationName = a} :: DeploymentSummary)

-- | The version of the configuration.
deploymentSummary_configurationVersion :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationVersion = Lens.lens (\DeploymentSummary' {configurationVersion} -> configurationVersion) (\s@DeploymentSummary' {} a -> s {configurationVersion = a} :: DeploymentSummary)

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

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deploymentSummary_growthFactor :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_growthFactor = Lens.lens (\DeploymentSummary' {growthFactor} -> growthFactor) (\s@DeploymentSummary' {} a -> s {growthFactor = a} :: DeploymentSummary)

-- | The algorithm used to define how percentage grows over time.
deploymentSummary_growthType :: Lens.Lens' DeploymentSummary (Prelude.Maybe GrowthType)
deploymentSummary_growthType = Lens.lens (\DeploymentSummary' {growthType} -> growthType) (\s@DeploymentSummary' {} a -> s {growthType = a} :: DeploymentSummary)

-- | The percentage of targets for which the deployment is available.
deploymentSummary_percentageComplete :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_percentageComplete = Lens.lens (\DeploymentSummary' {percentageComplete} -> percentageComplete) (\s@DeploymentSummary' {} a -> s {percentageComplete = a} :: DeploymentSummary)

-- | Time the deployment started.
deploymentSummary_startedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_startedAt = Lens.lens (\DeploymentSummary' {startedAt} -> startedAt) (\s@DeploymentSummary' {} a -> s {startedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Data._Time

-- | The state of the deployment.
deploymentSummary_state :: Lens.Lens' DeploymentSummary (Prelude.Maybe DeploymentState)
deploymentSummary_state = Lens.lens (\DeploymentSummary' {state} -> state) (\s@DeploymentSummary' {} a -> s {state = a} :: DeploymentSummary)

instance Data.FromJSON DeploymentSummary where
  parseJSON =
    Data.withObject
      "DeploymentSummary"
      ( \x ->
          DeploymentSummary'
            Prelude.<$> (x Data..:? "CompletedAt")
            Prelude.<*> (x Data..:? "ConfigurationName")
            Prelude.<*> (x Data..:? "ConfigurationVersion")
            Prelude.<*> (x Data..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Data..:? "DeploymentNumber")
            Prelude.<*> (x Data..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Data..:? "GrowthFactor")
            Prelude.<*> (x Data..:? "GrowthType")
            Prelude.<*> (x Data..:? "PercentageComplete")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable DeploymentSummary where
  hashWithSalt _salt DeploymentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` configurationVersion
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` deploymentNumber
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` growthFactor
      `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` state

instance Prelude.NFData DeploymentSummary where
  rnf DeploymentSummary' {..} =
    Prelude.rnf completedAt `Prelude.seq`
      Prelude.rnf configurationName `Prelude.seq`
        Prelude.rnf configurationVersion `Prelude.seq`
          Prelude.rnf deploymentDurationInMinutes `Prelude.seq`
            Prelude.rnf deploymentNumber `Prelude.seq`
              Prelude.rnf finalBakeTimeInMinutes `Prelude.seq`
                Prelude.rnf growthFactor `Prelude.seq`
                  Prelude.rnf growthType `Prelude.seq`
                    Prelude.rnf percentageComplete `Prelude.seq`
                      Prelude.rnf startedAt `Prelude.seq`
                        Prelude.rnf state
