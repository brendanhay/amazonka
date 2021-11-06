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
-- Module      : Amazonka.AppConfig.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Deployment where

import Amazonka.AppConfig.Types.DeploymentEvent
import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.GrowthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState,
    -- | The ID of the deployment strategy that was deployed.
    deploymentStrategyId :: Prelude.Maybe Prelude.Text,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The configuration version that was deployed.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | A list containing all events related to a deployment. The most recent
    -- events are displayed first.
    eventLog :: Prelude.Maybe [DeploymentEvent],
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | The time the deployment started.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the application that was deployed.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the environment that was deployed.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The time the deployment completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | Information about the source location of the configuration.
    configurationLocationUri :: Prelude.Maybe Prelude.Text,
    -- | The amount of time AppConfig monitored for alarms before considering the
    -- deployment to be complete and no longer eligible for automatic roll
    -- back.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The description of the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the configuration profile that was deployed.
    configurationProfileId :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to define how percentage grew over time.
    growthType :: Prelude.Maybe GrowthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'growthFactor', 'deployment_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'configurationName', 'deployment_configurationName' - The name of the configuration.
--
-- 'state', 'deployment_state' - The state of the deployment.
--
-- 'deploymentStrategyId', 'deployment_deploymentStrategyId' - The ID of the deployment strategy that was deployed.
--
-- 'deploymentNumber', 'deployment_deploymentNumber' - The sequence number of the deployment.
--
-- 'configurationVersion', 'deployment_configurationVersion' - The configuration version that was deployed.
--
-- 'eventLog', 'deployment_eventLog' - A list containing all events related to a deployment. The most recent
-- events are displayed first.
--
-- 'percentageComplete', 'deployment_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'startedAt', 'deployment_startedAt' - The time the deployment started.
--
-- 'applicationId', 'deployment_applicationId' - The ID of the application that was deployed.
--
-- 'deploymentDurationInMinutes', 'deployment_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'environmentId', 'deployment_environmentId' - The ID of the environment that was deployed.
--
-- 'completedAt', 'deployment_completedAt' - The time the deployment completed.
--
-- 'configurationLocationUri', 'deployment_configurationLocationUri' - Information about the source location of the configuration.
--
-- 'finalBakeTimeInMinutes', 'deployment_finalBakeTimeInMinutes' - The amount of time AppConfig monitored for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
--
-- 'description', 'deployment_description' - The description of the deployment.
--
-- 'configurationProfileId', 'deployment_configurationProfileId' - The ID of the configuration profile that was deployed.
--
-- 'growthType', 'deployment_growthType' - The algorithm used to define how percentage grew over time.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { growthFactor = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      state = Prelude.Nothing,
      deploymentStrategyId = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      eventLog = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      configurationLocationUri = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      description = Prelude.Nothing,
      configurationProfileId = Prelude.Nothing,
      growthType = Prelude.Nothing
    }

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deployment_growthFactor :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_growthFactor = Lens.lens (\Deployment' {growthFactor} -> growthFactor) (\s@Deployment' {} a -> s {growthFactor = a} :: Deployment)

-- | The name of the configuration.
deployment_configurationName :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationName = Lens.lens (\Deployment' {configurationName} -> configurationName) (\s@Deployment' {} a -> s {configurationName = a} :: Deployment)

-- | The state of the deployment.
deployment_state :: Lens.Lens' Deployment (Prelude.Maybe DeploymentState)
deployment_state = Lens.lens (\Deployment' {state} -> state) (\s@Deployment' {} a -> s {state = a} :: Deployment)

-- | The ID of the deployment strategy that was deployed.
deployment_deploymentStrategyId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentStrategyId = Lens.lens (\Deployment' {deploymentStrategyId} -> deploymentStrategyId) (\s@Deployment' {} a -> s {deploymentStrategyId = a} :: Deployment)

-- | The sequence number of the deployment.
deployment_deploymentNumber :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_deploymentNumber = Lens.lens (\Deployment' {deploymentNumber} -> deploymentNumber) (\s@Deployment' {} a -> s {deploymentNumber = a} :: Deployment)

-- | The configuration version that was deployed.
deployment_configurationVersion :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationVersion = Lens.lens (\Deployment' {configurationVersion} -> configurationVersion) (\s@Deployment' {} a -> s {configurationVersion = a} :: Deployment)

-- | A list containing all events related to a deployment. The most recent
-- events are displayed first.
deployment_eventLog :: Lens.Lens' Deployment (Prelude.Maybe [DeploymentEvent])
deployment_eventLog = Lens.lens (\Deployment' {eventLog} -> eventLog) (\s@Deployment' {} a -> s {eventLog = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of targets for which the deployment is available.
deployment_percentageComplete :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_percentageComplete = Lens.lens (\Deployment' {percentageComplete} -> percentageComplete) (\s@Deployment' {} a -> s {percentageComplete = a} :: Deployment)

-- | The time the deployment started.
deployment_startedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_startedAt = Lens.lens (\Deployment' {startedAt} -> startedAt) (\s@Deployment' {} a -> s {startedAt = a} :: Deployment) Prelude.. Lens.mapping Core._Time

-- | The ID of the application that was deployed.
deployment_applicationId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_applicationId = Lens.lens (\Deployment' {applicationId} -> applicationId) (\s@Deployment' {} a -> s {applicationId = a} :: Deployment)

-- | Total amount of time the deployment lasted.
deployment_deploymentDurationInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_deploymentDurationInMinutes = Lens.lens (\Deployment' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@Deployment' {} a -> s {deploymentDurationInMinutes = a} :: Deployment)

-- | The ID of the environment that was deployed.
deployment_environmentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_environmentId = Lens.lens (\Deployment' {environmentId} -> environmentId) (\s@Deployment' {} a -> s {environmentId = a} :: Deployment)

-- | The time the deployment completed.
deployment_completedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_completedAt = Lens.lens (\Deployment' {completedAt} -> completedAt) (\s@Deployment' {} a -> s {completedAt = a} :: Deployment) Prelude.. Lens.mapping Core._Time

-- | Information about the source location of the configuration.
deployment_configurationLocationUri :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationLocationUri = Lens.lens (\Deployment' {configurationLocationUri} -> configurationLocationUri) (\s@Deployment' {} a -> s {configurationLocationUri = a} :: Deployment)

-- | The amount of time AppConfig monitored for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
deployment_finalBakeTimeInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_finalBakeTimeInMinutes = Lens.lens (\Deployment' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@Deployment' {} a -> s {finalBakeTimeInMinutes = a} :: Deployment)

-- | The description of the deployment.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

-- | The ID of the configuration profile that was deployed.
deployment_configurationProfileId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationProfileId = Lens.lens (\Deployment' {configurationProfileId} -> configurationProfileId) (\s@Deployment' {} a -> s {configurationProfileId = a} :: Deployment)

-- | The algorithm used to define how percentage grew over time.
deployment_growthType :: Lens.Lens' Deployment (Prelude.Maybe GrowthType)
deployment_growthType = Lens.lens (\Deployment' {growthType} -> growthType) (\s@Deployment' {} a -> s {growthType = a} :: Deployment)

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Core..:? "GrowthFactor")
            Prelude.<*> (x Core..:? "ConfigurationName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DeploymentStrategyId")
            Prelude.<*> (x Core..:? "DeploymentNumber")
            Prelude.<*> (x Core..:? "ConfigurationVersion")
            Prelude.<*> (x Core..:? "EventLog" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PercentageComplete")
            Prelude.<*> (x Core..:? "StartedAt")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Core..:? "EnvironmentId")
            Prelude.<*> (x Core..:? "CompletedAt")
            Prelude.<*> (x Core..:? "ConfigurationLocationUri")
            Prelude.<*> (x Core..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ConfigurationProfileId")
            Prelude.<*> (x Core..:? "GrowthType")
      )

instance Prelude.Hashable Deployment

instance Prelude.NFData Deployment
