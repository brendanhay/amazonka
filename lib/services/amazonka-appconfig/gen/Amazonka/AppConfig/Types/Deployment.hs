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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Deployment where

import Amazonka.AppConfig.Types.AppliedExtension
import Amazonka.AppConfig.Types.DeploymentEvent
import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.GrowthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The ID of the application that was deployed.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | A list of extensions that were processed as part of the deployment. The
    -- extensions that were previously associated to the configuration profile,
    -- environment, or the application when @StartDeployment@ was called.
    appliedExtensions :: Prelude.Maybe [AppliedExtension],
    -- | The time the deployment completed.
    completedAt :: Prelude.Maybe Data.ISO8601,
    -- | Information about the source location of the configuration.
    configurationLocationUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the configuration profile that was deployed.
    configurationProfileId :: Prelude.Maybe Prelude.Text,
    -- | The configuration version that was deployed.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The ID of the deployment strategy that was deployed.
    deploymentStrategyId :: Prelude.Maybe Prelude.Text,
    -- | The description of the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment that was deployed.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | A list containing all events related to a deployment. The most recent
    -- events are displayed first.
    eventLog :: Prelude.Maybe [DeploymentEvent],
    -- | The amount of time that AppConfig monitored for alarms before
    -- considering the deployment to be complete and no longer eligible for
    -- automatic rollback.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The algorithm used to define how percentage grew over time.
    growthType :: Prelude.Maybe GrowthType,
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | The time the deployment started.
    startedAt :: Prelude.Maybe Data.ISO8601,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState
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
-- 'applicationId', 'deployment_applicationId' - The ID of the application that was deployed.
--
-- 'appliedExtensions', 'deployment_appliedExtensions' - A list of extensions that were processed as part of the deployment. The
-- extensions that were previously associated to the configuration profile,
-- environment, or the application when @StartDeployment@ was called.
--
-- 'completedAt', 'deployment_completedAt' - The time the deployment completed.
--
-- 'configurationLocationUri', 'deployment_configurationLocationUri' - Information about the source location of the configuration.
--
-- 'configurationName', 'deployment_configurationName' - The name of the configuration.
--
-- 'configurationProfileId', 'deployment_configurationProfileId' - The ID of the configuration profile that was deployed.
--
-- 'configurationVersion', 'deployment_configurationVersion' - The configuration version that was deployed.
--
-- 'deploymentDurationInMinutes', 'deployment_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'deploymentNumber', 'deployment_deploymentNumber' - The sequence number of the deployment.
--
-- 'deploymentStrategyId', 'deployment_deploymentStrategyId' - The ID of the deployment strategy that was deployed.
--
-- 'description', 'deployment_description' - The description of the deployment.
--
-- 'environmentId', 'deployment_environmentId' - The ID of the environment that was deployed.
--
-- 'eventLog', 'deployment_eventLog' - A list containing all events related to a deployment. The most recent
-- events are displayed first.
--
-- 'finalBakeTimeInMinutes', 'deployment_finalBakeTimeInMinutes' - The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
--
-- 'growthFactor', 'deployment_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'growthType', 'deployment_growthType' - The algorithm used to define how percentage grew over time.
--
-- 'percentageComplete', 'deployment_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'startedAt', 'deployment_startedAt' - The time the deployment started.
--
-- 'state', 'deployment_state' - The state of the deployment.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { applicationId = Prelude.Nothing,
      appliedExtensions = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      configurationLocationUri = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      configurationProfileId = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      deploymentStrategyId = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      eventLog = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      growthFactor = Prelude.Nothing,
      growthType = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The ID of the application that was deployed.
deployment_applicationId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_applicationId = Lens.lens (\Deployment' {applicationId} -> applicationId) (\s@Deployment' {} a -> s {applicationId = a} :: Deployment)

-- | A list of extensions that were processed as part of the deployment. The
-- extensions that were previously associated to the configuration profile,
-- environment, or the application when @StartDeployment@ was called.
deployment_appliedExtensions :: Lens.Lens' Deployment (Prelude.Maybe [AppliedExtension])
deployment_appliedExtensions = Lens.lens (\Deployment' {appliedExtensions} -> appliedExtensions) (\s@Deployment' {} a -> s {appliedExtensions = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The time the deployment completed.
deployment_completedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_completedAt = Lens.lens (\Deployment' {completedAt} -> completedAt) (\s@Deployment' {} a -> s {completedAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | Information about the source location of the configuration.
deployment_configurationLocationUri :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationLocationUri = Lens.lens (\Deployment' {configurationLocationUri} -> configurationLocationUri) (\s@Deployment' {} a -> s {configurationLocationUri = a} :: Deployment)

-- | The name of the configuration.
deployment_configurationName :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationName = Lens.lens (\Deployment' {configurationName} -> configurationName) (\s@Deployment' {} a -> s {configurationName = a} :: Deployment)

-- | The ID of the configuration profile that was deployed.
deployment_configurationProfileId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationProfileId = Lens.lens (\Deployment' {configurationProfileId} -> configurationProfileId) (\s@Deployment' {} a -> s {configurationProfileId = a} :: Deployment)

-- | The configuration version that was deployed.
deployment_configurationVersion :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationVersion = Lens.lens (\Deployment' {configurationVersion} -> configurationVersion) (\s@Deployment' {} a -> s {configurationVersion = a} :: Deployment)

-- | Total amount of time the deployment lasted.
deployment_deploymentDurationInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_deploymentDurationInMinutes = Lens.lens (\Deployment' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@Deployment' {} a -> s {deploymentDurationInMinutes = a} :: Deployment)

-- | The sequence number of the deployment.
deployment_deploymentNumber :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_deploymentNumber = Lens.lens (\Deployment' {deploymentNumber} -> deploymentNumber) (\s@Deployment' {} a -> s {deploymentNumber = a} :: Deployment)

-- | The ID of the deployment strategy that was deployed.
deployment_deploymentStrategyId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentStrategyId = Lens.lens (\Deployment' {deploymentStrategyId} -> deploymentStrategyId) (\s@Deployment' {} a -> s {deploymentStrategyId = a} :: Deployment)

-- | The description of the deployment.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

-- | The ID of the environment that was deployed.
deployment_environmentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_environmentId = Lens.lens (\Deployment' {environmentId} -> environmentId) (\s@Deployment' {} a -> s {environmentId = a} :: Deployment)

-- | A list containing all events related to a deployment. The most recent
-- events are displayed first.
deployment_eventLog :: Lens.Lens' Deployment (Prelude.Maybe [DeploymentEvent])
deployment_eventLog = Lens.lens (\Deployment' {eventLog} -> eventLog) (\s@Deployment' {} a -> s {eventLog = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
deployment_finalBakeTimeInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_finalBakeTimeInMinutes = Lens.lens (\Deployment' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@Deployment' {} a -> s {finalBakeTimeInMinutes = a} :: Deployment)

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deployment_growthFactor :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_growthFactor = Lens.lens (\Deployment' {growthFactor} -> growthFactor) (\s@Deployment' {} a -> s {growthFactor = a} :: Deployment)

-- | The algorithm used to define how percentage grew over time.
deployment_growthType :: Lens.Lens' Deployment (Prelude.Maybe GrowthType)
deployment_growthType = Lens.lens (\Deployment' {growthType} -> growthType) (\s@Deployment' {} a -> s {growthType = a} :: Deployment)

-- | The percentage of targets for which the deployment is available.
deployment_percentageComplete :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_percentageComplete = Lens.lens (\Deployment' {percentageComplete} -> percentageComplete) (\s@Deployment' {} a -> s {percentageComplete = a} :: Deployment)

-- | The time the deployment started.
deployment_startedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_startedAt = Lens.lens (\Deployment' {startedAt} -> startedAt) (\s@Deployment' {} a -> s {startedAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The state of the deployment.
deployment_state :: Lens.Lens' Deployment (Prelude.Maybe DeploymentState)
deployment_state = Lens.lens (\Deployment' {state} -> state) (\s@Deployment' {} a -> s {state = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> ( x
                            Data..:? "AppliedExtensions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CompletedAt")
            Prelude.<*> (x Data..:? "ConfigurationLocationUri")
            Prelude.<*> (x Data..:? "ConfigurationName")
            Prelude.<*> (x Data..:? "ConfigurationProfileId")
            Prelude.<*> (x Data..:? "ConfigurationVersion")
            Prelude.<*> (x Data..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Data..:? "DeploymentNumber")
            Prelude.<*> (x Data..:? "DeploymentStrategyId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "EventLog" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Data..:? "GrowthFactor")
            Prelude.<*> (x Data..:? "GrowthType")
            Prelude.<*> (x Data..:? "PercentageComplete")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` appliedExtensions
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` configurationLocationUri
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` configurationProfileId
      `Prelude.hashWithSalt` configurationVersion
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` deploymentNumber
      `Prelude.hashWithSalt` deploymentStrategyId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` eventLog
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` growthFactor
      `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` state

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf appliedExtensions
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf configurationLocationUri
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf configurationVersion
      `Prelude.seq` Prelude.rnf deploymentDurationInMinutes
      `Prelude.seq` Prelude.rnf deploymentNumber
      `Prelude.seq` Prelude.rnf deploymentStrategyId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf eventLog
      `Prelude.seq` Prelude.rnf finalBakeTimeInMinutes
      `Prelude.seq` Prelude.rnf growthFactor
      `Prelude.seq` Prelude.rnf growthType
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf state
