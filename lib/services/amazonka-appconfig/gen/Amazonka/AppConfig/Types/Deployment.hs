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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The ID of the deployment strategy that was deployed.
    deploymentStrategyId :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to define how percentage grew over time.
    growthType :: Prelude.Maybe GrowthType,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The description of the deployment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that AppConfig monitored for alarms before
    -- considering the deployment to be complete and no longer eligible for
    -- automatic rollback.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The time the deployment started.
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | A list of extensions that were processed as part of the deployment. The
    -- extensions that were previously associated to the configuration profile,
    -- environment, or the application when @StartDeployment@ was called.
    appliedExtensions :: Prelude.Maybe [AppliedExtension],
    -- | A list containing all events related to a deployment. The most recent
    -- events are displayed first.
    eventLog :: Prelude.Maybe [DeploymentEvent],
    -- | The configuration version that was deployed.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment that was deployed.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | Information about the source location of the configuration.
    configurationLocationUri :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application that was deployed.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The time the deployment completed.
    completedAt :: Prelude.Maybe Data.POSIX,
    -- | The ID of the configuration profile that was deployed.
    configurationProfileId :: Prelude.Maybe Prelude.Text
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
-- 'deploymentStrategyId', 'deployment_deploymentStrategyId' - The ID of the deployment strategy that was deployed.
--
-- 'growthType', 'deployment_growthType' - The algorithm used to define how percentage grew over time.
--
-- 'state', 'deployment_state' - The state of the deployment.
--
-- 'deploymentDurationInMinutes', 'deployment_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'deploymentNumber', 'deployment_deploymentNumber' - The sequence number of the deployment.
--
-- 'description', 'deployment_description' - The description of the deployment.
--
-- 'finalBakeTimeInMinutes', 'deployment_finalBakeTimeInMinutes' - The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
--
-- 'startedAt', 'deployment_startedAt' - The time the deployment started.
--
-- 'configurationName', 'deployment_configurationName' - The name of the configuration.
--
-- 'growthFactor', 'deployment_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'appliedExtensions', 'deployment_appliedExtensions' - A list of extensions that were processed as part of the deployment. The
-- extensions that were previously associated to the configuration profile,
-- environment, or the application when @StartDeployment@ was called.
--
-- 'eventLog', 'deployment_eventLog' - A list containing all events related to a deployment. The most recent
-- events are displayed first.
--
-- 'configurationVersion', 'deployment_configurationVersion' - The configuration version that was deployed.
--
-- 'environmentId', 'deployment_environmentId' - The ID of the environment that was deployed.
--
-- 'percentageComplete', 'deployment_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'configurationLocationUri', 'deployment_configurationLocationUri' - Information about the source location of the configuration.
--
-- 'applicationId', 'deployment_applicationId' - The ID of the application that was deployed.
--
-- 'completedAt', 'deployment_completedAt' - The time the deployment completed.
--
-- 'configurationProfileId', 'deployment_configurationProfileId' - The ID of the configuration profile that was deployed.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { deploymentStrategyId = Prelude.Nothing,
      growthType = Prelude.Nothing,
      state = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      description = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      growthFactor = Prelude.Nothing,
      appliedExtensions = Prelude.Nothing,
      eventLog = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      configurationLocationUri = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      configurationProfileId = Prelude.Nothing
    }

-- | The ID of the deployment strategy that was deployed.
deployment_deploymentStrategyId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_deploymentStrategyId = Lens.lens (\Deployment' {deploymentStrategyId} -> deploymentStrategyId) (\s@Deployment' {} a -> s {deploymentStrategyId = a} :: Deployment)

-- | The algorithm used to define how percentage grew over time.
deployment_growthType :: Lens.Lens' Deployment (Prelude.Maybe GrowthType)
deployment_growthType = Lens.lens (\Deployment' {growthType} -> growthType) (\s@Deployment' {} a -> s {growthType = a} :: Deployment)

-- | The state of the deployment.
deployment_state :: Lens.Lens' Deployment (Prelude.Maybe DeploymentState)
deployment_state = Lens.lens (\Deployment' {state} -> state) (\s@Deployment' {} a -> s {state = a} :: Deployment)

-- | Total amount of time the deployment lasted.
deployment_deploymentDurationInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_deploymentDurationInMinutes = Lens.lens (\Deployment' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@Deployment' {} a -> s {deploymentDurationInMinutes = a} :: Deployment)

-- | The sequence number of the deployment.
deployment_deploymentNumber :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_deploymentNumber = Lens.lens (\Deployment' {deploymentNumber} -> deploymentNumber) (\s@Deployment' {} a -> s {deploymentNumber = a} :: Deployment)

-- | The description of the deployment.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

-- | The amount of time that AppConfig monitored for alarms before
-- considering the deployment to be complete and no longer eligible for
-- automatic rollback.
deployment_finalBakeTimeInMinutes :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Natural)
deployment_finalBakeTimeInMinutes = Lens.lens (\Deployment' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@Deployment' {} a -> s {finalBakeTimeInMinutes = a} :: Deployment)

-- | The time the deployment started.
deployment_startedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_startedAt = Lens.lens (\Deployment' {startedAt} -> startedAt) (\s@Deployment' {} a -> s {startedAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The name of the configuration.
deployment_configurationName :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationName = Lens.lens (\Deployment' {configurationName} -> configurationName) (\s@Deployment' {} a -> s {configurationName = a} :: Deployment)

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deployment_growthFactor :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_growthFactor = Lens.lens (\Deployment' {growthFactor} -> growthFactor) (\s@Deployment' {} a -> s {growthFactor = a} :: Deployment)

-- | A list of extensions that were processed as part of the deployment. The
-- extensions that were previously associated to the configuration profile,
-- environment, or the application when @StartDeployment@ was called.
deployment_appliedExtensions :: Lens.Lens' Deployment (Prelude.Maybe [AppliedExtension])
deployment_appliedExtensions = Lens.lens (\Deployment' {appliedExtensions} -> appliedExtensions) (\s@Deployment' {} a -> s {appliedExtensions = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | A list containing all events related to a deployment. The most recent
-- events are displayed first.
deployment_eventLog :: Lens.Lens' Deployment (Prelude.Maybe [DeploymentEvent])
deployment_eventLog = Lens.lens (\Deployment' {eventLog} -> eventLog) (\s@Deployment' {} a -> s {eventLog = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The configuration version that was deployed.
deployment_configurationVersion :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationVersion = Lens.lens (\Deployment' {configurationVersion} -> configurationVersion) (\s@Deployment' {} a -> s {configurationVersion = a} :: Deployment)

-- | The ID of the environment that was deployed.
deployment_environmentId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_environmentId = Lens.lens (\Deployment' {environmentId} -> environmentId) (\s@Deployment' {} a -> s {environmentId = a} :: Deployment)

-- | The percentage of targets for which the deployment is available.
deployment_percentageComplete :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Double)
deployment_percentageComplete = Lens.lens (\Deployment' {percentageComplete} -> percentageComplete) (\s@Deployment' {} a -> s {percentageComplete = a} :: Deployment)

-- | Information about the source location of the configuration.
deployment_configurationLocationUri :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationLocationUri = Lens.lens (\Deployment' {configurationLocationUri} -> configurationLocationUri) (\s@Deployment' {} a -> s {configurationLocationUri = a} :: Deployment)

-- | The ID of the application that was deployed.
deployment_applicationId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_applicationId = Lens.lens (\Deployment' {applicationId} -> applicationId) (\s@Deployment' {} a -> s {applicationId = a} :: Deployment)

-- | The time the deployment completed.
deployment_completedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_completedAt = Lens.lens (\Deployment' {completedAt} -> completedAt) (\s@Deployment' {} a -> s {completedAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The ID of the configuration profile that was deployed.
deployment_configurationProfileId :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_configurationProfileId = Lens.lens (\Deployment' {configurationProfileId} -> configurationProfileId) (\s@Deployment' {} a -> s {configurationProfileId = a} :: Deployment)

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "DeploymentStrategyId")
            Prelude.<*> (x Data..:? "GrowthType")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Data..:? "DeploymentNumber")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Data..:? "StartedAt")
            Prelude.<*> (x Data..:? "ConfigurationName")
            Prelude.<*> (x Data..:? "GrowthFactor")
            Prelude.<*> ( x Data..:? "AppliedExtensions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EventLog" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ConfigurationVersion")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "PercentageComplete")
            Prelude.<*> (x Data..:? "ConfigurationLocationUri")
            Prelude.<*> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "CompletedAt")
            Prelude.<*> (x Data..:? "ConfigurationProfileId")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentStrategyId
      `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` deploymentNumber
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` growthFactor
      `Prelude.hashWithSalt` appliedExtensions
      `Prelude.hashWithSalt` eventLog
      `Prelude.hashWithSalt` configurationVersion
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` configurationLocationUri
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` configurationProfileId

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf deploymentStrategyId
      `Prelude.seq` Prelude.rnf growthType
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf deploymentDurationInMinutes
      `Prelude.seq` Prelude.rnf deploymentNumber
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf finalBakeTimeInMinutes
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf configurationName
      `Prelude.seq` Prelude.rnf growthFactor
      `Prelude.seq` Prelude.rnf appliedExtensions
      `Prelude.seq` Prelude.rnf eventLog
      `Prelude.seq` Prelude.rnf configurationVersion
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf configurationLocationUri
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf
        configurationProfileId
