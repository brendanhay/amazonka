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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.ApplicationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.ApplicationDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationMode
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationStatus
import Network.AWS.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalyticsV2.Types.RuntimeEnvironment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the application, including the application Amazon Resource
-- Name (ARN), status, latest version, and input and output configurations.
--
-- /See:/ 'newApplicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { -- | The description of the application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | A value you use to implement strong concurrency for application updates.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the application Amazon CloudWatch logging options.
    cloudWatchLoggingOptionDescriptions :: Prelude.Maybe [CloudWatchLoggingOptionDescription],
    -- | If you reverted the application using RollbackApplication, the
    -- application version when @RollbackApplication@ was called.
    applicationVersionRolledBackFrom :: Prelude.Maybe Prelude.Natural,
    -- | To create a Kinesis Data Analytics Studio notebook, you must set the
    -- mode to @INTERACTIVE@. However, for a Kinesis Data Analytics for Apache
    -- Flink application, the mode is optional.
    applicationMode :: Prelude.Maybe ApplicationMode,
    -- | Describes details about the application code and starting parameters for
    -- a Kinesis Data Analytics application.
    applicationConfigurationDescription :: Prelude.Maybe ApplicationConfigurationDescription,
    -- | The version to which you want to roll back the application.
    applicationVersionRolledBackTo :: Prelude.Maybe Prelude.Natural,
    -- | The current timestamp when the application was created.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The current timestamp when the application was last updated.
    lastUpdateTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Specifies the IAM role that the application uses to access external
    -- resources.
    serviceExecutionRole :: Prelude.Maybe Prelude.Text,
    -- | The previous application version before the latest application update.
    -- RollbackApplication reverts the application to this version.
    applicationVersionUpdatedFrom :: Prelude.Maybe Prelude.Natural,
    -- | The details of the maintenance configuration for the application.
    applicationMaintenanceConfigurationDescription :: Prelude.Maybe ApplicationMaintenanceConfigurationDescription,
    -- | The ARN of the application.
    applicationARN :: Prelude.Text,
    -- | The name of the application.
    applicationName :: Prelude.Text,
    -- | The runtime environment for the application (@SQL-1_0@, @FLINK-1_6@,
    -- @FLINK-1_8@, or @FLINK-1_11@).
    runtimeEnvironment :: RuntimeEnvironment,
    -- | The status of the application.
    applicationStatus :: ApplicationStatus,
    -- | Provides the current application version. Kinesis Data Analytics updates
    -- the @ApplicationVersionId@ each time you update the application.
    applicationVersionId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationDescription', 'applicationDetail_applicationDescription' - The description of the application.
--
-- 'conditionalToken', 'applicationDetail_conditionalToken' - A value you use to implement strong concurrency for application updates.
--
-- 'cloudWatchLoggingOptionDescriptions', 'applicationDetail_cloudWatchLoggingOptionDescriptions' - Describes the application Amazon CloudWatch logging options.
--
-- 'applicationVersionRolledBackFrom', 'applicationDetail_applicationVersionRolledBackFrom' - If you reverted the application using RollbackApplication, the
-- application version when @RollbackApplication@ was called.
--
-- 'applicationMode', 'applicationDetail_applicationMode' - To create a Kinesis Data Analytics Studio notebook, you must set the
-- mode to @INTERACTIVE@. However, for a Kinesis Data Analytics for Apache
-- Flink application, the mode is optional.
--
-- 'applicationConfigurationDescription', 'applicationDetail_applicationConfigurationDescription' - Describes details about the application code and starting parameters for
-- a Kinesis Data Analytics application.
--
-- 'applicationVersionRolledBackTo', 'applicationDetail_applicationVersionRolledBackTo' - The version to which you want to roll back the application.
--
-- 'createTimestamp', 'applicationDetail_createTimestamp' - The current timestamp when the application was created.
--
-- 'lastUpdateTimestamp', 'applicationDetail_lastUpdateTimestamp' - The current timestamp when the application was last updated.
--
-- 'serviceExecutionRole', 'applicationDetail_serviceExecutionRole' - Specifies the IAM role that the application uses to access external
-- resources.
--
-- 'applicationVersionUpdatedFrom', 'applicationDetail_applicationVersionUpdatedFrom' - The previous application version before the latest application update.
-- RollbackApplication reverts the application to this version.
--
-- 'applicationMaintenanceConfigurationDescription', 'applicationDetail_applicationMaintenanceConfigurationDescription' - The details of the maintenance configuration for the application.
--
-- 'applicationARN', 'applicationDetail_applicationARN' - The ARN of the application.
--
-- 'applicationName', 'applicationDetail_applicationName' - The name of the application.
--
-- 'runtimeEnvironment', 'applicationDetail_runtimeEnvironment' - The runtime environment for the application (@SQL-1_0@, @FLINK-1_6@,
-- @FLINK-1_8@, or @FLINK-1_11@).
--
-- 'applicationStatus', 'applicationDetail_applicationStatus' - The status of the application.
--
-- 'applicationVersionId', 'applicationDetail_applicationVersionId' - Provides the current application version. Kinesis Data Analytics updates
-- the @ApplicationVersionId@ each time you update the application.
newApplicationDetail ::
  -- | 'applicationARN'
  Prelude.Text ->
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'runtimeEnvironment'
  RuntimeEnvironment ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  -- | 'applicationVersionId'
  Prelude.Natural ->
  ApplicationDetail
newApplicationDetail
  pApplicationARN_
  pApplicationName_
  pRuntimeEnvironment_
  pApplicationStatus_
  pApplicationVersionId_ =
    ApplicationDetail'
      { applicationDescription =
          Prelude.Nothing,
        conditionalToken = Prelude.Nothing,
        cloudWatchLoggingOptionDescriptions =
          Prelude.Nothing,
        applicationVersionRolledBackFrom = Prelude.Nothing,
        applicationMode = Prelude.Nothing,
        applicationConfigurationDescription =
          Prelude.Nothing,
        applicationVersionRolledBackTo = Prelude.Nothing,
        createTimestamp = Prelude.Nothing,
        lastUpdateTimestamp = Prelude.Nothing,
        serviceExecutionRole = Prelude.Nothing,
        applicationVersionUpdatedFrom = Prelude.Nothing,
        applicationMaintenanceConfigurationDescription =
          Prelude.Nothing,
        applicationARN = pApplicationARN_,
        applicationName = pApplicationName_,
        runtimeEnvironment = pRuntimeEnvironment_,
        applicationStatus = pApplicationStatus_,
        applicationVersionId = pApplicationVersionId_
      }

-- | The description of the application.
applicationDetail_applicationDescription :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Text)
applicationDetail_applicationDescription = Lens.lens (\ApplicationDetail' {applicationDescription} -> applicationDescription) (\s@ApplicationDetail' {} a -> s {applicationDescription = a} :: ApplicationDetail)

-- | A value you use to implement strong concurrency for application updates.
applicationDetail_conditionalToken :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Text)
applicationDetail_conditionalToken = Lens.lens (\ApplicationDetail' {conditionalToken} -> conditionalToken) (\s@ApplicationDetail' {} a -> s {conditionalToken = a} :: ApplicationDetail)

-- | Describes the application Amazon CloudWatch logging options.
applicationDetail_cloudWatchLoggingOptionDescriptions :: Lens.Lens' ApplicationDetail (Prelude.Maybe [CloudWatchLoggingOptionDescription])
applicationDetail_cloudWatchLoggingOptionDescriptions = Lens.lens (\ApplicationDetail' {cloudWatchLoggingOptionDescriptions} -> cloudWatchLoggingOptionDescriptions) (\s@ApplicationDetail' {} a -> s {cloudWatchLoggingOptionDescriptions = a} :: ApplicationDetail) Prelude.. Lens.mapping Lens.coerced

-- | If you reverted the application using RollbackApplication, the
-- application version when @RollbackApplication@ was called.
applicationDetail_applicationVersionRolledBackFrom :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Natural)
applicationDetail_applicationVersionRolledBackFrom = Lens.lens (\ApplicationDetail' {applicationVersionRolledBackFrom} -> applicationVersionRolledBackFrom) (\s@ApplicationDetail' {} a -> s {applicationVersionRolledBackFrom = a} :: ApplicationDetail)

-- | To create a Kinesis Data Analytics Studio notebook, you must set the
-- mode to @INTERACTIVE@. However, for a Kinesis Data Analytics for Apache
-- Flink application, the mode is optional.
applicationDetail_applicationMode :: Lens.Lens' ApplicationDetail (Prelude.Maybe ApplicationMode)
applicationDetail_applicationMode = Lens.lens (\ApplicationDetail' {applicationMode} -> applicationMode) (\s@ApplicationDetail' {} a -> s {applicationMode = a} :: ApplicationDetail)

-- | Describes details about the application code and starting parameters for
-- a Kinesis Data Analytics application.
applicationDetail_applicationConfigurationDescription :: Lens.Lens' ApplicationDetail (Prelude.Maybe ApplicationConfigurationDescription)
applicationDetail_applicationConfigurationDescription = Lens.lens (\ApplicationDetail' {applicationConfigurationDescription} -> applicationConfigurationDescription) (\s@ApplicationDetail' {} a -> s {applicationConfigurationDescription = a} :: ApplicationDetail)

-- | The version to which you want to roll back the application.
applicationDetail_applicationVersionRolledBackTo :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Natural)
applicationDetail_applicationVersionRolledBackTo = Lens.lens (\ApplicationDetail' {applicationVersionRolledBackTo} -> applicationVersionRolledBackTo) (\s@ApplicationDetail' {} a -> s {applicationVersionRolledBackTo = a} :: ApplicationDetail)

-- | The current timestamp when the application was created.
applicationDetail_createTimestamp :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.UTCTime)
applicationDetail_createTimestamp = Lens.lens (\ApplicationDetail' {createTimestamp} -> createTimestamp) (\s@ApplicationDetail' {} a -> s {createTimestamp = a} :: ApplicationDetail) Prelude.. Lens.mapping Core._Time

-- | The current timestamp when the application was last updated.
applicationDetail_lastUpdateTimestamp :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.UTCTime)
applicationDetail_lastUpdateTimestamp = Lens.lens (\ApplicationDetail' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ApplicationDetail' {} a -> s {lastUpdateTimestamp = a} :: ApplicationDetail) Prelude.. Lens.mapping Core._Time

-- | Specifies the IAM role that the application uses to access external
-- resources.
applicationDetail_serviceExecutionRole :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Text)
applicationDetail_serviceExecutionRole = Lens.lens (\ApplicationDetail' {serviceExecutionRole} -> serviceExecutionRole) (\s@ApplicationDetail' {} a -> s {serviceExecutionRole = a} :: ApplicationDetail)

-- | The previous application version before the latest application update.
-- RollbackApplication reverts the application to this version.
applicationDetail_applicationVersionUpdatedFrom :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Natural)
applicationDetail_applicationVersionUpdatedFrom = Lens.lens (\ApplicationDetail' {applicationVersionUpdatedFrom} -> applicationVersionUpdatedFrom) (\s@ApplicationDetail' {} a -> s {applicationVersionUpdatedFrom = a} :: ApplicationDetail)

-- | The details of the maintenance configuration for the application.
applicationDetail_applicationMaintenanceConfigurationDescription :: Lens.Lens' ApplicationDetail (Prelude.Maybe ApplicationMaintenanceConfigurationDescription)
applicationDetail_applicationMaintenanceConfigurationDescription = Lens.lens (\ApplicationDetail' {applicationMaintenanceConfigurationDescription} -> applicationMaintenanceConfigurationDescription) (\s@ApplicationDetail' {} a -> s {applicationMaintenanceConfigurationDescription = a} :: ApplicationDetail)

-- | The ARN of the application.
applicationDetail_applicationARN :: Lens.Lens' ApplicationDetail Prelude.Text
applicationDetail_applicationARN = Lens.lens (\ApplicationDetail' {applicationARN} -> applicationARN) (\s@ApplicationDetail' {} a -> s {applicationARN = a} :: ApplicationDetail)

-- | The name of the application.
applicationDetail_applicationName :: Lens.Lens' ApplicationDetail Prelude.Text
applicationDetail_applicationName = Lens.lens (\ApplicationDetail' {applicationName} -> applicationName) (\s@ApplicationDetail' {} a -> s {applicationName = a} :: ApplicationDetail)

-- | The runtime environment for the application (@SQL-1_0@, @FLINK-1_6@,
-- @FLINK-1_8@, or @FLINK-1_11@).
applicationDetail_runtimeEnvironment :: Lens.Lens' ApplicationDetail RuntimeEnvironment
applicationDetail_runtimeEnvironment = Lens.lens (\ApplicationDetail' {runtimeEnvironment} -> runtimeEnvironment) (\s@ApplicationDetail' {} a -> s {runtimeEnvironment = a} :: ApplicationDetail)

-- | The status of the application.
applicationDetail_applicationStatus :: Lens.Lens' ApplicationDetail ApplicationStatus
applicationDetail_applicationStatus = Lens.lens (\ApplicationDetail' {applicationStatus} -> applicationStatus) (\s@ApplicationDetail' {} a -> s {applicationStatus = a} :: ApplicationDetail)

-- | Provides the current application version. Kinesis Data Analytics updates
-- the @ApplicationVersionId@ each time you update the application.
applicationDetail_applicationVersionId :: Lens.Lens' ApplicationDetail Prelude.Natural
applicationDetail_applicationVersionId = Lens.lens (\ApplicationDetail' {applicationVersionId} -> applicationVersionId) (\s@ApplicationDetail' {} a -> s {applicationVersionId = a} :: ApplicationDetail)

instance Core.FromJSON ApplicationDetail where
  parseJSON =
    Core.withObject
      "ApplicationDetail"
      ( \x ->
          ApplicationDetail'
            Prelude.<$> (x Core..:? "ApplicationDescription")
            Prelude.<*> (x Core..:? "ConditionalToken")
            Prelude.<*> ( x Core..:? "CloudWatchLoggingOptionDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ApplicationVersionRolledBackFrom")
            Prelude.<*> (x Core..:? "ApplicationMode")
            Prelude.<*> (x Core..:? "ApplicationConfigurationDescription")
            Prelude.<*> (x Core..:? "ApplicationVersionRolledBackTo")
            Prelude.<*> (x Core..:? "CreateTimestamp")
            Prelude.<*> (x Core..:? "LastUpdateTimestamp")
            Prelude.<*> (x Core..:? "ServiceExecutionRole")
            Prelude.<*> (x Core..:? "ApplicationVersionUpdatedFrom")
            Prelude.<*> ( x
                            Core..:? "ApplicationMaintenanceConfigurationDescription"
                        )
            Prelude.<*> (x Core..: "ApplicationARN")
            Prelude.<*> (x Core..: "ApplicationName")
            Prelude.<*> (x Core..: "RuntimeEnvironment")
            Prelude.<*> (x Core..: "ApplicationStatus")
            Prelude.<*> (x Core..: "ApplicationVersionId")
      )

instance Prelude.Hashable ApplicationDetail

instance Prelude.NFData ApplicationDetail
