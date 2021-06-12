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
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import qualified Network.AWS.Lens as Lens

-- | This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Provides a description of the application, including the application
-- Amazon Resource Name (ARN), status, latest version, and input and output
-- configuration.
--
-- /See:/ 'newApplicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { -- | Returns the application code that you provided to perform data analysis
    -- on any of the in-application streams in your application.
    applicationCode :: Core.Maybe Core.Text,
    -- | Description of the application.
    applicationDescription :: Core.Maybe Core.Text,
    -- | Describes the CloudWatch log streams that are configured to receive
    -- application messages. For more information about using CloudWatch log
    -- streams with Amazon Kinesis Analytics applications, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
    cloudWatchLoggingOptionDescriptions :: Core.Maybe [CloudWatchLoggingOptionDescription],
    -- | Time stamp when the application version was created.
    createTimestamp :: Core.Maybe Core.POSIX,
    -- | Describes the application output configuration. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    outputDescriptions :: Core.Maybe [OutputDescription],
    -- | Describes reference data sources configured for the application. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
    referenceDataSourceDescriptions :: Core.Maybe [ReferenceDataSourceDescription],
    -- | Describes the application input configuration. For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
    inputDescriptions :: Core.Maybe [InputDescription],
    -- | Time stamp when the application was last updated.
    lastUpdateTimestamp :: Core.Maybe Core.POSIX,
    -- | Name of the application.
    applicationName :: Core.Text,
    -- | ARN of the application.
    applicationARN :: Core.Text,
    -- | Status of the application.
    applicationStatus :: ApplicationStatus,
    -- | Provides the current application version.
    applicationVersionId :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationCode', 'applicationDetail_applicationCode' - Returns the application code that you provided to perform data analysis
-- on any of the in-application streams in your application.
--
-- 'applicationDescription', 'applicationDetail_applicationDescription' - Description of the application.
--
-- 'cloudWatchLoggingOptionDescriptions', 'applicationDetail_cloudWatchLoggingOptionDescriptions' - Describes the CloudWatch log streams that are configured to receive
-- application messages. For more information about using CloudWatch log
-- streams with Amazon Kinesis Analytics applications, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
--
-- 'createTimestamp', 'applicationDetail_createTimestamp' - Time stamp when the application version was created.
--
-- 'outputDescriptions', 'applicationDetail_outputDescriptions' - Describes the application output configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- 'referenceDataSourceDescriptions', 'applicationDetail_referenceDataSourceDescriptions' - Describes reference data sources configured for the application. For
-- more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- 'inputDescriptions', 'applicationDetail_inputDescriptions' - Describes the application input configuration. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- 'lastUpdateTimestamp', 'applicationDetail_lastUpdateTimestamp' - Time stamp when the application was last updated.
--
-- 'applicationName', 'applicationDetail_applicationName' - Name of the application.
--
-- 'applicationARN', 'applicationDetail_applicationARN' - ARN of the application.
--
-- 'applicationStatus', 'applicationDetail_applicationStatus' - Status of the application.
--
-- 'applicationVersionId', 'applicationDetail_applicationVersionId' - Provides the current application version.
newApplicationDetail ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'applicationARN'
  Core.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  -- | 'applicationVersionId'
  Core.Natural ->
  ApplicationDetail
newApplicationDetail
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_
  pApplicationVersionId_ =
    ApplicationDetail'
      { applicationCode = Core.Nothing,
        applicationDescription = Core.Nothing,
        cloudWatchLoggingOptionDescriptions = Core.Nothing,
        createTimestamp = Core.Nothing,
        outputDescriptions = Core.Nothing,
        referenceDataSourceDescriptions = Core.Nothing,
        inputDescriptions = Core.Nothing,
        lastUpdateTimestamp = Core.Nothing,
        applicationName = pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_,
        applicationVersionId = pApplicationVersionId_
      }

-- | Returns the application code that you provided to perform data analysis
-- on any of the in-application streams in your application.
applicationDetail_applicationCode :: Lens.Lens' ApplicationDetail (Core.Maybe Core.Text)
applicationDetail_applicationCode = Lens.lens (\ApplicationDetail' {applicationCode} -> applicationCode) (\s@ApplicationDetail' {} a -> s {applicationCode = a} :: ApplicationDetail)

-- | Description of the application.
applicationDetail_applicationDescription :: Lens.Lens' ApplicationDetail (Core.Maybe Core.Text)
applicationDetail_applicationDescription = Lens.lens (\ApplicationDetail' {applicationDescription} -> applicationDescription) (\s@ApplicationDetail' {} a -> s {applicationDescription = a} :: ApplicationDetail)

-- | Describes the CloudWatch log streams that are configured to receive
-- application messages. For more information about using CloudWatch log
-- streams with Amazon Kinesis Analytics applications, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
applicationDetail_cloudWatchLoggingOptionDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [CloudWatchLoggingOptionDescription])
applicationDetail_cloudWatchLoggingOptionDescriptions = Lens.lens (\ApplicationDetail' {cloudWatchLoggingOptionDescriptions} -> cloudWatchLoggingOptionDescriptions) (\s@ApplicationDetail' {} a -> s {cloudWatchLoggingOptionDescriptions = a} :: ApplicationDetail) Core.. Lens.mapping Lens._Coerce

-- | Time stamp when the application version was created.
applicationDetail_createTimestamp :: Lens.Lens' ApplicationDetail (Core.Maybe Core.UTCTime)
applicationDetail_createTimestamp = Lens.lens (\ApplicationDetail' {createTimestamp} -> createTimestamp) (\s@ApplicationDetail' {} a -> s {createTimestamp = a} :: ApplicationDetail) Core.. Lens.mapping Core._Time

-- | Describes the application output configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
applicationDetail_outputDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [OutputDescription])
applicationDetail_outputDescriptions = Lens.lens (\ApplicationDetail' {outputDescriptions} -> outputDescriptions) (\s@ApplicationDetail' {} a -> s {outputDescriptions = a} :: ApplicationDetail) Core.. Lens.mapping Lens._Coerce

-- | Describes reference data sources configured for the application. For
-- more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
applicationDetail_referenceDataSourceDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [ReferenceDataSourceDescription])
applicationDetail_referenceDataSourceDescriptions = Lens.lens (\ApplicationDetail' {referenceDataSourceDescriptions} -> referenceDataSourceDescriptions) (\s@ApplicationDetail' {} a -> s {referenceDataSourceDescriptions = a} :: ApplicationDetail) Core.. Lens.mapping Lens._Coerce

-- | Describes the application input configuration. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
applicationDetail_inputDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [InputDescription])
applicationDetail_inputDescriptions = Lens.lens (\ApplicationDetail' {inputDescriptions} -> inputDescriptions) (\s@ApplicationDetail' {} a -> s {inputDescriptions = a} :: ApplicationDetail) Core.. Lens.mapping Lens._Coerce

-- | Time stamp when the application was last updated.
applicationDetail_lastUpdateTimestamp :: Lens.Lens' ApplicationDetail (Core.Maybe Core.UTCTime)
applicationDetail_lastUpdateTimestamp = Lens.lens (\ApplicationDetail' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ApplicationDetail' {} a -> s {lastUpdateTimestamp = a} :: ApplicationDetail) Core.. Lens.mapping Core._Time

-- | Name of the application.
applicationDetail_applicationName :: Lens.Lens' ApplicationDetail Core.Text
applicationDetail_applicationName = Lens.lens (\ApplicationDetail' {applicationName} -> applicationName) (\s@ApplicationDetail' {} a -> s {applicationName = a} :: ApplicationDetail)

-- | ARN of the application.
applicationDetail_applicationARN :: Lens.Lens' ApplicationDetail Core.Text
applicationDetail_applicationARN = Lens.lens (\ApplicationDetail' {applicationARN} -> applicationARN) (\s@ApplicationDetail' {} a -> s {applicationARN = a} :: ApplicationDetail)

-- | Status of the application.
applicationDetail_applicationStatus :: Lens.Lens' ApplicationDetail ApplicationStatus
applicationDetail_applicationStatus = Lens.lens (\ApplicationDetail' {applicationStatus} -> applicationStatus) (\s@ApplicationDetail' {} a -> s {applicationStatus = a} :: ApplicationDetail)

-- | Provides the current application version.
applicationDetail_applicationVersionId :: Lens.Lens' ApplicationDetail Core.Natural
applicationDetail_applicationVersionId = Lens.lens (\ApplicationDetail' {applicationVersionId} -> applicationVersionId) (\s@ApplicationDetail' {} a -> s {applicationVersionId = a} :: ApplicationDetail)

instance Core.FromJSON ApplicationDetail where
  parseJSON =
    Core.withObject
      "ApplicationDetail"
      ( \x ->
          ApplicationDetail'
            Core.<$> (x Core..:? "ApplicationCode")
            Core.<*> (x Core..:? "ApplicationDescription")
            Core.<*> ( x Core..:? "CloudWatchLoggingOptionDescriptions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CreateTimestamp")
            Core.<*> ( x Core..:? "OutputDescriptions"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "ReferenceDataSourceDescriptions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "InputDescriptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastUpdateTimestamp")
            Core.<*> (x Core..: "ApplicationName")
            Core.<*> (x Core..: "ApplicationARN")
            Core.<*> (x Core..: "ApplicationStatus")
            Core.<*> (x Core..: "ApplicationVersionId")
      )

instance Core.Hashable ApplicationDetail

instance Core.NFData ApplicationDetail
