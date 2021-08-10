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
import qualified Network.AWS.Prelude as Prelude

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
    applicationCode :: Prelude.Maybe Prelude.Text,
    -- | Description of the application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | Describes the CloudWatch log streams that are configured to receive
    -- application messages. For more information about using CloudWatch log
    -- streams with Amazon Kinesis Analytics applications, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
    cloudWatchLoggingOptionDescriptions :: Prelude.Maybe [CloudWatchLoggingOptionDescription],
    -- | Time stamp when the application version was created.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Describes the application output configuration. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    outputDescriptions :: Prelude.Maybe [OutputDescription],
    -- | Describes reference data sources configured for the application. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
    referenceDataSourceDescriptions :: Prelude.Maybe [ReferenceDataSourceDescription],
    -- | Describes the application input configuration. For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
    inputDescriptions :: Prelude.Maybe [InputDescription],
    -- | Time stamp when the application was last updated.
    lastUpdateTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Name of the application.
    applicationName :: Prelude.Text,
    -- | ARN of the application.
    applicationARN :: Prelude.Text,
    -- | Status of the application.
    applicationStatus :: ApplicationStatus,
    -- | Provides the current application version.
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
  Prelude.Text ->
  -- | 'applicationARN'
  Prelude.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  -- | 'applicationVersionId'
  Prelude.Natural ->
  ApplicationDetail
newApplicationDetail
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_
  pApplicationVersionId_ =
    ApplicationDetail'
      { applicationCode =
          Prelude.Nothing,
        applicationDescription = Prelude.Nothing,
        cloudWatchLoggingOptionDescriptions =
          Prelude.Nothing,
        createTimestamp = Prelude.Nothing,
        outputDescriptions = Prelude.Nothing,
        referenceDataSourceDescriptions = Prelude.Nothing,
        inputDescriptions = Prelude.Nothing,
        lastUpdateTimestamp = Prelude.Nothing,
        applicationName = pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_,
        applicationVersionId = pApplicationVersionId_
      }

-- | Returns the application code that you provided to perform data analysis
-- on any of the in-application streams in your application.
applicationDetail_applicationCode :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Text)
applicationDetail_applicationCode = Lens.lens (\ApplicationDetail' {applicationCode} -> applicationCode) (\s@ApplicationDetail' {} a -> s {applicationCode = a} :: ApplicationDetail)

-- | Description of the application.
applicationDetail_applicationDescription :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.Text)
applicationDetail_applicationDescription = Lens.lens (\ApplicationDetail' {applicationDescription} -> applicationDescription) (\s@ApplicationDetail' {} a -> s {applicationDescription = a} :: ApplicationDetail)

-- | Describes the CloudWatch log streams that are configured to receive
-- application messages. For more information about using CloudWatch log
-- streams with Amazon Kinesis Analytics applications, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs>.
applicationDetail_cloudWatchLoggingOptionDescriptions :: Lens.Lens' ApplicationDetail (Prelude.Maybe [CloudWatchLoggingOptionDescription])
applicationDetail_cloudWatchLoggingOptionDescriptions = Lens.lens (\ApplicationDetail' {cloudWatchLoggingOptionDescriptions} -> cloudWatchLoggingOptionDescriptions) (\s@ApplicationDetail' {} a -> s {cloudWatchLoggingOptionDescriptions = a} :: ApplicationDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Time stamp when the application version was created.
applicationDetail_createTimestamp :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.UTCTime)
applicationDetail_createTimestamp = Lens.lens (\ApplicationDetail' {createTimestamp} -> createTimestamp) (\s@ApplicationDetail' {} a -> s {createTimestamp = a} :: ApplicationDetail) Prelude.. Lens.mapping Core._Time

-- | Describes the application output configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
applicationDetail_outputDescriptions :: Lens.Lens' ApplicationDetail (Prelude.Maybe [OutputDescription])
applicationDetail_outputDescriptions = Lens.lens (\ApplicationDetail' {outputDescriptions} -> outputDescriptions) (\s@ApplicationDetail' {} a -> s {outputDescriptions = a} :: ApplicationDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Describes reference data sources configured for the application. For
-- more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
applicationDetail_referenceDataSourceDescriptions :: Lens.Lens' ApplicationDetail (Prelude.Maybe [ReferenceDataSourceDescription])
applicationDetail_referenceDataSourceDescriptions = Lens.lens (\ApplicationDetail' {referenceDataSourceDescriptions} -> referenceDataSourceDescriptions) (\s@ApplicationDetail' {} a -> s {referenceDataSourceDescriptions = a} :: ApplicationDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Describes the application input configuration. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
applicationDetail_inputDescriptions :: Lens.Lens' ApplicationDetail (Prelude.Maybe [InputDescription])
applicationDetail_inputDescriptions = Lens.lens (\ApplicationDetail' {inputDescriptions} -> inputDescriptions) (\s@ApplicationDetail' {} a -> s {inputDescriptions = a} :: ApplicationDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Time stamp when the application was last updated.
applicationDetail_lastUpdateTimestamp :: Lens.Lens' ApplicationDetail (Prelude.Maybe Prelude.UTCTime)
applicationDetail_lastUpdateTimestamp = Lens.lens (\ApplicationDetail' {lastUpdateTimestamp} -> lastUpdateTimestamp) (\s@ApplicationDetail' {} a -> s {lastUpdateTimestamp = a} :: ApplicationDetail) Prelude.. Lens.mapping Core._Time

-- | Name of the application.
applicationDetail_applicationName :: Lens.Lens' ApplicationDetail Prelude.Text
applicationDetail_applicationName = Lens.lens (\ApplicationDetail' {applicationName} -> applicationName) (\s@ApplicationDetail' {} a -> s {applicationName = a} :: ApplicationDetail)

-- | ARN of the application.
applicationDetail_applicationARN :: Lens.Lens' ApplicationDetail Prelude.Text
applicationDetail_applicationARN = Lens.lens (\ApplicationDetail' {applicationARN} -> applicationARN) (\s@ApplicationDetail' {} a -> s {applicationARN = a} :: ApplicationDetail)

-- | Status of the application.
applicationDetail_applicationStatus :: Lens.Lens' ApplicationDetail ApplicationStatus
applicationDetail_applicationStatus = Lens.lens (\ApplicationDetail' {applicationStatus} -> applicationStatus) (\s@ApplicationDetail' {} a -> s {applicationStatus = a} :: ApplicationDetail)

-- | Provides the current application version.
applicationDetail_applicationVersionId :: Lens.Lens' ApplicationDetail Prelude.Natural
applicationDetail_applicationVersionId = Lens.lens (\ApplicationDetail' {applicationVersionId} -> applicationVersionId) (\s@ApplicationDetail' {} a -> s {applicationVersionId = a} :: ApplicationDetail)

instance Core.FromJSON ApplicationDetail where
  parseJSON =
    Core.withObject
      "ApplicationDetail"
      ( \x ->
          ApplicationDetail'
            Prelude.<$> (x Core..:? "ApplicationCode")
            Prelude.<*> (x Core..:? "ApplicationDescription")
            Prelude.<*> ( x Core..:? "CloudWatchLoggingOptionDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreateTimestamp")
            Prelude.<*> ( x Core..:? "OutputDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ReferenceDataSourceDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InputDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LastUpdateTimestamp")
            Prelude.<*> (x Core..: "ApplicationName")
            Prelude.<*> (x Core..: "ApplicationARN")
            Prelude.<*> (x Core..: "ApplicationStatus")
            Prelude.<*> (x Core..: "ApplicationVersionId")
      )

instance Prelude.Hashable ApplicationDetail

instance Prelude.NFData ApplicationDetail
