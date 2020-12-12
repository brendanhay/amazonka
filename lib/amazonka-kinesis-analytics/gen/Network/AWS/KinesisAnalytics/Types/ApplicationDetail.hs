{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationDetail
  ( ApplicationDetail (..),

    -- * Smart constructor
    mkApplicationDetail,

    -- * Lenses
    adApplicationDescription,
    adOutputDescriptions,
    adCloudWatchLoggingOptionDescriptions,
    adReferenceDataSourceDescriptions,
    adInputDescriptions,
    adApplicationCode,
    adCreateTimestamp,
    adLastUpdateTimestamp,
    adApplicationName,
    adApplicationARN,
    adApplicationStatus,
    adApplicationVersionId,
  )
where

import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.
--
-- /See:/ 'mkApplicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { applicationDescription ::
      Lude.Maybe Lude.Text,
    outputDescriptions :: Lude.Maybe [OutputDescription],
    cloudWatchLoggingOptionDescriptions ::
      Lude.Maybe [CloudWatchLoggingOptionDescription],
    referenceDataSourceDescriptions ::
      Lude.Maybe [ReferenceDataSourceDescription],
    inputDescriptions :: Lude.Maybe [InputDescription],
    applicationCode :: Lude.Maybe Lude.Text,
    createTimestamp :: Lude.Maybe Lude.Timestamp,
    lastUpdateTimestamp :: Lude.Maybe Lude.Timestamp,
    applicationName :: Lude.Text,
    applicationARN :: Lude.Text,
    applicationStatus :: ApplicationStatus,
    applicationVersionId :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationDetail' with the minimum fields required to make a request.
--
-- * 'applicationARN' - ARN of the application.
-- * 'applicationCode' - Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
-- * 'applicationDescription' - Description of the application.
-- * 'applicationName' - Name of the application.
-- * 'applicationStatus' - Status of the application.
-- * 'applicationVersionId' - Provides the current application version.
-- * 'cloudWatchLoggingOptionDescriptions' - Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
-- * 'createTimestamp' - Time stamp when the application version was created.
-- * 'inputDescriptions' - Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
-- * 'lastUpdateTimestamp' - Time stamp when the application was last updated.
-- * 'outputDescriptions' - Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
-- * 'referenceDataSourceDescriptions' - Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
mkApplicationDetail ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'applicationARN'
  Lude.Text ->
  -- | 'applicationStatus'
  ApplicationStatus ->
  -- | 'applicationVersionId'
  Lude.Natural ->
  ApplicationDetail
mkApplicationDetail
  pApplicationName_
  pApplicationARN_
  pApplicationStatus_
  pApplicationVersionId_ =
    ApplicationDetail'
      { applicationDescription = Lude.Nothing,
        outputDescriptions = Lude.Nothing,
        cloudWatchLoggingOptionDescriptions = Lude.Nothing,
        referenceDataSourceDescriptions = Lude.Nothing,
        inputDescriptions = Lude.Nothing,
        applicationCode = Lude.Nothing,
        createTimestamp = Lude.Nothing,
        lastUpdateTimestamp = Lude.Nothing,
        applicationName = pApplicationName_,
        applicationARN = pApplicationARN_,
        applicationStatus = pApplicationStatus_,
        applicationVersionId = pApplicationVersionId_
      }

-- | Description of the application.
--
-- /Note:/ Consider using 'applicationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationDescription :: Lens.Lens' ApplicationDetail (Lude.Maybe Lude.Text)
adApplicationDescription = Lens.lens (applicationDescription :: ApplicationDetail -> Lude.Maybe Lude.Text) (\s a -> s {applicationDescription = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationDescription "Use generic-lens or generic-optics with 'applicationDescription' instead." #-}

-- | Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- /Note:/ Consider using 'outputDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputDescriptions :: Lens.Lens' ApplicationDetail (Lude.Maybe [OutputDescription])
adOutputDescriptions = Lens.lens (outputDescriptions :: ApplicationDetail -> Lude.Maybe [OutputDescription]) (\s a -> s {outputDescriptions = a} :: ApplicationDetail)
{-# DEPRECATED adOutputDescriptions "Use generic-lens or generic-optics with 'outputDescriptions' instead." #-}

-- | Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCloudWatchLoggingOptionDescriptions :: Lens.Lens' ApplicationDetail (Lude.Maybe [CloudWatchLoggingOptionDescription])
adCloudWatchLoggingOptionDescriptions = Lens.lens (cloudWatchLoggingOptionDescriptions :: ApplicationDetail -> Lude.Maybe [CloudWatchLoggingOptionDescription]) (\s a -> s {cloudWatchLoggingOptionDescriptions = a} :: ApplicationDetail)
{-# DEPRECATED adCloudWatchLoggingOptionDescriptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionDescriptions' instead." #-}

-- | Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /Note:/ Consider using 'referenceDataSourceDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adReferenceDataSourceDescriptions :: Lens.Lens' ApplicationDetail (Lude.Maybe [ReferenceDataSourceDescription])
adReferenceDataSourceDescriptions = Lens.lens (referenceDataSourceDescriptions :: ApplicationDetail -> Lude.Maybe [ReferenceDataSourceDescription]) (\s a -> s {referenceDataSourceDescriptions = a} :: ApplicationDetail)
{-# DEPRECATED adReferenceDataSourceDescriptions "Use generic-lens or generic-optics with 'referenceDataSourceDescriptions' instead." #-}

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- /Note:/ Consider using 'inputDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInputDescriptions :: Lens.Lens' ApplicationDetail (Lude.Maybe [InputDescription])
adInputDescriptions = Lens.lens (inputDescriptions :: ApplicationDetail -> Lude.Maybe [InputDescription]) (\s a -> s {inputDescriptions = a} :: ApplicationDetail)
{-# DEPRECATED adInputDescriptions "Use generic-lens or generic-optics with 'inputDescriptions' instead." #-}

-- | Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
--
-- /Note:/ Consider using 'applicationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationCode :: Lens.Lens' ApplicationDetail (Lude.Maybe Lude.Text)
adApplicationCode = Lens.lens (applicationCode :: ApplicationDetail -> Lude.Maybe Lude.Text) (\s a -> s {applicationCode = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationCode "Use generic-lens or generic-optics with 'applicationCode' instead." #-}

-- | Time stamp when the application version was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCreateTimestamp :: Lens.Lens' ApplicationDetail (Lude.Maybe Lude.Timestamp)
adCreateTimestamp = Lens.lens (createTimestamp :: ApplicationDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTimestamp = a} :: ApplicationDetail)
{-# DEPRECATED adCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

-- | Time stamp when the application was last updated.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastUpdateTimestamp :: Lens.Lens' ApplicationDetail (Lude.Maybe Lude.Timestamp)
adLastUpdateTimestamp = Lens.lens (lastUpdateTimestamp :: ApplicationDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTimestamp = a} :: ApplicationDetail)
{-# DEPRECATED adLastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead." #-}

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationName :: Lens.Lens' ApplicationDetail Lude.Text
adApplicationName = Lens.lens (applicationName :: ApplicationDetail -> Lude.Text) (\s a -> s {applicationName = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | ARN of the application.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationARN :: Lens.Lens' ApplicationDetail Lude.Text
adApplicationARN = Lens.lens (applicationARN :: ApplicationDetail -> Lude.Text) (\s a -> s {applicationARN = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationARN "Use generic-lens or generic-optics with 'applicationARN' instead." #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationStatus :: Lens.Lens' ApplicationDetail ApplicationStatus
adApplicationStatus = Lens.lens (applicationStatus :: ApplicationDetail -> ApplicationStatus) (\s a -> s {applicationStatus = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead." #-}

-- | Provides the current application version.
--
-- /Note:/ Consider using 'applicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationVersionId :: Lens.Lens' ApplicationDetail Lude.Natural
adApplicationVersionId = Lens.lens (applicationVersionId :: ApplicationDetail -> Lude.Natural) (\s a -> s {applicationVersionId = a} :: ApplicationDetail)
{-# DEPRECATED adApplicationVersionId "Use generic-lens or generic-optics with 'applicationVersionId' instead." #-}

instance Lude.FromJSON ApplicationDetail where
  parseJSON =
    Lude.withObject
      "ApplicationDetail"
      ( \x ->
          ApplicationDetail'
            Lude.<$> (x Lude..:? "ApplicationDescription")
            Lude.<*> (x Lude..:? "OutputDescriptions" Lude..!= Lude.mempty)
            Lude.<*> ( x Lude..:? "CloudWatchLoggingOptionDescriptions"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "ReferenceDataSourceDescriptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InputDescriptions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ApplicationCode")
            Lude.<*> (x Lude..:? "CreateTimestamp")
            Lude.<*> (x Lude..:? "LastUpdateTimestamp")
            Lude.<*> (x Lude..: "ApplicationName")
            Lude.<*> (x Lude..: "ApplicationARN")
            Lude.<*> (x Lude..: "ApplicationStatus")
            Lude.<*> (x Lude..: "ApplicationVersionId")
      )
