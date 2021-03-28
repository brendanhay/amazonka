{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.ApplicationDetail
  ( ApplicationDetail (..)
  -- * Smart constructor
  , mkApplicationDetail
  -- * Lenses
  , adApplicationName
  , adApplicationARN
  , adApplicationStatus
  , adApplicationVersionId
  , adApplicationCode
  , adApplicationDescription
  , adCloudWatchLoggingOptionDescriptions
  , adCreateTimestamp
  , adInputDescriptions
  , adLastUpdateTimestamp
  , adOutputDescriptions
  , adReferenceDataSourceDescriptions
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ApplicationCode as Types
import qualified Network.AWS.KinesisAnalytics.Types.ApplicationDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.ApplicationName as Types
import qualified Network.AWS.KinesisAnalytics.Types.ApplicationStatus as Types
import qualified Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.OutputDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.
--
-- /See:/ 'mkApplicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { applicationName :: Types.ApplicationName
    -- ^ Name of the application.
  , applicationARN :: Types.ResourceARN
    -- ^ ARN of the application.
  , applicationStatus :: Types.ApplicationStatus
    -- ^ Status of the application.
  , applicationVersionId :: Core.Natural
    -- ^ Provides the current application version.
  , applicationCode :: Core.Maybe Types.ApplicationCode
    -- ^ Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
  , applicationDescription :: Core.Maybe Types.ApplicationDescription
    -- ^ Description of the application.
  , cloudWatchLoggingOptionDescriptions :: Core.Maybe [Types.CloudWatchLoggingOptionDescription]
    -- ^ Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> . 
  , createTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp when the application version was created.
  , inputDescriptions :: Core.Maybe [Types.InputDescription]
    -- ^ Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
  , lastUpdateTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp when the application was last updated.
  , outputDescriptions :: Core.Maybe [Types.OutputDescription]
    -- ^ Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> . 
  , referenceDataSourceDescriptions :: Core.Maybe [Types.ReferenceDataSourceDescription]
    -- ^ Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ApplicationDetail' value with any optional fields omitted.
mkApplicationDetail
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.ResourceARN -- ^ 'applicationARN'
    -> Types.ApplicationStatus -- ^ 'applicationStatus'
    -> Core.Natural -- ^ 'applicationVersionId'
    -> ApplicationDetail
mkApplicationDetail applicationName applicationARN
  applicationStatus applicationVersionId
  = ApplicationDetail'{applicationName, applicationARN,
                       applicationStatus, applicationVersionId,
                       applicationCode = Core.Nothing,
                       applicationDescription = Core.Nothing,
                       cloudWatchLoggingOptionDescriptions = Core.Nothing,
                       createTimestamp = Core.Nothing, inputDescriptions = Core.Nothing,
                       lastUpdateTimestamp = Core.Nothing,
                       outputDescriptions = Core.Nothing,
                       referenceDataSourceDescriptions = Core.Nothing}

-- | Name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationName :: Lens.Lens' ApplicationDetail Types.ApplicationName
adApplicationName = Lens.field @"applicationName"
{-# INLINEABLE adApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | ARN of the application.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationARN :: Lens.Lens' ApplicationDetail Types.ResourceARN
adApplicationARN = Lens.field @"applicationARN"
{-# INLINEABLE adApplicationARN #-}
{-# DEPRECATED applicationARN "Use generic-lens or generic-optics with 'applicationARN' instead"  #-}

-- | Status of the application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationStatus :: Lens.Lens' ApplicationDetail Types.ApplicationStatus
adApplicationStatus = Lens.field @"applicationStatus"
{-# INLINEABLE adApplicationStatus #-}
{-# DEPRECATED applicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead"  #-}

-- | Provides the current application version.
--
-- /Note:/ Consider using 'applicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationVersionId :: Lens.Lens' ApplicationDetail Core.Natural
adApplicationVersionId = Lens.field @"applicationVersionId"
{-# INLINEABLE adApplicationVersionId #-}
{-# DEPRECATED applicationVersionId "Use generic-lens or generic-optics with 'applicationVersionId' instead"  #-}

-- | Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
--
-- /Note:/ Consider using 'applicationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationCode :: Lens.Lens' ApplicationDetail (Core.Maybe Types.ApplicationCode)
adApplicationCode = Lens.field @"applicationCode"
{-# INLINEABLE adApplicationCode #-}
{-# DEPRECATED applicationCode "Use generic-lens or generic-optics with 'applicationCode' instead"  #-}

-- | Description of the application.
--
-- /Note:/ Consider using 'applicationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationDescription :: Lens.Lens' ApplicationDetail (Core.Maybe Types.ApplicationDescription)
adApplicationDescription = Lens.field @"applicationDescription"
{-# INLINEABLE adApplicationDescription #-}
{-# DEPRECATED applicationDescription "Use generic-lens or generic-optics with 'applicationDescription' instead"  #-}

-- | Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> . 
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCloudWatchLoggingOptionDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [Types.CloudWatchLoggingOptionDescription])
adCloudWatchLoggingOptionDescriptions = Lens.field @"cloudWatchLoggingOptionDescriptions"
{-# INLINEABLE adCloudWatchLoggingOptionDescriptions #-}
{-# DEPRECATED cloudWatchLoggingOptionDescriptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionDescriptions' instead"  #-}

-- | Time stamp when the application version was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCreateTimestamp :: Lens.Lens' ApplicationDetail (Core.Maybe Core.NominalDiffTime)
adCreateTimestamp = Lens.field @"createTimestamp"
{-# INLINEABLE adCreateTimestamp #-}
{-# DEPRECATED createTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead"  #-}

-- | Describes the application input configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
--
-- /Note:/ Consider using 'inputDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInputDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [Types.InputDescription])
adInputDescriptions = Lens.field @"inputDescriptions"
{-# INLINEABLE adInputDescriptions #-}
{-# DEPRECATED inputDescriptions "Use generic-lens or generic-optics with 'inputDescriptions' instead"  #-}

-- | Time stamp when the application was last updated.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastUpdateTimestamp :: Lens.Lens' ApplicationDetail (Core.Maybe Core.NominalDiffTime)
adLastUpdateTimestamp = Lens.field @"lastUpdateTimestamp"
{-# INLINEABLE adLastUpdateTimestamp #-}
{-# DEPRECATED lastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead"  #-}

-- | Describes the application output configuration. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> . 
--
-- /Note:/ Consider using 'outputDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adOutputDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [Types.OutputDescription])
adOutputDescriptions = Lens.field @"outputDescriptions"
{-# INLINEABLE adOutputDescriptions #-}
{-# DEPRECATED outputDescriptions "Use generic-lens or generic-optics with 'outputDescriptions' instead"  #-}

-- | Describes reference data sources configured for the application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . 
--
-- /Note:/ Consider using 'referenceDataSourceDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adReferenceDataSourceDescriptions :: Lens.Lens' ApplicationDetail (Core.Maybe [Types.ReferenceDataSourceDescription])
adReferenceDataSourceDescriptions = Lens.field @"referenceDataSourceDescriptions"
{-# INLINEABLE adReferenceDataSourceDescriptions #-}
{-# DEPRECATED referenceDataSourceDescriptions "Use generic-lens or generic-optics with 'referenceDataSourceDescriptions' instead"  #-}

instance Core.FromJSON ApplicationDetail where
        parseJSON
          = Core.withObject "ApplicationDetail" Core.$
              \ x ->
                ApplicationDetail' Core.<$>
                  (x Core..: "ApplicationName") Core.<*> x Core..: "ApplicationARN"
                    Core.<*> x Core..: "ApplicationStatus"
                    Core.<*> x Core..: "ApplicationVersionId"
                    Core.<*> x Core..:? "ApplicationCode"
                    Core.<*> x Core..:? "ApplicationDescription"
                    Core.<*> x Core..:? "CloudWatchLoggingOptionDescriptions"
                    Core.<*> x Core..:? "CreateTimestamp"
                    Core.<*> x Core..:? "InputDescriptions"
                    Core.<*> x Core..:? "LastUpdateTimestamp"
                    Core.<*> x Core..:? "OutputDescriptions"
                    Core.<*> x Core..:? "ReferenceDataSourceDescriptions"
