{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SplunkDestinationDescription
  ( SplunkDestinationDescription (..)
  -- * Smart constructor
  , mkSplunkDestinationDescription
  -- * Lenses
  , sddfCloudWatchLoggingOptions
  , sddfHECAcknowledgmentTimeoutInSeconds
  , sddfHECEndpoint
  , sddfHECEndpointType
  , sddfHECToken
  , sddfProcessingConfiguration
  , sddfRetryOptions
  , sddfS3BackupMode
  , sddfS3DestinationDescription
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.HECEndpoint as Types
import qualified Network.AWS.Firehose.Types.HECEndpointType as Types
import qualified Network.AWS.Firehose.Types.HECToken as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.SplunkRetryOptions as Types
import qualified Network.AWS.Firehose.Types.SplunkS3BackupMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a destination in Splunk.
--
-- /See:/ 'mkSplunkDestinationDescription' smart constructor.
data SplunkDestinationDescription = SplunkDestinationDescription'
  { cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The Amazon CloudWatch logging options for your delivery stream.
  , hECAcknowledgmentTimeoutInSeconds :: Core.Maybe Core.Natural
    -- ^ The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
  , hECEndpoint :: Core.Maybe Types.HECEndpoint
    -- ^ The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
  , hECEndpointType :: Core.Maybe Types.HECEndpointType
    -- ^ This type can be either "Raw" or "Event."
  , hECToken :: Core.Maybe Types.HECToken
    -- ^ A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
    -- ^ The data processing configuration.
  , retryOptions :: Core.Maybe Types.SplunkRetryOptions
    -- ^ The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
  , s3BackupMode :: Core.Maybe Types.SplunkS3BackupMode
    -- ^ Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ . 
  , s3DestinationDescription :: Core.Maybe Types.S3DestinationDescription
    -- ^ The Amazon S3 destination.>
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SplunkDestinationDescription' value with any optional fields omitted.
mkSplunkDestinationDescription
    :: SplunkDestinationDescription
mkSplunkDestinationDescription
  = SplunkDestinationDescription'{cloudWatchLoggingOptions =
                                    Core.Nothing,
                                  hECAcknowledgmentTimeoutInSeconds = Core.Nothing,
                                  hECEndpoint = Core.Nothing, hECEndpointType = Core.Nothing,
                                  hECToken = Core.Nothing, processingConfiguration = Core.Nothing,
                                  retryOptions = Core.Nothing, s3BackupMode = Core.Nothing,
                                  s3DestinationDescription = Core.Nothing}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfCloudWatchLoggingOptions :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
sddfCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE sddfCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- /Note:/ Consider using 'hECAcknowledgmentTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfHECAcknowledgmentTimeoutInSeconds :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Core.Natural)
sddfHECAcknowledgmentTimeoutInSeconds = Lens.field @"hECAcknowledgmentTimeoutInSeconds"
{-# INLINEABLE sddfHECAcknowledgmentTimeoutInSeconds #-}
{-# DEPRECATED hECAcknowledgmentTimeoutInSeconds "Use generic-lens or generic-optics with 'hECAcknowledgmentTimeoutInSeconds' instead"  #-}

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- /Note:/ Consider using 'hECEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfHECEndpoint :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.HECEndpoint)
sddfHECEndpoint = Lens.field @"hECEndpoint"
{-# INLINEABLE sddfHECEndpoint #-}
{-# DEPRECATED hECEndpoint "Use generic-lens or generic-optics with 'hECEndpoint' instead"  #-}

-- | This type can be either "Raw" or "Event."
--
-- /Note:/ Consider using 'hECEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfHECEndpointType :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.HECEndpointType)
sddfHECEndpointType = Lens.field @"hECEndpointType"
{-# INLINEABLE sddfHECEndpointType #-}
{-# DEPRECATED hECEndpointType "Use generic-lens or generic-optics with 'hECEndpointType' instead"  #-}

-- | A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- /Note:/ Consider using 'hECToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfHECToken :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.HECToken)
sddfHECToken = Lens.field @"hECToken"
{-# INLINEABLE sddfHECToken #-}
{-# DEPRECATED hECToken "Use generic-lens or generic-optics with 'hECToken' instead"  #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfProcessingConfiguration :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.ProcessingConfiguration)
sddfProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE sddfProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfRetryOptions :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.SplunkRetryOptions)
sddfRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE sddfRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ . 
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfS3BackupMode :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.SplunkS3BackupMode)
sddfS3BackupMode = Lens.field @"s3BackupMode"
{-# INLINEABLE sddfS3BackupMode #-}
{-# DEPRECATED s3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead"  #-}

-- | The Amazon S3 destination.>
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sddfS3DestinationDescription :: Lens.Lens' SplunkDestinationDescription (Core.Maybe Types.S3DestinationDescription)
sddfS3DestinationDescription = Lens.field @"s3DestinationDescription"
{-# INLINEABLE sddfS3DestinationDescription #-}
{-# DEPRECATED s3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead"  #-}

instance Core.FromJSON SplunkDestinationDescription where
        parseJSON
          = Core.withObject "SplunkDestinationDescription" Core.$
              \ x ->
                SplunkDestinationDescription' Core.<$>
                  (x Core..:? "CloudWatchLoggingOptions") Core.<*>
                    x Core..:? "HECAcknowledgmentTimeoutInSeconds"
                    Core.<*> x Core..:? "HECEndpoint"
                    Core.<*> x Core..:? "HECEndpointType"
                    Core.<*> x Core..:? "HECToken"
                    Core.<*> x Core..:? "ProcessingConfiguration"
                    Core.<*> x Core..:? "RetryOptions"
                    Core.<*> x Core..:? "S3BackupMode"
                    Core.<*> x Core..:? "S3DestinationDescription"
