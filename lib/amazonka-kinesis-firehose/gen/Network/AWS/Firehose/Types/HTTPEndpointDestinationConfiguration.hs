{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationConfiguration
  ( HTTPEndpointDestinationConfiguration (..),

    -- * Smart constructor
    mkHTTPEndpointDestinationConfiguration,

    -- * Lenses
    httpedcS3BackupMode,
    httpedcS3Configuration,
    httpedcCloudWatchLoggingOptions,
    httpedcBufferingHints,
    httpedcRetryOptions,
    httpedcEndpointConfiguration,
    httpedcProcessingConfiguration,
    httpedcRequestConfiguration,
    httpedcRoleARN,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointDestinationConfiguration' smart constructor.
data HTTPEndpointDestinationConfiguration = HTTPEndpointDestinationConfiguration'
  { -- | Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
    s3BackupMode :: Lude.Maybe HTTPEndpointS3BackupMode,
    s3Configuration :: S3DestinationConfiguration,
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    -- | The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
    bufferingHints :: Lude.Maybe HTTPEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
    retryOptions :: Lude.Maybe HTTPEndpointRetryOptions,
    -- | The configuration of the HTTP endpoint selected as the destination.
    endpointConfiguration :: HTTPEndpointConfiguration,
    processingConfiguration :: Lude.Maybe ProcessingConfiguration,
    -- | The configuration of the requeste sent to the HTTP endpoint specified as the destination.
    requestConfiguration :: Lude.Maybe HTTPEndpointRequestConfiguration,
    -- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
-- * 's3Configuration' -
-- * 'cloudWatchLoggingOptions' -
-- * 'bufferingHints' - The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
-- * 'retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
-- * 'endpointConfiguration' - The configuration of the HTTP endpoint selected as the destination.
-- * 'processingConfiguration' -
-- * 'requestConfiguration' - The configuration of the requeste sent to the HTTP endpoint specified as the destination.
-- * 'roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
mkHTTPEndpointDestinationConfiguration ::
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  -- | 'endpointConfiguration'
  HTTPEndpointConfiguration ->
  HTTPEndpointDestinationConfiguration
mkHTTPEndpointDestinationConfiguration
  pS3Configuration_
  pEndpointConfiguration_ =
    HTTPEndpointDestinationConfiguration'
      { s3BackupMode =
          Lude.Nothing,
        s3Configuration = pS3Configuration_,
        cloudWatchLoggingOptions = Lude.Nothing,
        bufferingHints = Lude.Nothing,
        retryOptions = Lude.Nothing,
        endpointConfiguration = pEndpointConfiguration_,
        processingConfiguration = Lude.Nothing,
        requestConfiguration = Lude.Nothing,
        roleARN = Lude.Nothing
      }

-- | Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcS3BackupMode :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe HTTPEndpointS3BackupMode)
httpedcS3BackupMode = Lens.lens (s3BackupMode :: HTTPEndpointDestinationConfiguration -> Lude.Maybe HTTPEndpointS3BackupMode) (\s a -> s {s3BackupMode = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcS3Configuration :: Lens.Lens' HTTPEndpointDestinationConfiguration S3DestinationConfiguration
httpedcS3Configuration = Lens.lens (s3Configuration :: HTTPEndpointDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcCloudWatchLoggingOptions :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
httpedcCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: HTTPEndpointDestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcBufferingHints :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe HTTPEndpointBufferingHints)
httpedcBufferingHints = Lens.lens (bufferingHints :: HTTPEndpointDestinationConfiguration -> Lude.Maybe HTTPEndpointBufferingHints) (\s a -> s {bufferingHints = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcRetryOptions :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe HTTPEndpointRetryOptions)
httpedcRetryOptions = Lens.lens (retryOptions :: HTTPEndpointDestinationConfiguration -> Lude.Maybe HTTPEndpointRetryOptions) (\s a -> s {retryOptions = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The configuration of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcEndpointConfiguration :: Lens.Lens' HTTPEndpointDestinationConfiguration HTTPEndpointConfiguration
httpedcEndpointConfiguration = Lens.lens (endpointConfiguration :: HTTPEndpointDestinationConfiguration -> HTTPEndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcProcessingConfiguration :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe ProcessingConfiguration)
httpedcProcessingConfiguration = Lens.lens (processingConfiguration :: HTTPEndpointDestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The configuration of the requeste sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcRequestConfiguration :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe HTTPEndpointRequestConfiguration)
httpedcRequestConfiguration = Lens.lens (requestConfiguration :: HTTPEndpointDestinationConfiguration -> Lude.Maybe HTTPEndpointRequestConfiguration) (\s a -> s {requestConfiguration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcRequestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead." #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcRoleARN :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe Lude.Text)
httpedcRoleARN = Lens.lens (roleARN :: HTTPEndpointDestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON HTTPEndpointDestinationConfiguration where
  toJSON HTTPEndpointDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            Lude.Just ("S3Configuration" Lude..= s3Configuration),
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            Lude.Just ("EndpointConfiguration" Lude..= endpointConfiguration),
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("RequestConfiguration" Lude..=) Lude.<$> requestConfiguration,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
