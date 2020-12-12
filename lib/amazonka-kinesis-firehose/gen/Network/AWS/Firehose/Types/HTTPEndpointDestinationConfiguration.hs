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
    httpedcCloudWatchLoggingOptions,
    httpedcBufferingHints,
    httpedcRetryOptions,
    httpedcProcessingConfiguration,
    httpedcRequestConfiguration,
    httpedcRoleARN,
    httpedcEndpointConfiguration,
    httpedcS3Configuration,
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
  { s3BackupMode ::
      Lude.Maybe
        HTTPEndpointS3BackupMode,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    bufferingHints ::
      Lude.Maybe
        HTTPEndpointBufferingHints,
    retryOptions ::
      Lude.Maybe
        HTTPEndpointRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    requestConfiguration ::
      Lude.Maybe
        HTTPEndpointRequestConfiguration,
    roleARN ::
      Lude.Maybe
        Lude.Text,
    endpointConfiguration ::
      HTTPEndpointConfiguration,
    s3Configuration ::
      S3DestinationConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'bufferingHints' - The buffering options that can be used before data is delivered to the specified destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if you specify a value for one of them, you must also provide a value for the other.
-- * 'cloudWatchLoggingOptions' - Undocumented field.
-- * 'endpointConfiguration' - The configuration of the HTTP endpoint selected as the destination.
-- * 'processingConfiguration' - Undocumented field.
-- * 'requestConfiguration' - The configuration of the requeste sent to the HTTP endpoint specified as the destination.
-- * 'retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
-- * 'roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
-- * 's3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
-- * 's3Configuration' - Undocumented field.
mkHTTPEndpointDestinationConfiguration ::
  -- | 'endpointConfiguration'
  HTTPEndpointConfiguration ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  HTTPEndpointDestinationConfiguration
mkHTTPEndpointDestinationConfiguration
  pEndpointConfiguration_
  pS3Configuration_ =
    HTTPEndpointDestinationConfiguration'
      { s3BackupMode =
          Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        bufferingHints = Lude.Nothing,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        requestConfiguration = Lude.Nothing,
        roleARN = Lude.Nothing,
        endpointConfiguration = pEndpointConfiguration_,
        s3Configuration = pS3Configuration_
      }

-- | Describes the S3 bucket backup options for the data that Kinesis Data Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcS3BackupMode :: Lens.Lens' HTTPEndpointDestinationConfiguration (Lude.Maybe HTTPEndpointS3BackupMode)
httpedcS3BackupMode = Lens.lens (s3BackupMode :: HTTPEndpointDestinationConfiguration -> Lude.Maybe HTTPEndpointS3BackupMode) (\s a -> s {s3BackupMode = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

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

-- | The configuration of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcEndpointConfiguration :: Lens.Lens' HTTPEndpointDestinationConfiguration HTTPEndpointConfiguration
httpedcEndpointConfiguration = Lens.lens (endpointConfiguration :: HTTPEndpointDestinationConfiguration -> HTTPEndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedcS3Configuration :: Lens.Lens' HTTPEndpointDestinationConfiguration S3DestinationConfiguration
httpedcS3Configuration = Lens.lens (s3Configuration :: HTTPEndpointDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: HTTPEndpointDestinationConfiguration)
{-# DEPRECATED httpedcS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

instance Lude.ToJSON HTTPEndpointDestinationConfiguration where
  toJSON HTTPEndpointDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("RequestConfiguration" Lude..=) Lude.<$> requestConfiguration,
            ("RoleARN" Lude..=) Lude.<$> roleARN,
            Lude.Just ("EndpointConfiguration" Lude..= endpointConfiguration),
            Lude.Just ("S3Configuration" Lude..= s3Configuration)
          ]
      )
