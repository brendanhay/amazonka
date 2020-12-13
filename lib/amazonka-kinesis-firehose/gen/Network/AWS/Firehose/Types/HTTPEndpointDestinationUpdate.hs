{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationUpdate
  ( HTTPEndpointDestinationUpdate (..),

    -- * Smart constructor
    mkHTTPEndpointDestinationUpdate,

    -- * Lenses
    httpeduS3BackupMode,
    httpeduCloudWatchLoggingOptions,
    httpeduS3Update,
    httpeduBufferingHints,
    httpeduRetryOptions,
    httpeduEndpointConfiguration,
    httpeduProcessingConfiguration,
    httpeduRequestConfiguration,
    httpeduRoleARN,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Updates the specified HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointDestinationUpdate' smart constructor.
data HTTPEndpointDestinationUpdate = HTTPEndpointDestinationUpdate'
  { -- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
    s3BackupMode :: Lude.Maybe HTTPEndpointS3BackupMode,
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    s3Update :: Lude.Maybe S3DestinationUpdate,
    -- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
    bufferingHints :: Lude.Maybe HTTPEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
    retryOptions :: Lude.Maybe HTTPEndpointRetryOptions,
    -- | Describes the configuration of the HTTP endpoint destination.
    endpointConfiguration :: Lude.Maybe HTTPEndpointConfiguration,
    processingConfiguration :: Lude.Maybe ProcessingConfiguration,
    -- | The configuration of the request sent to the HTTP endpoint specified as the destination.
    requestConfiguration :: Lude.Maybe HTTPEndpointRequestConfiguration,
    -- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointDestinationUpdate' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
-- * 'cloudWatchLoggingOptions' -
-- * 's3Update' -
-- * 'bufferingHints' - Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
-- * 'retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
-- * 'endpointConfiguration' - Describes the configuration of the HTTP endpoint destination.
-- * 'processingConfiguration' -
-- * 'requestConfiguration' - The configuration of the request sent to the HTTP endpoint specified as the destination.
-- * 'roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
mkHTTPEndpointDestinationUpdate ::
  HTTPEndpointDestinationUpdate
mkHTTPEndpointDestinationUpdate =
  HTTPEndpointDestinationUpdate'
    { s3BackupMode = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      s3Update = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      retryOptions = Lude.Nothing,
      endpointConfiguration = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      requestConfiguration = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduS3BackupMode :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe HTTPEndpointS3BackupMode)
httpeduS3BackupMode = Lens.lens (s3BackupMode :: HTTPEndpointDestinationUpdate -> Lude.Maybe HTTPEndpointS3BackupMode) (\s a -> s {s3BackupMode = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduCloudWatchLoggingOptions :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
httpeduCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: HTTPEndpointDestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduS3Update :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe S3DestinationUpdate)
httpeduS3Update = Lens.lens (s3Update :: HTTPEndpointDestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3Update = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduBufferingHints :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe HTTPEndpointBufferingHints)
httpeduBufferingHints = Lens.lens (bufferingHints :: HTTPEndpointDestinationUpdate -> Lude.Maybe HTTPEndpointBufferingHints) (\s a -> s {bufferingHints = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduRetryOptions :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe HTTPEndpointRetryOptions)
httpeduRetryOptions = Lens.lens (retryOptions :: HTTPEndpointDestinationUpdate -> Lude.Maybe HTTPEndpointRetryOptions) (\s a -> s {retryOptions = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduEndpointConfiguration :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe HTTPEndpointConfiguration)
httpeduEndpointConfiguration = Lens.lens (endpointConfiguration :: HTTPEndpointDestinationUpdate -> Lude.Maybe HTTPEndpointConfiguration) (\s a -> s {endpointConfiguration = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduProcessingConfiguration :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe ProcessingConfiguration)
httpeduProcessingConfiguration = Lens.lens (processingConfiguration :: HTTPEndpointDestinationUpdate -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The configuration of the request sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduRequestConfiguration :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe HTTPEndpointRequestConfiguration)
httpeduRequestConfiguration = Lens.lens (requestConfiguration :: HTTPEndpointDestinationUpdate -> Lude.Maybe HTTPEndpointRequestConfiguration) (\s a -> s {requestConfiguration = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduRequestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead." #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeduRoleARN :: Lens.Lens' HTTPEndpointDestinationUpdate (Lude.Maybe Lude.Text)
httpeduRoleARN = Lens.lens (roleARN :: HTTPEndpointDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: HTTPEndpointDestinationUpdate)
{-# DEPRECATED httpeduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON HTTPEndpointDestinationUpdate where
  toJSON HTTPEndpointDestinationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("S3Update" Lude..=) Lude.<$> s3Update,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("EndpointConfiguration" Lude..=) Lude.<$> endpointConfiguration,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("RequestConfiguration" Lude..=) Lude.<$> requestConfiguration,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
