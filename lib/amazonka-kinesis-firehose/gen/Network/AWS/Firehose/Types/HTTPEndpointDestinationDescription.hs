{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDestinationDescription
  ( HTTPEndpointDestinationDescription (..),

    -- * Smart constructor
    mkHTTPEndpointDestinationDescription,

    -- * Lenses
    httpeddS3BackupMode,
    httpeddCloudWatchLoggingOptions,
    httpeddS3DestinationDescription,
    httpeddBufferingHints,
    httpeddRetryOptions,
    httpeddEndpointConfiguration,
    httpeddProcessingConfiguration,
    httpeddRequestConfiguration,
    httpeddRoleARN,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
import Network.AWS.Firehose.Types.HTTPEndpointDescription
import Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HTTPEndpointRetryOptions
import Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointDestinationDescription' smart constructor.
data HTTPEndpointDestinationDescription = HTTPEndpointDestinationDescription'
  { -- | Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
    s3BackupMode :: Lude.Maybe HTTPEndpointS3BackupMode,
    cloudWatchLoggingOptions :: Lude.Maybe CloudWatchLoggingOptions,
    s3DestinationDescription :: Lude.Maybe S3DestinationDescription,
    -- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
    bufferingHints :: Lude.Maybe HTTPEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
    retryOptions :: Lude.Maybe HTTPEndpointRetryOptions,
    -- | The configuration of the specified HTTP endpoint destination.
    endpointConfiguration :: Lude.Maybe HTTPEndpointDescription,
    processingConfiguration :: Lude.Maybe ProcessingConfiguration,
    -- | The configuration of request sent to the HTTP endpoint specified as the destination.
    requestConfiguration :: Lude.Maybe HTTPEndpointRequestConfiguration,
    -- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointDestinationDescription' with the minimum fields required to make a request.
--
-- * 's3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Firehose delivers to the HTTP endpoint destination. You can back up all documents (@AllData@ ) or only the documents that Kinesis Data Firehose could not deliver to the specified HTTP endpoint destination (@FailedDataOnly@ ).
-- * 'cloudWatchLoggingOptions' -
-- * 's3DestinationDescription' -
-- * 'bufferingHints' - Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
-- * 'retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
-- * 'endpointConfiguration' - The configuration of the specified HTTP endpoint destination.
-- * 'processingConfiguration' -
-- * 'requestConfiguration' - The configuration of request sent to the HTTP endpoint specified as the destination.
-- * 'roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
mkHTTPEndpointDestinationDescription ::
  HTTPEndpointDestinationDescription
mkHTTPEndpointDestinationDescription =
  HTTPEndpointDestinationDescription'
    { s3BackupMode = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      s3DestinationDescription = Lude.Nothing,
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
httpeddS3BackupMode :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe HTTPEndpointS3BackupMode)
httpeddS3BackupMode = Lens.lens (s3BackupMode :: HTTPEndpointDestinationDescription -> Lude.Maybe HTTPEndpointS3BackupMode) (\s a -> s {s3BackupMode = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddCloudWatchLoggingOptions :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
httpeddCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: HTTPEndpointDestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddS3DestinationDescription :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe S3DestinationDescription)
httpeddS3DestinationDescription = Lens.lens (s3DestinationDescription :: HTTPEndpointDestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3DestinationDescription = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | Describes buffering options that can be applied to the data before it is delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddBufferingHints :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe HTTPEndpointBufferingHints)
httpeddBufferingHints = Lens.lens (bufferingHints :: HTTPEndpointDestinationDescription -> Lude.Maybe HTTPEndpointBufferingHints) (\s a -> s {bufferingHints = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to deliver data to the specified HTTP endpoint destination, or if it doesn't receive a valid acknowledgment of receipt from the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddRetryOptions :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe HTTPEndpointRetryOptions)
httpeddRetryOptions = Lens.lens (retryOptions :: HTTPEndpointDestinationDescription -> Lude.Maybe HTTPEndpointRetryOptions) (\s a -> s {retryOptions = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The configuration of the specified HTTP endpoint destination.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddEndpointConfiguration :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe HTTPEndpointDescription)
httpeddEndpointConfiguration = Lens.lens (endpointConfiguration :: HTTPEndpointDestinationDescription -> Lude.Maybe HTTPEndpointDescription) (\s a -> s {endpointConfiguration = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddEndpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddProcessingConfiguration :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe ProcessingConfiguration)
httpeddProcessingConfiguration = Lens.lens (processingConfiguration :: HTTPEndpointDestinationDescription -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The configuration of request sent to the HTTP endpoint specified as the destination.
--
-- /Note:/ Consider using 'requestConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddRequestConfiguration :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe HTTPEndpointRequestConfiguration)
httpeddRequestConfiguration = Lens.lens (requestConfiguration :: HTTPEndpointDestinationDescription -> Lude.Maybe HTTPEndpointRequestConfiguration) (\s a -> s {requestConfiguration = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddRequestConfiguration "Use generic-lens or generic-optics with 'requestConfiguration' instead." #-}

-- | Kinesis Data Firehose uses this IAM role for all the permissions that the delivery stream needs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpeddRoleARN :: Lens.Lens' HTTPEndpointDestinationDescription (Lude.Maybe Lude.Text)
httpeddRoleARN = Lens.lens (roleARN :: HTTPEndpointDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: HTTPEndpointDestinationDescription)
{-# DEPRECATED httpeddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON HTTPEndpointDestinationDescription where
  parseJSON =
    Lude.withObject
      "HTTPEndpointDestinationDescription"
      ( \x ->
          HTTPEndpointDestinationDescription'
            Lude.<$> (x Lude..:? "S3BackupMode")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "S3DestinationDescription")
            Lude.<*> (x Lude..:? "BufferingHints")
            Lude.<*> (x Lude..:? "RetryOptions")
            Lude.<*> (x Lude..:? "EndpointConfiguration")
            Lude.<*> (x Lude..:? "ProcessingConfiguration")
            Lude.<*> (x Lude..:? "RequestConfiguration")
            Lude.<*> (x Lude..:? "RoleARN")
      )
