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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointDestinationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointDescription
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import qualified Network.AWS.Lens as Lens

-- | Describes the HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationDescription' smart constructor.
data HttpEndpointDestinationDescription = HttpEndpointDestinationDescription'
  { -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Core.Maybe Core.Text,
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | The configuration of the specified HTTP endpoint destination.
    endpointConfiguration :: Core.Maybe HttpEndpointDescription,
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | The configuration of request sent to the HTTP endpoint specified as the
    -- destination.
    requestConfiguration :: Core.Maybe HttpEndpointRequestConfiguration,
    -- | Describes buffering options that can be applied to the data before it is
    -- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
    -- these options as hints, and it might choose to use more optimal values.
    -- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
    -- However, if specify a value for one of them, you must also provide a
    -- value for the other.
    bufferingHints :: Core.Maybe HttpEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to
    -- deliver data to the specified HTTP endpoint destination, or if it
    -- doesn\'t receive a valid acknowledgment of receipt from the specified
    -- HTTP endpoint destination.
    retryOptions :: Core.Maybe HttpEndpointRetryOptions,
    -- | Describes the S3 bucket backup options for the data that Kinesis
    -- Firehose delivers to the HTTP endpoint destination. You can back up all
    -- documents (@AllData@) or only the documents that Kinesis Data Firehose
    -- could not deliver to the specified HTTP endpoint destination
    -- (@FailedDataOnly@).
    s3BackupMode :: Core.Maybe HttpEndpointS3BackupMode,
    s3DestinationDescription :: Core.Maybe S3DestinationDescription
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'httpEndpointDestinationDescription_roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
--
-- 'processingConfiguration', 'httpEndpointDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'endpointConfiguration', 'httpEndpointDestinationDescription_endpointConfiguration' - The configuration of the specified HTTP endpoint destination.
--
-- 'cloudWatchLoggingOptions', 'httpEndpointDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'requestConfiguration', 'httpEndpointDestinationDescription_requestConfiguration' - The configuration of request sent to the HTTP endpoint specified as the
-- destination.
--
-- 'bufferingHints', 'httpEndpointDestinationDescription_bufferingHints' - Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
--
-- 'retryOptions', 'httpEndpointDestinationDescription_retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
--
-- 's3BackupMode', 'httpEndpointDestinationDescription_s3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
--
-- 's3DestinationDescription', 'httpEndpointDestinationDescription_s3DestinationDescription' - Undocumented member.
newHttpEndpointDestinationDescription ::
  HttpEndpointDestinationDescription
newHttpEndpointDestinationDescription =
  HttpEndpointDestinationDescription'
    { roleARN =
        Core.Nothing,
      processingConfiguration = Core.Nothing,
      endpointConfiguration = Core.Nothing,
      cloudWatchLoggingOptions = Core.Nothing,
      requestConfiguration = Core.Nothing,
      bufferingHints = Core.Nothing,
      retryOptions = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3DestinationDescription = Core.Nothing
    }

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationDescription_roleARN :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe Core.Text)
httpEndpointDestinationDescription_roleARN = Lens.lens (\HttpEndpointDestinationDescription' {roleARN} -> roleARN) (\s@HttpEndpointDestinationDescription' {} a -> s {roleARN = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_processingConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe ProcessingConfiguration)
httpEndpointDestinationDescription_processingConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationDescription)

-- | The configuration of the specified HTTP endpoint destination.
httpEndpointDestinationDescription_endpointConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe HttpEndpointDescription)
httpEndpointDestinationDescription_endpointConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {endpointConfiguration} -> endpointConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {endpointConfiguration = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationDescription)

-- | The configuration of request sent to the HTTP endpoint specified as the
-- destination.
httpEndpointDestinationDescription_requestConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationDescription_requestConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationDescription)

-- | Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
httpEndpointDestinationDescription_bufferingHints :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationDescription_bufferingHints = Lens.lens (\HttpEndpointDestinationDescription' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationDescription' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationDescription)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationDescription_retryOptions :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationDescription_retryOptions = Lens.lens (\HttpEndpointDestinationDescription' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationDescription' {} a -> s {retryOptions = a} :: HttpEndpointDestinationDescription)

-- | Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
httpEndpointDestinationDescription_s3BackupMode :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe HttpEndpointS3BackupMode)
httpEndpointDestinationDescription_s3BackupMode = Lens.lens (\HttpEndpointDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@HttpEndpointDestinationDescription' {} a -> s {s3BackupMode = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_s3DestinationDescription :: Lens.Lens' HttpEndpointDestinationDescription (Core.Maybe S3DestinationDescription)
httpEndpointDestinationDescription_s3DestinationDescription = Lens.lens (\HttpEndpointDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@HttpEndpointDestinationDescription' {} a -> s {s3DestinationDescription = a} :: HttpEndpointDestinationDescription)

instance
  Core.FromJSON
    HttpEndpointDestinationDescription
  where
  parseJSON =
    Core.withObject
      "HttpEndpointDestinationDescription"
      ( \x ->
          HttpEndpointDestinationDescription'
            Core.<$> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "ProcessingConfiguration")
            Core.<*> (x Core..:? "EndpointConfiguration")
            Core.<*> (x Core..:? "CloudWatchLoggingOptions")
            Core.<*> (x Core..:? "RequestConfiguration")
            Core.<*> (x Core..:? "BufferingHints")
            Core.<*> (x Core..:? "RetryOptions")
            Core.<*> (x Core..:? "S3BackupMode")
            Core.<*> (x Core..:? "S3DestinationDescription")
      )

instance
  Core.Hashable
    HttpEndpointDestinationDescription

instance
  Core.NFData
    HttpEndpointDestinationDescription
