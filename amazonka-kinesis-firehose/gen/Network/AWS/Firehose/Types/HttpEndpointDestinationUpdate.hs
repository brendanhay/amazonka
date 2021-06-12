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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens

-- | Updates the specified HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationUpdate' smart constructor.
data HttpEndpointDestinationUpdate = HttpEndpointDestinationUpdate'
  { -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Core.Maybe Core.Text,
    s3Update :: Core.Maybe S3DestinationUpdate,
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | Describes the configuration of the HTTP endpoint destination.
    endpointConfiguration :: Core.Maybe HttpEndpointConfiguration,
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | The configuration of the request sent to the HTTP endpoint specified as
    -- the destination.
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
    s3BackupMode :: Core.Maybe HttpEndpointS3BackupMode
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointDestinationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'httpEndpointDestinationUpdate_roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
--
-- 's3Update', 'httpEndpointDestinationUpdate_s3Update' - Undocumented member.
--
-- 'processingConfiguration', 'httpEndpointDestinationUpdate_processingConfiguration' - Undocumented member.
--
-- 'endpointConfiguration', 'httpEndpointDestinationUpdate_endpointConfiguration' - Describes the configuration of the HTTP endpoint destination.
--
-- 'cloudWatchLoggingOptions', 'httpEndpointDestinationUpdate_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'requestConfiguration', 'httpEndpointDestinationUpdate_requestConfiguration' - The configuration of the request sent to the HTTP endpoint specified as
-- the destination.
--
-- 'bufferingHints', 'httpEndpointDestinationUpdate_bufferingHints' - Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
--
-- 'retryOptions', 'httpEndpointDestinationUpdate_retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
--
-- 's3BackupMode', 'httpEndpointDestinationUpdate_s3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
newHttpEndpointDestinationUpdate ::
  HttpEndpointDestinationUpdate
newHttpEndpointDestinationUpdate =
  HttpEndpointDestinationUpdate'
    { roleARN =
        Core.Nothing,
      s3Update = Core.Nothing,
      processingConfiguration = Core.Nothing,
      endpointConfiguration = Core.Nothing,
      cloudWatchLoggingOptions = Core.Nothing,
      requestConfiguration = Core.Nothing,
      bufferingHints = Core.Nothing,
      retryOptions = Core.Nothing,
      s3BackupMode = Core.Nothing
    }

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationUpdate_roleARN :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe Core.Text)
httpEndpointDestinationUpdate_roleARN = Lens.lens (\HttpEndpointDestinationUpdate' {roleARN} -> roleARN) (\s@HttpEndpointDestinationUpdate' {} a -> s {roleARN = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_s3Update :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe S3DestinationUpdate)
httpEndpointDestinationUpdate_s3Update = Lens.lens (\HttpEndpointDestinationUpdate' {s3Update} -> s3Update) (\s@HttpEndpointDestinationUpdate' {} a -> s {s3Update = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_processingConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe ProcessingConfiguration)
httpEndpointDestinationUpdate_processingConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Describes the configuration of the HTTP endpoint destination.
httpEndpointDestinationUpdate_endpointConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe HttpEndpointConfiguration)
httpEndpointDestinationUpdate_endpointConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {endpointConfiguration} -> endpointConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {endpointConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationUpdate)

-- | The configuration of the request sent to the HTTP endpoint specified as
-- the destination.
httpEndpointDestinationUpdate_requestConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationUpdate_requestConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
httpEndpointDestinationUpdate_bufferingHints :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationUpdate_bufferingHints = Lens.lens (\HttpEndpointDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationUpdate' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationUpdate)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationUpdate_retryOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationUpdate_retryOptions = Lens.lens (\HttpEndpointDestinationUpdate' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationUpdate' {} a -> s {retryOptions = a} :: HttpEndpointDestinationUpdate)

-- | Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
httpEndpointDestinationUpdate_s3BackupMode :: Lens.Lens' HttpEndpointDestinationUpdate (Core.Maybe HttpEndpointS3BackupMode)
httpEndpointDestinationUpdate_s3BackupMode = Lens.lens (\HttpEndpointDestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@HttpEndpointDestinationUpdate' {} a -> s {s3BackupMode = a} :: HttpEndpointDestinationUpdate)

instance Core.Hashable HttpEndpointDestinationUpdate

instance Core.NFData HttpEndpointDestinationUpdate

instance Core.ToJSON HttpEndpointDestinationUpdate where
  toJSON HttpEndpointDestinationUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleARN" Core..=) Core.<$> roleARN,
            ("S3Update" Core..=) Core.<$> s3Update,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("EndpointConfiguration" Core..=)
              Core.<$> endpointConfiguration,
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("RequestConfiguration" Core..=)
              Core.<$> requestConfiguration,
            ("BufferingHints" Core..=) Core.<$> bufferingHints,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode
          ]
      )
