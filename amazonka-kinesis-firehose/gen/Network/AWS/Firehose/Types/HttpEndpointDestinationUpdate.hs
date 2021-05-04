{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Updates the specified HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationUpdate' smart constructor.
data HttpEndpointDestinationUpdate = HttpEndpointDestinationUpdate'
  { -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Prelude.Maybe Prelude.Text,
    s3Update :: Prelude.Maybe S3DestinationUpdate,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | Describes the configuration of the HTTP endpoint destination.
    endpointConfiguration :: Prelude.Maybe HttpEndpointConfiguration,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The configuration of the request sent to the HTTP endpoint specified as
    -- the destination.
    requestConfiguration :: Prelude.Maybe HttpEndpointRequestConfiguration,
    -- | Describes buffering options that can be applied to the data before it is
    -- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
    -- these options as hints, and it might choose to use more optimal values.
    -- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
    -- However, if specify a value for one of them, you must also provide a
    -- value for the other.
    bufferingHints :: Prelude.Maybe HttpEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to
    -- deliver data to the specified HTTP endpoint destination, or if it
    -- doesn\'t receive a valid acknowledgment of receipt from the specified
    -- HTTP endpoint destination.
    retryOptions :: Prelude.Maybe HttpEndpointRetryOptions,
    -- | Describes the S3 bucket backup options for the data that Kinesis
    -- Firehose delivers to the HTTP endpoint destination. You can back up all
    -- documents (@AllData@) or only the documents that Kinesis Data Firehose
    -- could not deliver to the specified HTTP endpoint destination
    -- (@FailedDataOnly@).
    s3BackupMode :: Prelude.Maybe HttpEndpointS3BackupMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      s3Update = Prelude.Nothing,
      processingConfiguration = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      cloudWatchLoggingOptions = Prelude.Nothing,
      requestConfiguration = Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      s3BackupMode = Prelude.Nothing
    }

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationUpdate_roleARN :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe Prelude.Text)
httpEndpointDestinationUpdate_roleARN = Lens.lens (\HttpEndpointDestinationUpdate' {roleARN} -> roleARN) (\s@HttpEndpointDestinationUpdate' {} a -> s {roleARN = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_s3Update :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe S3DestinationUpdate)
httpEndpointDestinationUpdate_s3Update = Lens.lens (\HttpEndpointDestinationUpdate' {s3Update} -> s3Update) (\s@HttpEndpointDestinationUpdate' {} a -> s {s3Update = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_processingConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe ProcessingConfiguration)
httpEndpointDestinationUpdate_processingConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Describes the configuration of the HTTP endpoint destination.
httpEndpointDestinationUpdate_endpointConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe HttpEndpointConfiguration)
httpEndpointDestinationUpdate_endpointConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {endpointConfiguration} -> endpointConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {endpointConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Undocumented member.
httpEndpointDestinationUpdate_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationUpdate_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationUpdate' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationUpdate' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationUpdate)

-- | The configuration of the request sent to the HTTP endpoint specified as
-- the destination.
httpEndpointDestinationUpdate_requestConfiguration :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationUpdate_requestConfiguration = Lens.lens (\HttpEndpointDestinationUpdate' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationUpdate' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationUpdate)

-- | Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
httpEndpointDestinationUpdate_bufferingHints :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationUpdate_bufferingHints = Lens.lens (\HttpEndpointDestinationUpdate' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationUpdate' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationUpdate)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationUpdate_retryOptions :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationUpdate_retryOptions = Lens.lens (\HttpEndpointDestinationUpdate' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationUpdate' {} a -> s {retryOptions = a} :: HttpEndpointDestinationUpdate)

-- | Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
httpEndpointDestinationUpdate_s3BackupMode :: Lens.Lens' HttpEndpointDestinationUpdate (Prelude.Maybe HttpEndpointS3BackupMode)
httpEndpointDestinationUpdate_s3BackupMode = Lens.lens (\HttpEndpointDestinationUpdate' {s3BackupMode} -> s3BackupMode) (\s@HttpEndpointDestinationUpdate' {} a -> s {s3BackupMode = a} :: HttpEndpointDestinationUpdate)

instance
  Prelude.Hashable
    HttpEndpointDestinationUpdate

instance Prelude.NFData HttpEndpointDestinationUpdate

instance Prelude.ToJSON HttpEndpointDestinationUpdate where
  toJSON HttpEndpointDestinationUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("S3Update" Prelude..=) Prelude.<$> s3Update,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("EndpointConfiguration" Prelude..=)
              Prelude.<$> endpointConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("RequestConfiguration" Prelude..=)
              Prelude.<$> requestConfiguration,
            ("BufferingHints" Prelude..=)
              Prelude.<$> bufferingHints,
            ("RetryOptions" Prelude..=) Prelude.<$> retryOptions,
            ("S3BackupMode" Prelude..=)
              Prelude.<$> s3BackupMode
          ]
      )
