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
-- Module      : Amazonka.Firehose.Types.HttpEndpointDestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.HttpEndpointBufferingHints
import Amazonka.Firehose.Types.HttpEndpointDescription
import Amazonka.Firehose.Types.HttpEndpointRequestConfiguration
import Amazonka.Firehose.Types.HttpEndpointRetryOptions
import Amazonka.Firehose.Types.HttpEndpointS3BackupMode
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes the HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationDescription' smart constructor.
data HttpEndpointDestinationDescription = HttpEndpointDestinationDescription'
  { -- | The configuration of request sent to the HTTP endpoint specified as the
    -- destination.
    requestConfiguration :: Prelude.Maybe HttpEndpointRequestConfiguration,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Describes buffering options that can be applied to the data before it is
    -- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
    -- these options as hints, and it might choose to use more optimal values.
    -- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
    -- However, if specify a value for one of them, you must also provide a
    -- value for the other.
    bufferingHints :: Prelude.Maybe HttpEndpointBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | Describes the S3 bucket backup options for the data that Kinesis
    -- Firehose delivers to the HTTP endpoint destination. You can back up all
    -- documents (@AllData@) or only the documents that Kinesis Data Firehose
    -- could not deliver to the specified HTTP endpoint destination
    -- (@FailedDataOnly@).
    s3BackupMode :: Prelude.Maybe HttpEndpointS3BackupMode,
    -- | The configuration of the specified HTTP endpoint destination.
    endpointConfiguration :: Prelude.Maybe HttpEndpointDescription,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to
    -- deliver data to the specified HTTP endpoint destination, or if it
    -- doesn\'t receive a valid acknowledgment of receipt from the specified
    -- HTTP endpoint destination.
    retryOptions :: Prelude.Maybe HttpEndpointRetryOptions,
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestConfiguration', 'httpEndpointDestinationDescription_requestConfiguration' - The configuration of request sent to the HTTP endpoint specified as the
-- destination.
--
-- 'processingConfiguration', 'httpEndpointDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'roleARN', 'httpEndpointDestinationDescription_roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
--
-- 'bufferingHints', 'httpEndpointDestinationDescription_bufferingHints' - Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
--
-- 'cloudWatchLoggingOptions', 'httpEndpointDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 's3BackupMode', 'httpEndpointDestinationDescription_s3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
--
-- 'endpointConfiguration', 'httpEndpointDestinationDescription_endpointConfiguration' - The configuration of the specified HTTP endpoint destination.
--
-- 'retryOptions', 'httpEndpointDestinationDescription_retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
--
-- 's3DestinationDescription', 'httpEndpointDestinationDescription_s3DestinationDescription' - Undocumented member.
newHttpEndpointDestinationDescription ::
  HttpEndpointDestinationDescription
newHttpEndpointDestinationDescription =
  HttpEndpointDestinationDescription'
    { requestConfiguration =
        Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing
    }

-- | The configuration of request sent to the HTTP endpoint specified as the
-- destination.
httpEndpointDestinationDescription_requestConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationDescription_requestConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_processingConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe ProcessingConfiguration)
httpEndpointDestinationDescription_processingConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationDescription)

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationDescription_roleARN :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe Prelude.Text)
httpEndpointDestinationDescription_roleARN = Lens.lens (\HttpEndpointDestinationDescription' {roleARN} -> roleARN) (\s@HttpEndpointDestinationDescription' {} a -> s {roleARN = a} :: HttpEndpointDestinationDescription)

-- | Describes buffering options that can be applied to the data before it is
-- delivered to the HTTPS endpoint destination. Kinesis Data Firehose teats
-- these options as hints, and it might choose to use more optimal values.
-- The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional.
-- However, if specify a value for one of them, you must also provide a
-- value for the other.
httpEndpointDestinationDescription_bufferingHints :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationDescription_bufferingHints = Lens.lens (\HttpEndpointDestinationDescription' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationDescription' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationDescription)

-- | Describes the S3 bucket backup options for the data that Kinesis
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
httpEndpointDestinationDescription_s3BackupMode :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe HttpEndpointS3BackupMode)
httpEndpointDestinationDescription_s3BackupMode = Lens.lens (\HttpEndpointDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@HttpEndpointDestinationDescription' {} a -> s {s3BackupMode = a} :: HttpEndpointDestinationDescription)

-- | The configuration of the specified HTTP endpoint destination.
httpEndpointDestinationDescription_endpointConfiguration :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe HttpEndpointDescription)
httpEndpointDestinationDescription_endpointConfiguration = Lens.lens (\HttpEndpointDestinationDescription' {endpointConfiguration} -> endpointConfiguration) (\s@HttpEndpointDestinationDescription' {} a -> s {endpointConfiguration = a} :: HttpEndpointDestinationDescription)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationDescription_retryOptions :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationDescription_retryOptions = Lens.lens (\HttpEndpointDestinationDescription' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationDescription' {} a -> s {retryOptions = a} :: HttpEndpointDestinationDescription)

-- | Undocumented member.
httpEndpointDestinationDescription_s3DestinationDescription :: Lens.Lens' HttpEndpointDestinationDescription (Prelude.Maybe S3DestinationDescription)
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
            Prelude.<$> (x Core..:? "RequestConfiguration")
            Prelude.<*> (x Core..:? "ProcessingConfiguration")
            Prelude.<*> (x Core..:? "RoleARN")
            Prelude.<*> (x Core..:? "BufferingHints")
            Prelude.<*> (x Core..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Core..:? "S3BackupMode")
            Prelude.<*> (x Core..:? "EndpointConfiguration")
            Prelude.<*> (x Core..:? "RetryOptions")
            Prelude.<*> (x Core..:? "S3DestinationDescription")
      )

instance
  Prelude.Hashable
    HttpEndpointDestinationDescription
  where
  hashWithSalt
    _salt
    HttpEndpointDestinationDescription' {..} =
      _salt `Prelude.hashWithSalt` requestConfiguration
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` endpointConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3DestinationDescription

instance
  Prelude.NFData
    HttpEndpointDestinationDescription
  where
  rnf HttpEndpointDestinationDescription' {..} =
    Prelude.rnf requestConfiguration
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf s3DestinationDescription
