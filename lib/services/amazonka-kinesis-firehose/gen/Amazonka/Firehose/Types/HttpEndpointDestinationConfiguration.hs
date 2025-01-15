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
-- Module      : Amazonka.Firehose.Types.HttpEndpointDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.HttpEndpointBufferingHints
import Amazonka.Firehose.Types.HttpEndpointConfiguration
import Amazonka.Firehose.Types.HttpEndpointRequestConfiguration
import Amazonka.Firehose.Types.HttpEndpointRetryOptions
import Amazonka.Firehose.Types.HttpEndpointS3BackupMode
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationConfiguration' smart constructor.
data HttpEndpointDestinationConfiguration = HttpEndpointDestinationConfiguration'
  { -- | The buffering options that can be used before data is delivered to the
    -- specified destination. Kinesis Data Firehose treats these options as
    -- hints, and it might choose to use more optimal values. The @SizeInMBs@
    -- and @IntervalInSeconds@ parameters are optional. However, if you specify
    -- a value for one of them, you must also provide a value for the other.
    bufferingHints :: Prelude.Maybe HttpEndpointBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The configuration of the requeste sent to the HTTP endpoint specified as
    -- the destination.
    requestConfiguration :: Prelude.Maybe HttpEndpointRequestConfiguration,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to
    -- deliver data to the specified HTTP endpoint destination, or if it
    -- doesn\'t receive a valid acknowledgment of receipt from the specified
    -- HTTP endpoint destination.
    retryOptions :: Prelude.Maybe HttpEndpointRetryOptions,
    -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Describes the S3 bucket backup options for the data that Kinesis Data
    -- Firehose delivers to the HTTP endpoint destination. You can back up all
    -- documents (@AllData@) or only the documents that Kinesis Data Firehose
    -- could not deliver to the specified HTTP endpoint destination
    -- (@FailedDataOnly@).
    s3BackupMode :: Prelude.Maybe HttpEndpointS3BackupMode,
    -- | The configuration of the HTTP endpoint selected as the destination.
    endpointConfiguration :: HttpEndpointConfiguration,
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'httpEndpointDestinationConfiguration_bufferingHints' - The buffering options that can be used before data is delivered to the
-- specified destination. Kinesis Data Firehose treats these options as
-- hints, and it might choose to use more optimal values. The @SizeInMBs@
-- and @IntervalInSeconds@ parameters are optional. However, if you specify
-- a value for one of them, you must also provide a value for the other.
--
-- 'cloudWatchLoggingOptions', 'httpEndpointDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'processingConfiguration', 'httpEndpointDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'requestConfiguration', 'httpEndpointDestinationConfiguration_requestConfiguration' - The configuration of the requeste sent to the HTTP endpoint specified as
-- the destination.
--
-- 'retryOptions', 'httpEndpointDestinationConfiguration_retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
--
-- 'roleARN', 'httpEndpointDestinationConfiguration_roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
--
-- 's3BackupMode', 'httpEndpointDestinationConfiguration_s3BackupMode' - Describes the S3 bucket backup options for the data that Kinesis Data
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
--
-- 'endpointConfiguration', 'httpEndpointDestinationConfiguration_endpointConfiguration' - The configuration of the HTTP endpoint selected as the destination.
--
-- 's3Configuration', 'httpEndpointDestinationConfiguration_s3Configuration' - Undocumented member.
newHttpEndpointDestinationConfiguration ::
  -- | 'endpointConfiguration'
  HttpEndpointConfiguration ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  HttpEndpointDestinationConfiguration
newHttpEndpointDestinationConfiguration
  pEndpointConfiguration_
  pS3Configuration_ =
    HttpEndpointDestinationConfiguration'
      { bufferingHints =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        requestConfiguration =
          Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        roleARN = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        endpointConfiguration =
          pEndpointConfiguration_,
        s3Configuration = pS3Configuration_
      }

-- | The buffering options that can be used before data is delivered to the
-- specified destination. Kinesis Data Firehose treats these options as
-- hints, and it might choose to use more optimal values. The @SizeInMBs@
-- and @IntervalInSeconds@ parameters are optional. However, if you specify
-- a value for one of them, you must also provide a value for the other.
httpEndpointDestinationConfiguration_bufferingHints :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationConfiguration_bufferingHints = Lens.lens (\HttpEndpointDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationConfiguration' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationConfiguration)

-- | Undocumented member.
httpEndpointDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationConfiguration)

-- | Undocumented member.
httpEndpointDestinationConfiguration_processingConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
httpEndpointDestinationConfiguration_processingConfiguration = Lens.lens (\HttpEndpointDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationConfiguration)

-- | The configuration of the requeste sent to the HTTP endpoint specified as
-- the destination.
httpEndpointDestinationConfiguration_requestConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationConfiguration_requestConfiguration = Lens.lens (\HttpEndpointDestinationConfiguration' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationConfiguration)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationConfiguration_retryOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationConfiguration_retryOptions = Lens.lens (\HttpEndpointDestinationConfiguration' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationConfiguration' {} a -> s {retryOptions = a} :: HttpEndpointDestinationConfiguration)

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationConfiguration_roleARN :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointDestinationConfiguration_roleARN = Lens.lens (\HttpEndpointDestinationConfiguration' {roleARN} -> roleARN) (\s@HttpEndpointDestinationConfiguration' {} a -> s {roleARN = a} :: HttpEndpointDestinationConfiguration)

-- | Describes the S3 bucket backup options for the data that Kinesis Data
-- Firehose delivers to the HTTP endpoint destination. You can back up all
-- documents (@AllData@) or only the documents that Kinesis Data Firehose
-- could not deliver to the specified HTTP endpoint destination
-- (@FailedDataOnly@).
httpEndpointDestinationConfiguration_s3BackupMode :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointS3BackupMode)
httpEndpointDestinationConfiguration_s3BackupMode = Lens.lens (\HttpEndpointDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@HttpEndpointDestinationConfiguration' {} a -> s {s3BackupMode = a} :: HttpEndpointDestinationConfiguration)

-- | The configuration of the HTTP endpoint selected as the destination.
httpEndpointDestinationConfiguration_endpointConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration HttpEndpointConfiguration
httpEndpointDestinationConfiguration_endpointConfiguration = Lens.lens (\HttpEndpointDestinationConfiguration' {endpointConfiguration} -> endpointConfiguration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {endpointConfiguration = a} :: HttpEndpointDestinationConfiguration)

-- | Undocumented member.
httpEndpointDestinationConfiguration_s3Configuration :: Lens.Lens' HttpEndpointDestinationConfiguration S3DestinationConfiguration
httpEndpointDestinationConfiguration_s3Configuration = Lens.lens (\HttpEndpointDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {s3Configuration = a} :: HttpEndpointDestinationConfiguration)

instance
  Prelude.Hashable
    HttpEndpointDestinationConfiguration
  where
  hashWithSalt
    _salt
    HttpEndpointDestinationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` requestConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` endpointConfiguration
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    HttpEndpointDestinationConfiguration
  where
  rnf HttpEndpointDestinationConfiguration' {..} =
    Prelude.rnf bufferingHints `Prelude.seq`
      Prelude.rnf cloudWatchLoggingOptions `Prelude.seq`
        Prelude.rnf processingConfiguration `Prelude.seq`
          Prelude.rnf requestConfiguration `Prelude.seq`
            Prelude.rnf retryOptions `Prelude.seq`
              Prelude.rnf roleARN `Prelude.seq`
                Prelude.rnf s3BackupMode `Prelude.seq`
                  Prelude.rnf endpointConfiguration `Prelude.seq`
                    Prelude.rnf s3Configuration

instance
  Data.ToJSON
    HttpEndpointDestinationConfiguration
  where
  toJSON HttpEndpointDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BufferingHints" Data..=)
              Prelude.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("RequestConfiguration" Data..=)
              Prelude.<$> requestConfiguration,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            Prelude.Just
              ( "EndpointConfiguration"
                  Data..= endpointConfiguration
              ),
            Prelude.Just
              ("S3Configuration" Data..= s3Configuration)
          ]
      )
