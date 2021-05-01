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
-- Module      : Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of the HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointDestinationConfiguration' smart constructor.
data HttpEndpointDestinationConfiguration = HttpEndpointDestinationConfiguration'
  { -- | Kinesis Data Firehose uses this IAM role for all the permissions that
    -- the delivery stream needs.
    roleARN :: Prelude.Maybe Prelude.Text,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The configuration of the requeste sent to the HTTP endpoint specified as
    -- the destination.
    requestConfiguration :: Prelude.Maybe HttpEndpointRequestConfiguration,
    -- | The buffering options that can be used before data is delivered to the
    -- specified destination. Kinesis Data Firehose treats these options as
    -- hints, and it might choose to use more optimal values. The @SizeInMBs@
    -- and @IntervalInSeconds@ parameters are optional. However, if you specify
    -- a value for one of them, you must also provide a value for the other.
    bufferingHints :: Prelude.Maybe HttpEndpointBufferingHints,
    -- | Describes the retry behavior in case Kinesis Data Firehose is unable to
    -- deliver data to the specified HTTP endpoint destination, or if it
    -- doesn\'t receive a valid acknowledgment of receipt from the specified
    -- HTTP endpoint destination.
    retryOptions :: Prelude.Maybe HttpEndpointRetryOptions,
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'httpEndpointDestinationConfiguration_roleARN' - Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
--
-- 'processingConfiguration', 'httpEndpointDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'cloudWatchLoggingOptions', 'httpEndpointDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'requestConfiguration', 'httpEndpointDestinationConfiguration_requestConfiguration' - The configuration of the requeste sent to the HTTP endpoint specified as
-- the destination.
--
-- 'bufferingHints', 'httpEndpointDestinationConfiguration_bufferingHints' - The buffering options that can be used before data is delivered to the
-- specified destination. Kinesis Data Firehose treats these options as
-- hints, and it might choose to use more optimal values. The @SizeInMBs@
-- and @IntervalInSeconds@ parameters are optional. However, if you specify
-- a value for one of them, you must also provide a value for the other.
--
-- 'retryOptions', 'httpEndpointDestinationConfiguration_retryOptions' - Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
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
      { roleARN =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        requestConfiguration =
          Prelude.Nothing,
        bufferingHints = Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        endpointConfiguration =
          pEndpointConfiguration_,
        s3Configuration = pS3Configuration_
      }

-- | Kinesis Data Firehose uses this IAM role for all the permissions that
-- the delivery stream needs.
httpEndpointDestinationConfiguration_roleARN :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe Prelude.Text)
httpEndpointDestinationConfiguration_roleARN = Lens.lens (\HttpEndpointDestinationConfiguration' {roleARN} -> roleARN) (\s@HttpEndpointDestinationConfiguration' {} a -> s {roleARN = a} :: HttpEndpointDestinationConfiguration)

-- | Undocumented member.
httpEndpointDestinationConfiguration_processingConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
httpEndpointDestinationConfiguration_processingConfiguration = Lens.lens (\HttpEndpointDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {processingConfiguration = a} :: HttpEndpointDestinationConfiguration)

-- | Undocumented member.
httpEndpointDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
httpEndpointDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\HttpEndpointDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@HttpEndpointDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: HttpEndpointDestinationConfiguration)

-- | The configuration of the requeste sent to the HTTP endpoint specified as
-- the destination.
httpEndpointDestinationConfiguration_requestConfiguration :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointRequestConfiguration)
httpEndpointDestinationConfiguration_requestConfiguration = Lens.lens (\HttpEndpointDestinationConfiguration' {requestConfiguration} -> requestConfiguration) (\s@HttpEndpointDestinationConfiguration' {} a -> s {requestConfiguration = a} :: HttpEndpointDestinationConfiguration)

-- | The buffering options that can be used before data is delivered to the
-- specified destination. Kinesis Data Firehose treats these options as
-- hints, and it might choose to use more optimal values. The @SizeInMBs@
-- and @IntervalInSeconds@ parameters are optional. However, if you specify
-- a value for one of them, you must also provide a value for the other.
httpEndpointDestinationConfiguration_bufferingHints :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointBufferingHints)
httpEndpointDestinationConfiguration_bufferingHints = Lens.lens (\HttpEndpointDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@HttpEndpointDestinationConfiguration' {} a -> s {bufferingHints = a} :: HttpEndpointDestinationConfiguration)

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
httpEndpointDestinationConfiguration_retryOptions :: Lens.Lens' HttpEndpointDestinationConfiguration (Prelude.Maybe HttpEndpointRetryOptions)
httpEndpointDestinationConfiguration_retryOptions = Lens.lens (\HttpEndpointDestinationConfiguration' {retryOptions} -> retryOptions) (\s@HttpEndpointDestinationConfiguration' {} a -> s {retryOptions = a} :: HttpEndpointDestinationConfiguration)

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

instance
  Prelude.NFData
    HttpEndpointDestinationConfiguration

instance
  Prelude.ToJSON
    HttpEndpointDestinationConfiguration
  where
  toJSON HttpEndpointDestinationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("ProcessingConfiguration" Prelude..=)
              Prelude.<$> processingConfiguration,
            ("CloudWatchLoggingOptions" Prelude..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("RequestConfiguration" Prelude..=)
              Prelude.<$> requestConfiguration,
            ("BufferingHints" Prelude..=)
              Prelude.<$> bufferingHints,
            ("RetryOptions" Prelude..=) Prelude.<$> retryOptions,
            ("S3BackupMode" Prelude..=) Prelude.<$> s3BackupMode,
            Prelude.Just
              ( "EndpointConfiguration"
                  Prelude..= endpointConfiguration
              ),
            Prelude.Just
              ("S3Configuration" Prelude..= s3Configuration)
          ]
      )
