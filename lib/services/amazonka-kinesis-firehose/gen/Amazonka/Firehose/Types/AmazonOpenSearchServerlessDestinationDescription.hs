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
-- Module      : Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessBufferingHints
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessS3BackupMode
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationDescription
import Amazonka.Firehose.Types.VpcConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | The destination description in the Serverless offering for Amazon
-- OpenSearch Service.
--
-- /See:/ 'newAmazonOpenSearchServerlessDestinationDescription' smart constructor.
data AmazonOpenSearchServerlessDestinationDescription = AmazonOpenSearchServerlessDestinationDescription'
  { -- | The buffering options.
    bufferingHints :: Prelude.Maybe AmazonOpenSearchServerlessBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the collection in the
    -- Serverless offering for Amazon OpenSearch Service.
    collectionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Serverless offering for Amazon OpenSearch Service index name.
    indexName :: Prelude.Maybe Prelude.Text,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The Serverless offering for Amazon OpenSearch Service retry options.
    retryOptions :: Prelude.Maybe AmazonOpenSearchServerlessRetryOptions,
    -- | The Amazon Resource Name (ARN) of the AWS credentials.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Prelude.Maybe AmazonOpenSearchServerlessS3BackupMode,
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription,
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchServerlessDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'amazonOpenSearchServerlessDestinationDescription_bufferingHints' - The buffering options.
--
-- 'cloudWatchLoggingOptions', 'amazonOpenSearchServerlessDestinationDescription_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'collectionEndpoint', 'amazonOpenSearchServerlessDestinationDescription_collectionEndpoint' - The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
--
-- 'indexName', 'amazonOpenSearchServerlessDestinationDescription_indexName' - The Serverless offering for Amazon OpenSearch Service index name.
--
-- 'processingConfiguration', 'amazonOpenSearchServerlessDestinationDescription_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonOpenSearchServerlessDestinationDescription_retryOptions' - The Serverless offering for Amazon OpenSearch Service retry options.
--
-- 'roleARN', 'amazonOpenSearchServerlessDestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials.
--
-- 's3BackupMode', 'amazonOpenSearchServerlessDestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 's3DestinationDescription', 'amazonOpenSearchServerlessDestinationDescription_s3DestinationDescription' - Undocumented member.
--
-- 'vpcConfigurationDescription', 'amazonOpenSearchServerlessDestinationDescription_vpcConfigurationDescription' - Undocumented member.
newAmazonOpenSearchServerlessDestinationDescription ::
  AmazonOpenSearchServerlessDestinationDescription
newAmazonOpenSearchServerlessDestinationDescription =
  AmazonOpenSearchServerlessDestinationDescription'
    { bufferingHints =
        Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      collectionEndpoint =
        Prelude.Nothing,
      indexName =
        Prelude.Nothing,
      processingConfiguration =
        Prelude.Nothing,
      retryOptions =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      s3BackupMode =
        Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing,
      vpcConfigurationDescription =
        Prelude.Nothing
    }

-- | The buffering options.
amazonOpenSearchServerlessDestinationDescription_bufferingHints :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe AmazonOpenSearchServerlessBufferingHints)
amazonOpenSearchServerlessDestinationDescription_bufferingHints = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {bufferingHints} -> bufferingHints) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {bufferingHints = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
amazonOpenSearchServerlessDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
amazonOpenSearchServerlessDestinationDescription_collectionEndpoint :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationDescription_collectionEndpoint = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {collectionEndpoint} -> collectionEndpoint) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {collectionEndpoint = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | The Serverless offering for Amazon OpenSearch Service index name.
amazonOpenSearchServerlessDestinationDescription_indexName :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationDescription_indexName = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {indexName} -> indexName) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {indexName = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationDescription_processingConfiguration :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe ProcessingConfiguration)
amazonOpenSearchServerlessDestinationDescription_processingConfiguration = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {processingConfiguration = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | The Serverless offering for Amazon OpenSearch Service retry options.
amazonOpenSearchServerlessDestinationDescription_retryOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe AmazonOpenSearchServerlessRetryOptions)
amazonOpenSearchServerlessDestinationDescription_retryOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {retryOptions} -> retryOptions) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {retryOptions = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | The Amazon Resource Name (ARN) of the AWS credentials.
amazonOpenSearchServerlessDestinationDescription_roleARN :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationDescription_roleARN = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {roleARN} -> roleARN) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {roleARN = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | The Amazon S3 backup mode.
amazonOpenSearchServerlessDestinationDescription_s3BackupMode :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe AmazonOpenSearchServerlessS3BackupMode)
amazonOpenSearchServerlessDestinationDescription_s3BackupMode = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {s3BackupMode = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationDescription_s3DestinationDescription :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe S3DestinationDescription)
amazonOpenSearchServerlessDestinationDescription_s3DestinationDescription = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {s3DestinationDescription = a} :: AmazonOpenSearchServerlessDestinationDescription)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationDescription_vpcConfigurationDescription :: Lens.Lens' AmazonOpenSearchServerlessDestinationDescription (Prelude.Maybe VpcConfigurationDescription)
amazonOpenSearchServerlessDestinationDescription_vpcConfigurationDescription = Lens.lens (\AmazonOpenSearchServerlessDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@AmazonOpenSearchServerlessDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: AmazonOpenSearchServerlessDestinationDescription)

instance
  Data.FromJSON
    AmazonOpenSearchServerlessDestinationDescription
  where
  parseJSON =
    Data.withObject
      "AmazonOpenSearchServerlessDestinationDescription"
      ( \x ->
          AmazonOpenSearchServerlessDestinationDescription'
            Prelude.<$> (x Data..:? "BufferingHints")
              Prelude.<*> (x Data..:? "CloudWatchLoggingOptions")
              Prelude.<*> (x Data..:? "CollectionEndpoint")
              Prelude.<*> (x Data..:? "IndexName")
              Prelude.<*> (x Data..:? "ProcessingConfiguration")
              Prelude.<*> (x Data..:? "RetryOptions")
              Prelude.<*> (x Data..:? "RoleARN")
              Prelude.<*> (x Data..:? "S3BackupMode")
              Prelude.<*> (x Data..:? "S3DestinationDescription")
              Prelude.<*> (x Data..:? "VpcConfigurationDescription")
      )

instance
  Prelude.Hashable
    AmazonOpenSearchServerlessDestinationDescription
  where
  hashWithSalt
    _salt
    AmazonOpenSearchServerlessDestinationDescription' {..} =
      _salt `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` collectionEndpoint
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` s3DestinationDescription
        `Prelude.hashWithSalt` vpcConfigurationDescription

instance
  Prelude.NFData
    AmazonOpenSearchServerlessDestinationDescription
  where
  rnf
    AmazonOpenSearchServerlessDestinationDescription' {..} =
      Prelude.rnf bufferingHints
        `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
        `Prelude.seq` Prelude.rnf collectionEndpoint
        `Prelude.seq` Prelude.rnf indexName
        `Prelude.seq` Prelude.rnf processingConfiguration
        `Prelude.seq` Prelude.rnf retryOptions
        `Prelude.seq` Prelude.rnf roleARN
        `Prelude.seq` Prelude.rnf s3BackupMode
        `Prelude.seq` Prelude.rnf s3DestinationDescription
        `Prelude.seq` Prelude.rnf vpcConfigurationDescription
