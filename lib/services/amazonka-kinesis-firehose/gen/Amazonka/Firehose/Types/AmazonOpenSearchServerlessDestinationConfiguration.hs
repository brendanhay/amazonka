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
-- Module      : Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonOpenSearchServerlessDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessBufferingHints
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions
import Amazonka.Firehose.Types.AmazonOpenSearchServerlessS3BackupMode
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationConfiguration
import Amazonka.Firehose.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in the Serverless offering
-- for Amazon OpenSearch Service.
--
-- /See:/ 'newAmazonOpenSearchServerlessDestinationConfiguration' smart constructor.
data AmazonOpenSearchServerlessDestinationConfiguration = AmazonOpenSearchServerlessDestinationConfiguration'
  { -- | The buffering options. If no value is specified, the default values for
    -- AmazonopensearchserviceBufferingHints are used.
    bufferingHints :: Prelude.Maybe AmazonOpenSearchServerlessBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the collection in the
    -- Serverless offering for Amazon OpenSearch Service.
    collectionEndpoint :: Prelude.Maybe Prelude.Text,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to the Serverless offering for Amazon OpenSearch Service. The
    -- default value is 300 (5 minutes).
    retryOptions :: Prelude.Maybe AmazonOpenSearchServerlessRetryOptions,
    -- | Defines how documents should be delivered to Amazon S3. When it is set
    -- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
    -- could not be indexed to the configured Amazon S3 destination, with
    -- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
    -- AllDocuments, Kinesis Data Firehose delivers all incoming records to
    -- Amazon S3, and also writes failed documents with
    -- AmazonOpenSearchService-failed\/ appended to the prefix.
    s3BackupMode :: Prelude.Maybe AmazonOpenSearchServerlessS3BackupMode,
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Serverless offering for Amazon OpenSearch
    -- Service Configuration API and for indexing documents.
    roleARN :: Prelude.Text,
    -- | The Serverless offering for Amazon OpenSearch Service index name.
    indexName :: Prelude.Text,
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchServerlessDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'amazonOpenSearchServerlessDestinationConfiguration_bufferingHints' - The buffering options. If no value is specified, the default values for
-- AmazonopensearchserviceBufferingHints are used.
--
-- 'cloudWatchLoggingOptions', 'amazonOpenSearchServerlessDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'collectionEndpoint', 'amazonOpenSearchServerlessDestinationConfiguration_collectionEndpoint' - The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
--
-- 'processingConfiguration', 'amazonOpenSearchServerlessDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonOpenSearchServerlessDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to the Serverless offering for Amazon OpenSearch Service. The
-- default value is 300 (5 minutes).
--
-- 's3BackupMode', 'amazonOpenSearchServerlessDestinationConfiguration_s3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set
-- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
-- could not be indexed to the configured Amazon S3 destination, with
-- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
-- AllDocuments, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- AmazonOpenSearchService-failed\/ appended to the prefix.
--
-- 'vpcConfiguration', 'amazonOpenSearchServerlessDestinationConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'roleARN', 'amazonOpenSearchServerlessDestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Serverless offering for Amazon OpenSearch
-- Service Configuration API and for indexing documents.
--
-- 'indexName', 'amazonOpenSearchServerlessDestinationConfiguration_indexName' - The Serverless offering for Amazon OpenSearch Service index name.
--
-- 's3Configuration', 'amazonOpenSearchServerlessDestinationConfiguration_s3Configuration' - Undocumented member.
newAmazonOpenSearchServerlessDestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  AmazonOpenSearchServerlessDestinationConfiguration
newAmazonOpenSearchServerlessDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    AmazonOpenSearchServerlessDestinationConfiguration'
      { bufferingHints =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        collectionEndpoint =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        retryOptions =
          Prelude.Nothing,
        s3BackupMode =
          Prelude.Nothing,
        vpcConfiguration =
          Prelude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration =
          pS3Configuration_
      }

-- | The buffering options. If no value is specified, the default values for
-- AmazonopensearchserviceBufferingHints are used.
amazonOpenSearchServerlessDestinationConfiguration_bufferingHints :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe AmazonOpenSearchServerlessBufferingHints)
amazonOpenSearchServerlessDestinationConfiguration_bufferingHints = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {bufferingHints = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
amazonOpenSearchServerlessDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | The endpoint to use when communicating with the collection in the
-- Serverless offering for Amazon OpenSearch Service.
amazonOpenSearchServerlessDestinationConfiguration_collectionEndpoint :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonOpenSearchServerlessDestinationConfiguration_collectionEndpoint = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {collectionEndpoint} -> collectionEndpoint) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {collectionEndpoint = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationConfiguration_processingConfiguration :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
amazonOpenSearchServerlessDestinationConfiguration_processingConfiguration = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {processingConfiguration = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to the Serverless offering for Amazon OpenSearch Service. The
-- default value is 300 (5 minutes).
amazonOpenSearchServerlessDestinationConfiguration_retryOptions :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe AmazonOpenSearchServerlessRetryOptions)
amazonOpenSearchServerlessDestinationConfiguration_retryOptions = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {retryOptions} -> retryOptions) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {retryOptions = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | Defines how documents should be delivered to Amazon S3. When it is set
-- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
-- could not be indexed to the configured Amazon S3 destination, with
-- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
-- AllDocuments, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- AmazonOpenSearchService-failed\/ appended to the prefix.
amazonOpenSearchServerlessDestinationConfiguration_s3BackupMode :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe AmazonOpenSearchServerlessS3BackupMode)
amazonOpenSearchServerlessDestinationConfiguration_s3BackupMode = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {s3BackupMode = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationConfiguration_vpcConfiguration :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration (Prelude.Maybe VpcConfiguration)
amazonOpenSearchServerlessDestinationConfiguration_vpcConfiguration = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Serverless offering for Amazon OpenSearch
-- Service Configuration API and for indexing documents.
amazonOpenSearchServerlessDestinationConfiguration_roleARN :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration Prelude.Text
amazonOpenSearchServerlessDestinationConfiguration_roleARN = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {roleARN} -> roleARN) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {roleARN = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | The Serverless offering for Amazon OpenSearch Service index name.
amazonOpenSearchServerlessDestinationConfiguration_indexName :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration Prelude.Text
amazonOpenSearchServerlessDestinationConfiguration_indexName = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {indexName} -> indexName) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {indexName = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

-- | Undocumented member.
amazonOpenSearchServerlessDestinationConfiguration_s3Configuration :: Lens.Lens' AmazonOpenSearchServerlessDestinationConfiguration S3DestinationConfiguration
amazonOpenSearchServerlessDestinationConfiguration_s3Configuration = Lens.lens (\AmazonOpenSearchServerlessDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@AmazonOpenSearchServerlessDestinationConfiguration' {} a -> s {s3Configuration = a} :: AmazonOpenSearchServerlessDestinationConfiguration)

instance
  Prelude.Hashable
    AmazonOpenSearchServerlessDestinationConfiguration
  where
  hashWithSalt
    _salt
    AmazonOpenSearchServerlessDestinationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` collectionEndpoint
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` vpcConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    AmazonOpenSearchServerlessDestinationConfiguration
  where
  rnf
    AmazonOpenSearchServerlessDestinationConfiguration' {..} =
      Prelude.rnf bufferingHints `Prelude.seq`
        Prelude.rnf cloudWatchLoggingOptions `Prelude.seq`
          Prelude.rnf collectionEndpoint `Prelude.seq`
            Prelude.rnf processingConfiguration `Prelude.seq`
              Prelude.rnf retryOptions `Prelude.seq`
                Prelude.rnf s3BackupMode `Prelude.seq`
                  Prelude.rnf vpcConfiguration `Prelude.seq`
                    Prelude.rnf roleARN `Prelude.seq`
                      Prelude.rnf indexName `Prelude.seq`
                        Prelude.rnf s3Configuration

instance
  Data.ToJSON
    AmazonOpenSearchServerlessDestinationConfiguration
  where
  toJSON
    AmazonOpenSearchServerlessDestinationConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BufferingHints" Data..=)
                Prelude.<$> bufferingHints,
              ("CloudWatchLoggingOptions" Data..=)
                Prelude.<$> cloudWatchLoggingOptions,
              ("CollectionEndpoint" Data..=)
                Prelude.<$> collectionEndpoint,
              ("ProcessingConfiguration" Data..=)
                Prelude.<$> processingConfiguration,
              ("RetryOptions" Data..=) Prelude.<$> retryOptions,
              ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
              ("VpcConfiguration" Data..=)
                Prelude.<$> vpcConfiguration,
              Prelude.Just ("RoleARN" Data..= roleARN),
              Prelude.Just ("IndexName" Data..= indexName),
              Prelude.Just
                ("S3Configuration" Data..= s3Configuration)
            ]
        )
