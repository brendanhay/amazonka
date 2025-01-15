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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.AmazonopensearchserviceBufferingHints
import Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
import Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
import Amazonka.Firehose.Types.AmazonopensearchserviceS3BackupMode
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationConfiguration
import Amazonka.Firehose.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon OpenSearch
-- Service
--
-- /See:/ 'newAmazonopensearchserviceDestinationConfiguration' smart constructor.
data AmazonopensearchserviceDestinationConfiguration = AmazonopensearchserviceDestinationConfiguration'
  { -- | The buffering options. If no value is specified, the default values for
    -- AmazonopensearchserviceBufferingHints are used.
    bufferingHints :: Prelude.Maybe AmazonopensearchserviceBufferingHints,
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Specify either
    -- this ClusterEndpoint or the DomainARN field.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon OpenSearch Service domain. The IAM role must have
    -- permissions for DescribeElasticsearchDomain,
    -- DescribeElasticsearchDomains, and DescribeElasticsearchDomainConfig
    -- after assuming the role specified in RoleARN.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon OpenSearch Service index rotation period. Index rotation
    -- appends a timestamp to the IndexName to facilitate the expiration of old
    -- data.
    indexRotationPeriod :: Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod,
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon OpenSearch Service. The default value is 300 (5
    -- minutes).
    retryOptions :: Prelude.Maybe AmazonopensearchserviceRetryOptions,
    -- | Defines how documents should be delivered to Amazon S3. When it is set
    -- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
    -- could not be indexed to the configured Amazon S3 destination, with
    -- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
    -- AllDocuments, Kinesis Data Firehose delivers all incoming records to
    -- Amazon S3, and also writes failed documents with
    -- AmazonOpenSearchService-failed\/ appended to the prefix.
    s3BackupMode :: Prelude.Maybe AmazonopensearchserviceS3BackupMode,
    -- | The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
    -- can be only one type per index. If you try to specify a new type for an
    -- existing index that already has another type, Kinesis Data Firehose
    -- returns an error during run time.
    typeName :: Prelude.Maybe Prelude.Text,
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Amazon OpenSearch Service Configuration
    -- API and for indexing documents.
    roleARN :: Prelude.Text,
    -- | The ElasticsearAmazon OpenSearch Service index name.
    indexName :: Prelude.Text,
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bufferingHints', 'amazonopensearchserviceDestinationConfiguration_bufferingHints' - The buffering options. If no value is specified, the default values for
-- AmazonopensearchserviceBufferingHints are used.
--
-- 'cloudWatchLoggingOptions', 'amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions' - Undocumented member.
--
-- 'clusterEndpoint', 'amazonopensearchserviceDestinationConfiguration_clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either
-- this ClusterEndpoint or the DomainARN field.
--
-- 'domainARN', 'amazonopensearchserviceDestinationConfiguration_domainARN' - The ARN of the Amazon OpenSearch Service domain. The IAM role must have
-- permissions for DescribeElasticsearchDomain,
-- DescribeElasticsearchDomains, and DescribeElasticsearchDomainConfig
-- after assuming the role specified in RoleARN.
--
-- 'indexRotationPeriod', 'amazonopensearchserviceDestinationConfiguration_indexRotationPeriod' - The Amazon OpenSearch Service index rotation period. Index rotation
-- appends a timestamp to the IndexName to facilitate the expiration of old
-- data.
--
-- 'processingConfiguration', 'amazonopensearchserviceDestinationConfiguration_processingConfiguration' - Undocumented member.
--
-- 'retryOptions', 'amazonopensearchserviceDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon OpenSearch Service. The default value is 300 (5
-- minutes).
--
-- 's3BackupMode', 'amazonopensearchserviceDestinationConfiguration_s3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set
-- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
-- could not be indexed to the configured Amazon S3 destination, with
-- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
-- AllDocuments, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- AmazonOpenSearchService-failed\/ appended to the prefix.
--
-- 'typeName', 'amazonopensearchserviceDestinationConfiguration_typeName' - The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
-- can be only one type per index. If you try to specify a new type for an
-- existing index that already has another type, Kinesis Data Firehose
-- returns an error during run time.
--
-- 'vpcConfiguration', 'amazonopensearchserviceDestinationConfiguration_vpcConfiguration' - Undocumented member.
--
-- 'roleARN', 'amazonopensearchserviceDestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon OpenSearch Service Configuration
-- API and for indexing documents.
--
-- 'indexName', 'amazonopensearchserviceDestinationConfiguration_indexName' - The ElasticsearAmazon OpenSearch Service index name.
--
-- 's3Configuration', 'amazonopensearchserviceDestinationConfiguration_s3Configuration' - Undocumented member.
newAmazonopensearchserviceDestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  AmazonopensearchserviceDestinationConfiguration
newAmazonopensearchserviceDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    AmazonopensearchserviceDestinationConfiguration'
      { bufferingHints =
          Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        clusterEndpoint =
          Prelude.Nothing,
        domainARN =
          Prelude.Nothing,
        indexRotationPeriod =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        retryOptions =
          Prelude.Nothing,
        s3BackupMode =
          Prelude.Nothing,
        typeName = Prelude.Nothing,
        vpcConfiguration =
          Prelude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration =
          pS3Configuration_
      }

-- | The buffering options. If no value is specified, the default values for
-- AmazonopensearchserviceBufferingHints are used.
amazonopensearchserviceDestinationConfiguration_bufferingHints :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceBufferingHints)
amazonopensearchserviceDestinationConfiguration_bufferingHints = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {bufferingHints = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
amazonopensearchserviceDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The endpoint to use when communicating with the cluster. Specify either
-- this ClusterEndpoint or the DomainARN field.
amazonopensearchserviceDestinationConfiguration_clusterEndpoint :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_clusterEndpoint = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {clusterEndpoint} -> clusterEndpoint) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {clusterEndpoint = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The ARN of the Amazon OpenSearch Service domain. The IAM role must have
-- permissions for DescribeElasticsearchDomain,
-- DescribeElasticsearchDomains, and DescribeElasticsearchDomainConfig
-- after assuming the role specified in RoleARN.
amazonopensearchserviceDestinationConfiguration_domainARN :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_domainARN = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {domainARN} -> domainARN) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {domainARN = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The Amazon OpenSearch Service index rotation period. Index rotation
-- appends a timestamp to the IndexName to facilitate the expiration of old
-- data.
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceIndexRotationPeriod)
amazonopensearchserviceDestinationConfiguration_indexRotationPeriod = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {indexRotationPeriod} -> indexRotationPeriod) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {indexRotationPeriod = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_processingConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
amazonopensearchserviceDestinationConfiguration_processingConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {processingConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon OpenSearch Service. The default value is 300 (5
-- minutes).
amazonopensearchserviceDestinationConfiguration_retryOptions :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceRetryOptions)
amazonopensearchserviceDestinationConfiguration_retryOptions = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {retryOptions} -> retryOptions) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {retryOptions = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Defines how documents should be delivered to Amazon S3. When it is set
-- to FailedDocumentsOnly, Kinesis Data Firehose writes any documents that
-- could not be indexed to the configured Amazon S3 destination, with
-- AmazonOpenSearchService-failed\/ appended to the key prefix. When set to
-- AllDocuments, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- AmazonOpenSearchService-failed\/ appended to the prefix.
amazonopensearchserviceDestinationConfiguration_s3BackupMode :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe AmazonopensearchserviceS3BackupMode)
amazonopensearchserviceDestinationConfiguration_s3BackupMode = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {s3BackupMode = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The Amazon OpenSearch Service type name. For Elasticsearch 6.x, there
-- can be only one type per index. If you try to specify a new type for an
-- existing index that already has another type, Kinesis Data Firehose
-- returns an error during run time.
amazonopensearchserviceDestinationConfiguration_typeName :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe Prelude.Text)
amazonopensearchserviceDestinationConfiguration_typeName = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {typeName} -> typeName) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {typeName = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_vpcConfiguration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration (Prelude.Maybe VpcConfiguration)
amazonopensearchserviceDestinationConfiguration_vpcConfiguration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon OpenSearch Service Configuration
-- API and for indexing documents.
amazonopensearchserviceDestinationConfiguration_roleARN :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration Prelude.Text
amazonopensearchserviceDestinationConfiguration_roleARN = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {roleARN} -> roleARN) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {roleARN = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | The ElasticsearAmazon OpenSearch Service index name.
amazonopensearchserviceDestinationConfiguration_indexName :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration Prelude.Text
amazonopensearchserviceDestinationConfiguration_indexName = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {indexName} -> indexName) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {indexName = a} :: AmazonopensearchserviceDestinationConfiguration)

-- | Undocumented member.
amazonopensearchserviceDestinationConfiguration_s3Configuration :: Lens.Lens' AmazonopensearchserviceDestinationConfiguration S3DestinationConfiguration
amazonopensearchserviceDestinationConfiguration_s3Configuration = Lens.lens (\AmazonopensearchserviceDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@AmazonopensearchserviceDestinationConfiguration' {} a -> s {s3Configuration = a} :: AmazonopensearchserviceDestinationConfiguration)

instance
  Prelude.Hashable
    AmazonopensearchserviceDestinationConfiguration
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceDestinationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` vpcConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    AmazonopensearchserviceDestinationConfiguration
  where
  rnf
    AmazonopensearchserviceDestinationConfiguration' {..} =
      Prelude.rnf bufferingHints `Prelude.seq`
        Prelude.rnf cloudWatchLoggingOptions `Prelude.seq`
          Prelude.rnf clusterEndpoint `Prelude.seq`
            Prelude.rnf domainARN `Prelude.seq`
              Prelude.rnf indexRotationPeriod `Prelude.seq`
                Prelude.rnf processingConfiguration `Prelude.seq`
                  Prelude.rnf retryOptions `Prelude.seq`
                    Prelude.rnf s3BackupMode `Prelude.seq`
                      Prelude.rnf typeName `Prelude.seq`
                        Prelude.rnf vpcConfiguration `Prelude.seq`
                          Prelude.rnf roleARN `Prelude.seq`
                            Prelude.rnf indexName `Prelude.seq`
                              Prelude.rnf s3Configuration

instance
  Data.ToJSON
    AmazonopensearchserviceDestinationConfiguration
  where
  toJSON
    AmazonopensearchserviceDestinationConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BufferingHints" Data..=)
                Prelude.<$> bufferingHints,
              ("CloudWatchLoggingOptions" Data..=)
                Prelude.<$> cloudWatchLoggingOptions,
              ("ClusterEndpoint" Data..=)
                Prelude.<$> clusterEndpoint,
              ("DomainARN" Data..=) Prelude.<$> domainARN,
              ("IndexRotationPeriod" Data..=)
                Prelude.<$> indexRotationPeriod,
              ("ProcessingConfiguration" Data..=)
                Prelude.<$> processingConfiguration,
              ("RetryOptions" Data..=) Prelude.<$> retryOptions,
              ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
              ("TypeName" Data..=) Prelude.<$> typeName,
              ("VpcConfiguration" Data..=)
                Prelude.<$> vpcConfiguration,
              Prelude.Just ("RoleARN" Data..= roleARN),
              Prelude.Just ("IndexName" Data..= indexName),
              Prelude.Just
                ("S3Configuration" Data..= s3Configuration)
            ]
        )
