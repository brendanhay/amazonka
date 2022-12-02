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
-- Module      : Amazonka.Firehose.Types.ElasticsearchDestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ElasticsearchDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ElasticsearchBufferingHints
import Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
import Amazonka.Firehose.Types.ElasticsearchRetryOptions
import Amazonka.Firehose.Types.ElasticsearchS3BackupMode
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationConfiguration
import Amazonka.Firehose.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a destination in Amazon ES.
--
-- /See:/ 'newElasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
  { -- | The details of the VPC of the Amazon ES destination.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The buffering options. If no value is specified, the default values for
    -- @ElasticsearchBufferingHints@ are used.
    bufferingHints :: Prelude.Maybe ElasticsearchBufferingHints,
    -- | The endpoint to use when communicating with the cluster. Specify either
    -- this @ClusterEndpoint@ or the @DomainARN@ field.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon ES domain. The IAM role must have permissions
    -- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
    -- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
    -- __RoleARN__. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    --
    -- Specify either @ClusterEndpoint@ or @DomainARN@.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
    -- one type per index. If you try to specify a new type for an existing
    -- index that already has another type, Kinesis Data Firehose returns an
    -- error during run time.
    --
    -- For Elasticsearch 7.x, don\'t specify a @TypeName@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | Defines how documents should be delivered to Amazon S3. When it is set
    -- to @FailedDocumentsOnly@, Kinesis Data Firehose writes any documents
    -- that could not be indexed to the configured Amazon S3 destination, with
    -- @elasticsearch-failed\/@ appended to the key prefix. When set to
    -- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
    -- Amazon S3, and also writes failed documents with
    -- @elasticsearch-failed\/@ appended to the prefix. For more information,
    -- see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination>.
    -- Default value is @FailedDocumentsOnly@.
    --
    -- You can\'t change this backup mode after you create the delivery stream.
    s3BackupMode :: Prelude.Maybe ElasticsearchS3BackupMode,
    -- | The Elasticsearch index rotation period. Index rotation appends a
    -- timestamp to the @IndexName@ to facilitate the expiration of old data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
    -- The default value is @OneDay@.
    indexRotationPeriod :: Prelude.Maybe ElasticsearchIndexRotationPeriod,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon ES. The default value is 300 (5 minutes).
    retryOptions :: Prelude.Maybe ElasticsearchRetryOptions,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Amazon ES Configuration API and for
    -- indexing documents. For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Text,
    -- | The Elasticsearch index name.
    indexName :: Prelude.Text,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfiguration', 'elasticsearchDestinationConfiguration_vpcConfiguration' - The details of the VPC of the Amazon ES destination.
--
-- 'processingConfiguration', 'elasticsearchDestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'bufferingHints', 'elasticsearchDestinationConfiguration_bufferingHints' - The buffering options. If no value is specified, the default values for
-- @ElasticsearchBufferingHints@ are used.
--
-- 'clusterEndpoint', 'elasticsearchDestinationConfiguration_clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- 'domainARN', 'elasticsearchDestinationConfiguration_domainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
-- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
-- __RoleARN__. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
--
-- 'typeName', 'elasticsearchDestinationConfiguration_typeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during run time.
--
-- For Elasticsearch 7.x, don\'t specify a @TypeName@.
--
-- 'cloudWatchLoggingOptions', 'elasticsearchDestinationConfiguration_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 's3BackupMode', 'elasticsearchDestinationConfiguration_s3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set
-- to @FailedDocumentsOnly@, Kinesis Data Firehose writes any documents
-- that could not be indexed to the configured Amazon S3 destination, with
-- @elasticsearch-failed\/@ appended to the key prefix. When set to
-- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- @elasticsearch-failed\/@ appended to the prefix. For more information,
-- see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination>.
-- Default value is @FailedDocumentsOnly@.
--
-- You can\'t change this backup mode after you create the delivery stream.
--
-- 'indexRotationPeriod', 'elasticsearchDestinationConfiguration_indexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to the @IndexName@ to facilitate the expiration of old data.
-- For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- The default value is @OneDay@.
--
-- 'retryOptions', 'elasticsearchDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
--
-- 'roleARN', 'elasticsearchDestinationConfiguration_roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon ES Configuration API and for
-- indexing documents. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'indexName', 'elasticsearchDestinationConfiguration_indexName' - The Elasticsearch index name.
--
-- 's3Configuration', 'elasticsearchDestinationConfiguration_s3Configuration' - The configuration for the backup Amazon S3 location.
newElasticsearchDestinationConfiguration ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  ElasticsearchDestinationConfiguration
newElasticsearchDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    ElasticsearchDestinationConfiguration'
      { vpcConfiguration =
          Prelude.Nothing,
        processingConfiguration =
          Prelude.Nothing,
        bufferingHints = Prelude.Nothing,
        clusterEndpoint = Prelude.Nothing,
        domainARN = Prelude.Nothing,
        typeName = Prelude.Nothing,
        cloudWatchLoggingOptions =
          Prelude.Nothing,
        s3BackupMode = Prelude.Nothing,
        indexRotationPeriod =
          Prelude.Nothing,
        retryOptions = Prelude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration = pS3Configuration_
      }

-- | The details of the VPC of the Amazon ES destination.
elasticsearchDestinationConfiguration_vpcConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe VpcConfiguration)
elasticsearchDestinationConfiguration_vpcConfiguration = Lens.lens (\ElasticsearchDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: ElasticsearchDestinationConfiguration)

-- | The data processing configuration.
elasticsearchDestinationConfiguration_processingConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe ProcessingConfiguration)
elasticsearchDestinationConfiguration_processingConfiguration = Lens.lens (\ElasticsearchDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {processingConfiguration = a} :: ElasticsearchDestinationConfiguration)

-- | The buffering options. If no value is specified, the default values for
-- @ElasticsearchBufferingHints@ are used.
elasticsearchDestinationConfiguration_bufferingHints :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe ElasticsearchBufferingHints)
elasticsearchDestinationConfiguration_bufferingHints = Lens.lens (\ElasticsearchDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@ElasticsearchDestinationConfiguration' {} a -> s {bufferingHints = a} :: ElasticsearchDestinationConfiguration)

-- | The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
elasticsearchDestinationConfiguration_clusterEndpoint :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe Prelude.Text)
elasticsearchDestinationConfiguration_clusterEndpoint = Lens.lens (\ElasticsearchDestinationConfiguration' {clusterEndpoint} -> clusterEndpoint) (\s@ElasticsearchDestinationConfiguration' {} a -> s {clusterEndpoint = a} :: ElasticsearchDestinationConfiguration)

-- | The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
-- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
-- __RoleARN__. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
elasticsearchDestinationConfiguration_domainARN :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe Prelude.Text)
elasticsearchDestinationConfiguration_domainARN = Lens.lens (\ElasticsearchDestinationConfiguration' {domainARN} -> domainARN) (\s@ElasticsearchDestinationConfiguration' {} a -> s {domainARN = a} :: ElasticsearchDestinationConfiguration)

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during run time.
--
-- For Elasticsearch 7.x, don\'t specify a @TypeName@.
elasticsearchDestinationConfiguration_typeName :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe Prelude.Text)
elasticsearchDestinationConfiguration_typeName = Lens.lens (\ElasticsearchDestinationConfiguration' {typeName} -> typeName) (\s@ElasticsearchDestinationConfiguration' {} a -> s {typeName = a} :: ElasticsearchDestinationConfiguration)

-- | The Amazon CloudWatch logging options for your delivery stream.
elasticsearchDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe CloudWatchLoggingOptions)
elasticsearchDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\ElasticsearchDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ElasticsearchDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationConfiguration)

-- | Defines how documents should be delivered to Amazon S3. When it is set
-- to @FailedDocumentsOnly@, Kinesis Data Firehose writes any documents
-- that could not be indexed to the configured Amazon S3 destination, with
-- @elasticsearch-failed\/@ appended to the key prefix. When set to
-- @AllDocuments@, Kinesis Data Firehose delivers all incoming records to
-- Amazon S3, and also writes failed documents with
-- @elasticsearch-failed\/@ appended to the prefix. For more information,
-- see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination>.
-- Default value is @FailedDocumentsOnly@.
--
-- You can\'t change this backup mode after you create the delivery stream.
elasticsearchDestinationConfiguration_s3BackupMode :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe ElasticsearchS3BackupMode)
elasticsearchDestinationConfiguration_s3BackupMode = Lens.lens (\ElasticsearchDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@ElasticsearchDestinationConfiguration' {} a -> s {s3BackupMode = a} :: ElasticsearchDestinationConfiguration)

-- | The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to the @IndexName@ to facilitate the expiration of old data.
-- For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- The default value is @OneDay@.
elasticsearchDestinationConfiguration_indexRotationPeriod :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe ElasticsearchIndexRotationPeriod)
elasticsearchDestinationConfiguration_indexRotationPeriod = Lens.lens (\ElasticsearchDestinationConfiguration' {indexRotationPeriod} -> indexRotationPeriod) (\s@ElasticsearchDestinationConfiguration' {} a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
elasticsearchDestinationConfiguration_retryOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Prelude.Maybe ElasticsearchRetryOptions)
elasticsearchDestinationConfiguration_retryOptions = Lens.lens (\ElasticsearchDestinationConfiguration' {retryOptions} -> retryOptions) (\s@ElasticsearchDestinationConfiguration' {} a -> s {retryOptions = a} :: ElasticsearchDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon ES Configuration API and for
-- indexing documents. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
elasticsearchDestinationConfiguration_roleARN :: Lens.Lens' ElasticsearchDestinationConfiguration Prelude.Text
elasticsearchDestinationConfiguration_roleARN = Lens.lens (\ElasticsearchDestinationConfiguration' {roleARN} -> roleARN) (\s@ElasticsearchDestinationConfiguration' {} a -> s {roleARN = a} :: ElasticsearchDestinationConfiguration)

-- | The Elasticsearch index name.
elasticsearchDestinationConfiguration_indexName :: Lens.Lens' ElasticsearchDestinationConfiguration Prelude.Text
elasticsearchDestinationConfiguration_indexName = Lens.lens (\ElasticsearchDestinationConfiguration' {indexName} -> indexName) (\s@ElasticsearchDestinationConfiguration' {} a -> s {indexName = a} :: ElasticsearchDestinationConfiguration)

-- | The configuration for the backup Amazon S3 location.
elasticsearchDestinationConfiguration_s3Configuration :: Lens.Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
elasticsearchDestinationConfiguration_s3Configuration = Lens.lens (\ElasticsearchDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {s3Configuration = a} :: ElasticsearchDestinationConfiguration)

instance
  Prelude.Hashable
    ElasticsearchDestinationConfiguration
  where
  hashWithSalt
    _salt
    ElasticsearchDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` vpcConfiguration
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` s3Configuration

instance
  Prelude.NFData
    ElasticsearchDestinationConfiguration
  where
  rnf ElasticsearchDestinationConfiguration' {..} =
    Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf clusterEndpoint
      `Prelude.seq` Prelude.rnf domainARN
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf indexRotationPeriod
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf s3Configuration

instance
  Data.ToJSON
    ElasticsearchDestinationConfiguration
  where
  toJSON ElasticsearchDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            ("ProcessingConfiguration" Data..=)
              Prelude.<$> processingConfiguration,
            ("BufferingHints" Data..=)
              Prelude.<$> bufferingHints,
            ("ClusterEndpoint" Data..=)
              Prelude.<$> clusterEndpoint,
            ("DomainARN" Data..=) Prelude.<$> domainARN,
            ("TypeName" Data..=) Prelude.<$> typeName,
            ("CloudWatchLoggingOptions" Data..=)
              Prelude.<$> cloudWatchLoggingOptions,
            ("S3BackupMode" Data..=) Prelude.<$> s3BackupMode,
            ("IndexRotationPeriod" Data..=)
              Prelude.<$> indexRotationPeriod,
            ("RetryOptions" Data..=) Prelude.<$> retryOptions,
            Prelude.Just ("RoleARN" Data..= roleARN),
            Prelude.Just ("IndexName" Data..= indexName),
            Prelude.Just
              ("S3Configuration" Data..= s3Configuration)
          ]
      )
