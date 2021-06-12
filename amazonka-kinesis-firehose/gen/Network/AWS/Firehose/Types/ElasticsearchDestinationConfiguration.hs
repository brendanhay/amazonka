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
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.VpcConfiguration
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration of a destination in Amazon ES.
--
-- /See:/ 'newElasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
  { -- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
    -- one type per index. If you try to specify a new type for an existing
    -- index that already has another type, Kinesis Data Firehose returns an
    -- error during run time.
    --
    -- For Elasticsearch 7.x, don\'t specify a @TypeName@.
    typeName :: Core.Maybe Core.Text,
    -- | The endpoint to use when communicating with the cluster. Specify either
    -- this @ClusterEndpoint@ or the @DomainARN@ field.
    clusterEndpoint :: Core.Maybe Core.Text,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | The ARN of the Amazon ES domain. The IAM role must have permissions
    -- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
    -- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
    -- __RoleARN__. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    --
    -- Specify either @ClusterEndpoint@ or @DomainARN@.
    domainARN :: Core.Maybe Core.Text,
    -- | The details of the VPC of the Amazon ES destination.
    vpcConfiguration :: Core.Maybe VpcConfiguration,
    -- | The Elasticsearch index rotation period. Index rotation appends a
    -- timestamp to the @IndexName@ to facilitate the expiration of old data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
    -- The default value is @OneDay@.
    indexRotationPeriod :: Core.Maybe ElasticsearchIndexRotationPeriod,
    -- | The buffering options. If no value is specified, the default values for
    -- @ElasticsearchBufferingHints@ are used.
    bufferingHints :: Core.Maybe ElasticsearchBufferingHints,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- documents to Amazon ES. The default value is 300 (5 minutes).
    retryOptions :: Core.Maybe ElasticsearchRetryOptions,
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
    s3BackupMode :: Core.Maybe ElasticsearchS3BackupMode,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
    -- Data Firehose for calling the Amazon ES Configuration API and for
    -- indexing documents. For more information, see
    -- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Core.Text,
    -- | The Elasticsearch index name.
    indexName :: Core.Text,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: S3DestinationConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'elasticsearchDestinationConfiguration_typeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during run time.
--
-- For Elasticsearch 7.x, don\'t specify a @TypeName@.
--
-- 'clusterEndpoint', 'elasticsearchDestinationConfiguration_clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- 'processingConfiguration', 'elasticsearchDestinationConfiguration_processingConfiguration' - The data processing configuration.
--
-- 'cloudWatchLoggingOptions', 'elasticsearchDestinationConfiguration_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- 'domainARN', 'elasticsearchDestinationConfiguration_domainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
-- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
-- __RoleARN__. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
--
-- 'vpcConfiguration', 'elasticsearchDestinationConfiguration_vpcConfiguration' - The details of the VPC of the Amazon ES destination.
--
-- 'indexRotationPeriod', 'elasticsearchDestinationConfiguration_indexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to the @IndexName@ to facilitate the expiration of old data.
-- For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- The default value is @OneDay@.
--
-- 'bufferingHints', 'elasticsearchDestinationConfiguration_bufferingHints' - The buffering options. If no value is specified, the default values for
-- @ElasticsearchBufferingHints@ are used.
--
-- 'retryOptions', 'elasticsearchDestinationConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
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
  Core.Text ->
  -- | 'indexName'
  Core.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  ElasticsearchDestinationConfiguration
newElasticsearchDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    ElasticsearchDestinationConfiguration'
      { typeName =
          Core.Nothing,
        clusterEndpoint = Core.Nothing,
        processingConfiguration =
          Core.Nothing,
        cloudWatchLoggingOptions =
          Core.Nothing,
        domainARN = Core.Nothing,
        vpcConfiguration = Core.Nothing,
        indexRotationPeriod = Core.Nothing,
        bufferingHints = Core.Nothing,
        retryOptions = Core.Nothing,
        s3BackupMode = Core.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration = pS3Configuration_
      }

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only
-- one type per index. If you try to specify a new type for an existing
-- index that already has another type, Kinesis Data Firehose returns an
-- error during run time.
--
-- For Elasticsearch 7.x, don\'t specify a @TypeName@.
elasticsearchDestinationConfiguration_typeName :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Core.Text)
elasticsearchDestinationConfiguration_typeName = Lens.lens (\ElasticsearchDestinationConfiguration' {typeName} -> typeName) (\s@ElasticsearchDestinationConfiguration' {} a -> s {typeName = a} :: ElasticsearchDestinationConfiguration)

-- | The endpoint to use when communicating with the cluster. Specify either
-- this @ClusterEndpoint@ or the @DomainARN@ field.
elasticsearchDestinationConfiguration_clusterEndpoint :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Core.Text)
elasticsearchDestinationConfiguration_clusterEndpoint = Lens.lens (\ElasticsearchDestinationConfiguration' {clusterEndpoint} -> clusterEndpoint) (\s@ElasticsearchDestinationConfiguration' {} a -> s {clusterEndpoint = a} :: ElasticsearchDestinationConfiguration)

-- | The data processing configuration.
elasticsearchDestinationConfiguration_processingConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe ProcessingConfiguration)
elasticsearchDestinationConfiguration_processingConfiguration = Lens.lens (\ElasticsearchDestinationConfiguration' {processingConfiguration} -> processingConfiguration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {processingConfiguration = a} :: ElasticsearchDestinationConfiguration)

-- | The Amazon CloudWatch logging options for your delivery stream.
elasticsearchDestinationConfiguration_cloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe CloudWatchLoggingOptions)
elasticsearchDestinationConfiguration_cloudWatchLoggingOptions = Lens.lens (\ElasticsearchDestinationConfiguration' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ElasticsearchDestinationConfiguration' {} a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationConfiguration)

-- | The ARN of the Amazon ES domain. The IAM role must have permissions
-- for @DescribeElasticsearchDomain@, @DescribeElasticsearchDomains@, and
-- @DescribeElasticsearchDomainConfig@ after assuming the role specified in
-- __RoleARN__. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Specify either @ClusterEndpoint@ or @DomainARN@.
elasticsearchDestinationConfiguration_domainARN :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Core.Text)
elasticsearchDestinationConfiguration_domainARN = Lens.lens (\ElasticsearchDestinationConfiguration' {domainARN} -> domainARN) (\s@ElasticsearchDestinationConfiguration' {} a -> s {domainARN = a} :: ElasticsearchDestinationConfiguration)

-- | The details of the VPC of the Amazon ES destination.
elasticsearchDestinationConfiguration_vpcConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe VpcConfiguration)
elasticsearchDestinationConfiguration_vpcConfiguration = Lens.lens (\ElasticsearchDestinationConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {vpcConfiguration = a} :: ElasticsearchDestinationConfiguration)

-- | The Elasticsearch index rotation period. Index rotation appends a
-- timestamp to the @IndexName@ to facilitate the expiration of old data.
-- For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination>.
-- The default value is @OneDay@.
elasticsearchDestinationConfiguration_indexRotationPeriod :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe ElasticsearchIndexRotationPeriod)
elasticsearchDestinationConfiguration_indexRotationPeriod = Lens.lens (\ElasticsearchDestinationConfiguration' {indexRotationPeriod} -> indexRotationPeriod) (\s@ElasticsearchDestinationConfiguration' {} a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationConfiguration)

-- | The buffering options. If no value is specified, the default values for
-- @ElasticsearchBufferingHints@ are used.
elasticsearchDestinationConfiguration_bufferingHints :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe ElasticsearchBufferingHints)
elasticsearchDestinationConfiguration_bufferingHints = Lens.lens (\ElasticsearchDestinationConfiguration' {bufferingHints} -> bufferingHints) (\s@ElasticsearchDestinationConfiguration' {} a -> s {bufferingHints = a} :: ElasticsearchDestinationConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- documents to Amazon ES. The default value is 300 (5 minutes).
elasticsearchDestinationConfiguration_retryOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe ElasticsearchRetryOptions)
elasticsearchDestinationConfiguration_retryOptions = Lens.lens (\ElasticsearchDestinationConfiguration' {retryOptions} -> retryOptions) (\s@ElasticsearchDestinationConfiguration' {} a -> s {retryOptions = a} :: ElasticsearchDestinationConfiguration)

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
elasticsearchDestinationConfiguration_s3BackupMode :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe ElasticsearchS3BackupMode)
elasticsearchDestinationConfiguration_s3BackupMode = Lens.lens (\ElasticsearchDestinationConfiguration' {s3BackupMode} -> s3BackupMode) (\s@ElasticsearchDestinationConfiguration' {} a -> s {s3BackupMode = a} :: ElasticsearchDestinationConfiguration)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis
-- Data Firehose for calling the Amazon ES Configuration API and for
-- indexing documents. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
elasticsearchDestinationConfiguration_roleARN :: Lens.Lens' ElasticsearchDestinationConfiguration Core.Text
elasticsearchDestinationConfiguration_roleARN = Lens.lens (\ElasticsearchDestinationConfiguration' {roleARN} -> roleARN) (\s@ElasticsearchDestinationConfiguration' {} a -> s {roleARN = a} :: ElasticsearchDestinationConfiguration)

-- | The Elasticsearch index name.
elasticsearchDestinationConfiguration_indexName :: Lens.Lens' ElasticsearchDestinationConfiguration Core.Text
elasticsearchDestinationConfiguration_indexName = Lens.lens (\ElasticsearchDestinationConfiguration' {indexName} -> indexName) (\s@ElasticsearchDestinationConfiguration' {} a -> s {indexName = a} :: ElasticsearchDestinationConfiguration)

-- | The configuration for the backup Amazon S3 location.
elasticsearchDestinationConfiguration_s3Configuration :: Lens.Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
elasticsearchDestinationConfiguration_s3Configuration = Lens.lens (\ElasticsearchDestinationConfiguration' {s3Configuration} -> s3Configuration) (\s@ElasticsearchDestinationConfiguration' {} a -> s {s3Configuration = a} :: ElasticsearchDestinationConfiguration)

instance
  Core.Hashable
    ElasticsearchDestinationConfiguration

instance
  Core.NFData
    ElasticsearchDestinationConfiguration

instance
  Core.ToJSON
    ElasticsearchDestinationConfiguration
  where
  toJSON ElasticsearchDestinationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TypeName" Core..=) Core.<$> typeName,
            ("ClusterEndpoint" Core..=) Core.<$> clusterEndpoint,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("DomainARN" Core..=) Core.<$> domainARN,
            ("VpcConfiguration" Core..=)
              Core.<$> vpcConfiguration,
            ("IndexRotationPeriod" Core..=)
              Core.<$> indexRotationPeriod,
            ("BufferingHints" Core..=) Core.<$> bufferingHints,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            Core.Just ("RoleARN" Core..= roleARN),
            Core.Just ("IndexName" Core..= indexName),
            Core.Just
              ("S3Configuration" Core..= s3Configuration)
          ]
      )
