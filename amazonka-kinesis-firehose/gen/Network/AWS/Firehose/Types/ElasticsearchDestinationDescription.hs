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
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.VpcConfigurationDescription
import qualified Network.AWS.Lens as Lens

-- | The destination description in Amazon ES.
--
-- /See:/ 'newElasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
  { -- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
    -- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
    typeName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Core.Maybe Core.Text,
    -- | The endpoint to use when communicating with the cluster. Kinesis Data
    -- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
    -- send data to Amazon ES.
    clusterEndpoint :: Core.Maybe Core.Text,
    -- | The Elasticsearch index name.
    indexName :: Core.Maybe Core.Text,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe ProcessingConfiguration,
    -- | The Amazon CloudWatch logging options.
    cloudWatchLoggingOptions :: Core.Maybe CloudWatchLoggingOptions,
    -- | The ARN of the Amazon ES domain. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    --
    -- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
    -- send data to Amazon ES.
    domainARN :: Core.Maybe Core.Text,
    -- | The Elasticsearch index rotation period
    indexRotationPeriod :: Core.Maybe ElasticsearchIndexRotationPeriod,
    -- | The details of the VPC of the Amazon ES destination.
    vpcConfigurationDescription :: Core.Maybe VpcConfigurationDescription,
    -- | The buffering options.
    bufferingHints :: Core.Maybe ElasticsearchBufferingHints,
    -- | The Amazon ES retry options.
    retryOptions :: Core.Maybe ElasticsearchRetryOptions,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Core.Maybe ElasticsearchS3BackupMode,
    -- | The Amazon S3 destination.
    s3DestinationDescription :: Core.Maybe S3DestinationDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'elasticsearchDestinationDescription_typeName' - The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
-- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
--
-- 'roleARN', 'elasticsearchDestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'clusterEndpoint', 'elasticsearchDestinationDescription_clusterEndpoint' - The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
-- send data to Amazon ES.
--
-- 'indexName', 'elasticsearchDestinationDescription_indexName' - The Elasticsearch index name.
--
-- 'processingConfiguration', 'elasticsearchDestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 'cloudWatchLoggingOptions', 'elasticsearchDestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options.
--
-- 'domainARN', 'elasticsearchDestinationDescription_domainARN' - The ARN of the Amazon ES domain. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
-- send data to Amazon ES.
--
-- 'indexRotationPeriod', 'elasticsearchDestinationDescription_indexRotationPeriod' - The Elasticsearch index rotation period
--
-- 'vpcConfigurationDescription', 'elasticsearchDestinationDescription_vpcConfigurationDescription' - The details of the VPC of the Amazon ES destination.
--
-- 'bufferingHints', 'elasticsearchDestinationDescription_bufferingHints' - The buffering options.
--
-- 'retryOptions', 'elasticsearchDestinationDescription_retryOptions' - The Amazon ES retry options.
--
-- 's3BackupMode', 'elasticsearchDestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 's3DestinationDescription', 'elasticsearchDestinationDescription_s3DestinationDescription' - The Amazon S3 destination.
newElasticsearchDestinationDescription ::
  ElasticsearchDestinationDescription
newElasticsearchDestinationDescription =
  ElasticsearchDestinationDescription'
    { typeName =
        Core.Nothing,
      roleARN = Core.Nothing,
      clusterEndpoint = Core.Nothing,
      indexName = Core.Nothing,
      processingConfiguration = Core.Nothing,
      cloudWatchLoggingOptions =
        Core.Nothing,
      domainARN = Core.Nothing,
      indexRotationPeriod = Core.Nothing,
      vpcConfigurationDescription =
        Core.Nothing,
      bufferingHints = Core.Nothing,
      retryOptions = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3DestinationDescription =
        Core.Nothing
    }

-- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
-- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
elasticsearchDestinationDescription_typeName :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Core.Text)
elasticsearchDestinationDescription_typeName = Lens.lens (\ElasticsearchDestinationDescription' {typeName} -> typeName) (\s@ElasticsearchDestinationDescription' {} a -> s {typeName = a} :: ElasticsearchDestinationDescription)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
elasticsearchDestinationDescription_roleARN :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Core.Text)
elasticsearchDestinationDescription_roleARN = Lens.lens (\ElasticsearchDestinationDescription' {roleARN} -> roleARN) (\s@ElasticsearchDestinationDescription' {} a -> s {roleARN = a} :: ElasticsearchDestinationDescription)

-- | The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
-- send data to Amazon ES.
elasticsearchDestinationDescription_clusterEndpoint :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Core.Text)
elasticsearchDestinationDescription_clusterEndpoint = Lens.lens (\ElasticsearchDestinationDescription' {clusterEndpoint} -> clusterEndpoint) (\s@ElasticsearchDestinationDescription' {} a -> s {clusterEndpoint = a} :: ElasticsearchDestinationDescription)

-- | The Elasticsearch index name.
elasticsearchDestinationDescription_indexName :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Core.Text)
elasticsearchDestinationDescription_indexName = Lens.lens (\ElasticsearchDestinationDescription' {indexName} -> indexName) (\s@ElasticsearchDestinationDescription' {} a -> s {indexName = a} :: ElasticsearchDestinationDescription)

-- | The data processing configuration.
elasticsearchDestinationDescription_processingConfiguration :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe ProcessingConfiguration)
elasticsearchDestinationDescription_processingConfiguration = Lens.lens (\ElasticsearchDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@ElasticsearchDestinationDescription' {} a -> s {processingConfiguration = a} :: ElasticsearchDestinationDescription)

-- | The Amazon CloudWatch logging options.
elasticsearchDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe CloudWatchLoggingOptions)
elasticsearchDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\ElasticsearchDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ElasticsearchDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationDescription)

-- | The ARN of the Amazon ES domain. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
-- send data to Amazon ES.
elasticsearchDestinationDescription_domainARN :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Core.Text)
elasticsearchDestinationDescription_domainARN = Lens.lens (\ElasticsearchDestinationDescription' {domainARN} -> domainARN) (\s@ElasticsearchDestinationDescription' {} a -> s {domainARN = a} :: ElasticsearchDestinationDescription)

-- | The Elasticsearch index rotation period
elasticsearchDestinationDescription_indexRotationPeriod :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe ElasticsearchIndexRotationPeriod)
elasticsearchDestinationDescription_indexRotationPeriod = Lens.lens (\ElasticsearchDestinationDescription' {indexRotationPeriod} -> indexRotationPeriod) (\s@ElasticsearchDestinationDescription' {} a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationDescription)

-- | The details of the VPC of the Amazon ES destination.
elasticsearchDestinationDescription_vpcConfigurationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe VpcConfigurationDescription)
elasticsearchDestinationDescription_vpcConfigurationDescription = Lens.lens (\ElasticsearchDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@ElasticsearchDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: ElasticsearchDestinationDescription)

-- | The buffering options.
elasticsearchDestinationDescription_bufferingHints :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe ElasticsearchBufferingHints)
elasticsearchDestinationDescription_bufferingHints = Lens.lens (\ElasticsearchDestinationDescription' {bufferingHints} -> bufferingHints) (\s@ElasticsearchDestinationDescription' {} a -> s {bufferingHints = a} :: ElasticsearchDestinationDescription)

-- | The Amazon ES retry options.
elasticsearchDestinationDescription_retryOptions :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe ElasticsearchRetryOptions)
elasticsearchDestinationDescription_retryOptions = Lens.lens (\ElasticsearchDestinationDescription' {retryOptions} -> retryOptions) (\s@ElasticsearchDestinationDescription' {} a -> s {retryOptions = a} :: ElasticsearchDestinationDescription)

-- | The Amazon S3 backup mode.
elasticsearchDestinationDescription_s3BackupMode :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe ElasticsearchS3BackupMode)
elasticsearchDestinationDescription_s3BackupMode = Lens.lens (\ElasticsearchDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@ElasticsearchDestinationDescription' {} a -> s {s3BackupMode = a} :: ElasticsearchDestinationDescription)

-- | The Amazon S3 destination.
elasticsearchDestinationDescription_s3DestinationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe S3DestinationDescription)
elasticsearchDestinationDescription_s3DestinationDescription = Lens.lens (\ElasticsearchDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@ElasticsearchDestinationDescription' {} a -> s {s3DestinationDescription = a} :: ElasticsearchDestinationDescription)

instance
  Core.FromJSON
    ElasticsearchDestinationDescription
  where
  parseJSON =
    Core.withObject
      "ElasticsearchDestinationDescription"
      ( \x ->
          ElasticsearchDestinationDescription'
            Core.<$> (x Core..:? "TypeName")
            Core.<*> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "ClusterEndpoint")
            Core.<*> (x Core..:? "IndexName")
            Core.<*> (x Core..:? "ProcessingConfiguration")
            Core.<*> (x Core..:? "CloudWatchLoggingOptions")
            Core.<*> (x Core..:? "DomainARN")
            Core.<*> (x Core..:? "IndexRotationPeriod")
            Core.<*> (x Core..:? "VpcConfigurationDescription")
            Core.<*> (x Core..:? "BufferingHints")
            Core.<*> (x Core..:? "RetryOptions")
            Core.<*> (x Core..:? "S3BackupMode")
            Core.<*> (x Core..:? "S3DestinationDescription")
      )

instance
  Core.Hashable
    ElasticsearchDestinationDescription

instance
  Core.NFData
    ElasticsearchDestinationDescription
