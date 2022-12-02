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
-- Module      : Amazonka.Firehose.Types.ElasticsearchDestinationDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ElasticsearchDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.CloudWatchLoggingOptions
import Amazonka.Firehose.Types.ElasticsearchBufferingHints
import Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
import Amazonka.Firehose.Types.ElasticsearchRetryOptions
import Amazonka.Firehose.Types.ElasticsearchS3BackupMode
import Amazonka.Firehose.Types.ProcessingConfiguration
import Amazonka.Firehose.Types.S3DestinationDescription
import Amazonka.Firehose.Types.VpcConfigurationDescription
import qualified Amazonka.Prelude as Prelude

-- | The destination description in Amazon ES.
--
-- /See:/ 'newElasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
  { -- | The data processing configuration.
    processingConfiguration :: Prelude.Maybe ProcessingConfiguration,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The buffering options.
    bufferingHints :: Prelude.Maybe ElasticsearchBufferingHints,
    -- | The endpoint to use when communicating with the cluster. Kinesis Data
    -- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
    -- send data to Amazon ES.
    clusterEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon ES domain. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    --
    -- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
    -- send data to Amazon ES.
    domainARN :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
    -- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The Elasticsearch index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon CloudWatch logging options.
    cloudWatchLoggingOptions :: Prelude.Maybe CloudWatchLoggingOptions,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Prelude.Maybe ElasticsearchS3BackupMode,
    -- | The details of the VPC of the Amazon ES destination.
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription,
    -- | The Elasticsearch index rotation period
    indexRotationPeriod :: Prelude.Maybe ElasticsearchIndexRotationPeriod,
    -- | The Amazon ES retry options.
    retryOptions :: Prelude.Maybe ElasticsearchRetryOptions,
    -- | The Amazon S3 destination.
    s3DestinationDescription :: Prelude.Maybe S3DestinationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfiguration', 'elasticsearchDestinationDescription_processingConfiguration' - The data processing configuration.
--
-- 'roleARN', 'elasticsearchDestinationDescription_roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'bufferingHints', 'elasticsearchDestinationDescription_bufferingHints' - The buffering options.
--
-- 'clusterEndpoint', 'elasticsearchDestinationDescription_clusterEndpoint' - The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
-- send data to Amazon ES.
--
-- 'domainARN', 'elasticsearchDestinationDescription_domainARN' - The ARN of the Amazon ES domain. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
-- send data to Amazon ES.
--
-- 'typeName', 'elasticsearchDestinationDescription_typeName' - The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
-- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
--
-- 'indexName', 'elasticsearchDestinationDescription_indexName' - The Elasticsearch index name.
--
-- 'cloudWatchLoggingOptions', 'elasticsearchDestinationDescription_cloudWatchLoggingOptions' - The Amazon CloudWatch logging options.
--
-- 's3BackupMode', 'elasticsearchDestinationDescription_s3BackupMode' - The Amazon S3 backup mode.
--
-- 'vpcConfigurationDescription', 'elasticsearchDestinationDescription_vpcConfigurationDescription' - The details of the VPC of the Amazon ES destination.
--
-- 'indexRotationPeriod', 'elasticsearchDestinationDescription_indexRotationPeriod' - The Elasticsearch index rotation period
--
-- 'retryOptions', 'elasticsearchDestinationDescription_retryOptions' - The Amazon ES retry options.
--
-- 's3DestinationDescription', 'elasticsearchDestinationDescription_s3DestinationDescription' - The Amazon S3 destination.
newElasticsearchDestinationDescription ::
  ElasticsearchDestinationDescription
newElasticsearchDestinationDescription =
  ElasticsearchDestinationDescription'
    { processingConfiguration =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      bufferingHints = Prelude.Nothing,
      clusterEndpoint = Prelude.Nothing,
      domainARN = Prelude.Nothing,
      typeName = Prelude.Nothing,
      indexName = Prelude.Nothing,
      cloudWatchLoggingOptions =
        Prelude.Nothing,
      s3BackupMode = Prelude.Nothing,
      vpcConfigurationDescription =
        Prelude.Nothing,
      indexRotationPeriod = Prelude.Nothing,
      retryOptions = Prelude.Nothing,
      s3DestinationDescription =
        Prelude.Nothing
    }

-- | The data processing configuration.
elasticsearchDestinationDescription_processingConfiguration :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe ProcessingConfiguration)
elasticsearchDestinationDescription_processingConfiguration = Lens.lens (\ElasticsearchDestinationDescription' {processingConfiguration} -> processingConfiguration) (\s@ElasticsearchDestinationDescription' {} a -> s {processingConfiguration = a} :: ElasticsearchDestinationDescription)

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
elasticsearchDestinationDescription_roleARN :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe Prelude.Text)
elasticsearchDestinationDescription_roleARN = Lens.lens (\ElasticsearchDestinationDescription' {roleARN} -> roleARN) (\s@ElasticsearchDestinationDescription' {} a -> s {roleARN = a} :: ElasticsearchDestinationDescription)

-- | The buffering options.
elasticsearchDestinationDescription_bufferingHints :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe ElasticsearchBufferingHints)
elasticsearchDestinationDescription_bufferingHints = Lens.lens (\ElasticsearchDestinationDescription' {bufferingHints} -> bufferingHints) (\s@ElasticsearchDestinationDescription' {} a -> s {bufferingHints = a} :: ElasticsearchDestinationDescription)

-- | The endpoint to use when communicating with the cluster. Kinesis Data
-- Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to
-- send data to Amazon ES.
elasticsearchDestinationDescription_clusterEndpoint :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe Prelude.Text)
elasticsearchDestinationDescription_clusterEndpoint = Lens.lens (\ElasticsearchDestinationDescription' {clusterEndpoint} -> clusterEndpoint) (\s@ElasticsearchDestinationDescription' {} a -> s {clusterEndpoint = a} :: ElasticsearchDestinationDescription)

-- | The ARN of the Amazon ES domain. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to
-- send data to Amazon ES.
elasticsearchDestinationDescription_domainARN :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe Prelude.Text)
elasticsearchDestinationDescription_domainARN = Lens.lens (\ElasticsearchDestinationDescription' {domainARN} -> domainARN) (\s@ElasticsearchDestinationDescription' {} a -> s {domainARN = a} :: ElasticsearchDestinationDescription)

-- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower
-- versions. For Elasticsearch 7.x, there\'s no value for @TypeName@.
elasticsearchDestinationDescription_typeName :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe Prelude.Text)
elasticsearchDestinationDescription_typeName = Lens.lens (\ElasticsearchDestinationDescription' {typeName} -> typeName) (\s@ElasticsearchDestinationDescription' {} a -> s {typeName = a} :: ElasticsearchDestinationDescription)

-- | The Elasticsearch index name.
elasticsearchDestinationDescription_indexName :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe Prelude.Text)
elasticsearchDestinationDescription_indexName = Lens.lens (\ElasticsearchDestinationDescription' {indexName} -> indexName) (\s@ElasticsearchDestinationDescription' {} a -> s {indexName = a} :: ElasticsearchDestinationDescription)

-- | The Amazon CloudWatch logging options.
elasticsearchDestinationDescription_cloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe CloudWatchLoggingOptions)
elasticsearchDestinationDescription_cloudWatchLoggingOptions = Lens.lens (\ElasticsearchDestinationDescription' {cloudWatchLoggingOptions} -> cloudWatchLoggingOptions) (\s@ElasticsearchDestinationDescription' {} a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationDescription)

-- | The Amazon S3 backup mode.
elasticsearchDestinationDescription_s3BackupMode :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe ElasticsearchS3BackupMode)
elasticsearchDestinationDescription_s3BackupMode = Lens.lens (\ElasticsearchDestinationDescription' {s3BackupMode} -> s3BackupMode) (\s@ElasticsearchDestinationDescription' {} a -> s {s3BackupMode = a} :: ElasticsearchDestinationDescription)

-- | The details of the VPC of the Amazon ES destination.
elasticsearchDestinationDescription_vpcConfigurationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe VpcConfigurationDescription)
elasticsearchDestinationDescription_vpcConfigurationDescription = Lens.lens (\ElasticsearchDestinationDescription' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@ElasticsearchDestinationDescription' {} a -> s {vpcConfigurationDescription = a} :: ElasticsearchDestinationDescription)

-- | The Elasticsearch index rotation period
elasticsearchDestinationDescription_indexRotationPeriod :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe ElasticsearchIndexRotationPeriod)
elasticsearchDestinationDescription_indexRotationPeriod = Lens.lens (\ElasticsearchDestinationDescription' {indexRotationPeriod} -> indexRotationPeriod) (\s@ElasticsearchDestinationDescription' {} a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationDescription)

-- | The Amazon ES retry options.
elasticsearchDestinationDescription_retryOptions :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe ElasticsearchRetryOptions)
elasticsearchDestinationDescription_retryOptions = Lens.lens (\ElasticsearchDestinationDescription' {retryOptions} -> retryOptions) (\s@ElasticsearchDestinationDescription' {} a -> s {retryOptions = a} :: ElasticsearchDestinationDescription)

-- | The Amazon S3 destination.
elasticsearchDestinationDescription_s3DestinationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Prelude.Maybe S3DestinationDescription)
elasticsearchDestinationDescription_s3DestinationDescription = Lens.lens (\ElasticsearchDestinationDescription' {s3DestinationDescription} -> s3DestinationDescription) (\s@ElasticsearchDestinationDescription' {} a -> s {s3DestinationDescription = a} :: ElasticsearchDestinationDescription)

instance
  Data.FromJSON
    ElasticsearchDestinationDescription
  where
  parseJSON =
    Data.withObject
      "ElasticsearchDestinationDescription"
      ( \x ->
          ElasticsearchDestinationDescription'
            Prelude.<$> (x Data..:? "ProcessingConfiguration")
            Prelude.<*> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "BufferingHints")
            Prelude.<*> (x Data..:? "ClusterEndpoint")
            Prelude.<*> (x Data..:? "DomainARN")
            Prelude.<*> (x Data..:? "TypeName")
            Prelude.<*> (x Data..:? "IndexName")
            Prelude.<*> (x Data..:? "CloudWatchLoggingOptions")
            Prelude.<*> (x Data..:? "S3BackupMode")
            Prelude.<*> (x Data..:? "VpcConfigurationDescription")
            Prelude.<*> (x Data..:? "IndexRotationPeriod")
            Prelude.<*> (x Data..:? "RetryOptions")
            Prelude.<*> (x Data..:? "S3DestinationDescription")
      )

instance
  Prelude.Hashable
    ElasticsearchDestinationDescription
  where
  hashWithSalt
    _salt
    ElasticsearchDestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` processingConfiguration
        `Prelude.hashWithSalt` roleARN
        `Prelude.hashWithSalt` bufferingHints
        `Prelude.hashWithSalt` clusterEndpoint
        `Prelude.hashWithSalt` domainARN
        `Prelude.hashWithSalt` typeName
        `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` cloudWatchLoggingOptions
        `Prelude.hashWithSalt` s3BackupMode
        `Prelude.hashWithSalt` vpcConfigurationDescription
        `Prelude.hashWithSalt` indexRotationPeriod
        `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` s3DestinationDescription

instance
  Prelude.NFData
    ElasticsearchDestinationDescription
  where
  rnf ElasticsearchDestinationDescription' {..} =
    Prelude.rnf processingConfiguration
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf bufferingHints
      `Prelude.seq` Prelude.rnf clusterEndpoint
      `Prelude.seq` Prelude.rnf domainARN
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf cloudWatchLoggingOptions
      `Prelude.seq` Prelude.rnf s3BackupMode
      `Prelude.seq` Prelude.rnf vpcConfigurationDescription
      `Prelude.seq` Prelude.rnf indexRotationPeriod
      `Prelude.seq` Prelude.rnf retryOptions
      `Prelude.seq` Prelude.rnf s3DestinationDescription
