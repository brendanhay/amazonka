{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
  ( ElasticsearchDestinationConfiguration (..),

    -- * Smart constructor
    mkElasticsearchDestinationConfiguration,

    -- * Lenses
    edcRoleARN,
    edcIndexName,
    edcS3Configuration,
    edcBufferingHints,
    edcCloudWatchLoggingOptions,
    edcClusterEndpoint,
    edcDomainARN,
    edcIndexRotationPeriod,
    edcProcessingConfiguration,
    edcRetryOptions,
    edcS3BackupMode,
    edcTypeName,
    edcVpcConfiguration,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ClusterEndpoint as Types
import qualified Network.AWS.Firehose.Types.DomainARN as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchBufferingHints as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchRetryOptions as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.IndexName as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationConfiguration as Types
import qualified Network.AWS.Firehose.Types.TypeName as Types
import qualified Network.AWS.Firehose.Types.VpcConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of a destination in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
  { -- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Types.RoleARN,
    -- | The Elasticsearch index name.
    indexName :: Types.IndexName,
    -- | The configuration for the backup Amazon S3 location.
    s3Configuration :: Types.S3DestinationConfiguration,
    -- | The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
    bufferingHints :: Core.Maybe Types.ElasticsearchBufferingHints,
    -- | The Amazon CloudWatch logging options for your delivery stream.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
    clusterEndpoint :: Core.Maybe Types.ClusterEndpoint,
    -- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    --
    -- Specify either @ClusterEndpoint@ or @DomainARN@ .
    domainARN :: Core.Maybe Types.DomainARN,
    -- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
    indexRotationPeriod :: Core.Maybe Types.ElasticsearchIndexRotationPeriod,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
    retryOptions :: Core.Maybe Types.ElasticsearchRetryOptions,
    -- | Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
    --
    -- You can't change this backup mode after you create the delivery stream.
    s3BackupMode :: Core.Maybe Types.ElasticsearchS3BackupMode,
    -- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
    --
    -- For Elasticsearch 7.x, don't specify a @TypeName@ .
    typeName :: Core.Maybe Types.TypeName,
    -- | The details of the VPC of the Amazon ES destination.
    vpcConfiguration :: Core.Maybe Types.VpcConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchDestinationConfiguration' value with any optional fields omitted.
mkElasticsearchDestinationConfiguration ::
  -- | 'roleARN'
  Types.RoleARN ->
  -- | 'indexName'
  Types.IndexName ->
  -- | 's3Configuration'
  Types.S3DestinationConfiguration ->
  ElasticsearchDestinationConfiguration
mkElasticsearchDestinationConfiguration
  roleARN
  indexName
  s3Configuration =
    ElasticsearchDestinationConfiguration'
      { roleARN,
        indexName,
        s3Configuration,
        bufferingHints = Core.Nothing,
        cloudWatchLoggingOptions = Core.Nothing,
        clusterEndpoint = Core.Nothing,
        domainARN = Core.Nothing,
        indexRotationPeriod = Core.Nothing,
        processingConfiguration = Core.Nothing,
        retryOptions = Core.Nothing,
        s3BackupMode = Core.Nothing,
        typeName = Core.Nothing,
        vpcConfiguration = Core.Nothing
      }

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcRoleARN :: Lens.Lens' ElasticsearchDestinationConfiguration Types.RoleARN
edcRoleARN = Lens.field @"roleARN"
{-# DEPRECATED edcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcIndexName :: Lens.Lens' ElasticsearchDestinationConfiguration Types.IndexName
edcIndexName = Lens.field @"indexName"
{-# DEPRECATED edcIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The configuration for the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcS3Configuration :: Lens.Lens' ElasticsearchDestinationConfiguration Types.S3DestinationConfiguration
edcS3Configuration = Lens.field @"s3Configuration"
{-# DEPRECATED edcS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

-- | The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcBufferingHints :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ElasticsearchBufferingHints)
edcBufferingHints = Lens.field @"bufferingHints"
{-# DEPRECATED edcBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.CloudWatchLoggingOptions)
edcCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED edcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcClusterEndpoint :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ClusterEndpoint)
edcClusterEndpoint = Lens.field @"clusterEndpoint"
{-# DEPRECATED edcClusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead." #-}

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcDomainARN :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.DomainARN)
edcDomainARN = Lens.field @"domainARN"
{-# DEPRECATED edcDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ElasticsearchIndexRotationPeriod)
edcIndexRotationPeriod = Lens.field @"indexRotationPeriod"
{-# DEPRECATED edcIndexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ProcessingConfiguration)
edcProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED edcProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcRetryOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ElasticsearchRetryOptions)
edcRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED edcRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
--
-- You can't change this backup mode after you create the delivery stream.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcS3BackupMode :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.ElasticsearchS3BackupMode)
edcS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED edcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
--
-- For Elasticsearch 7.x, don't specify a @TypeName@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcTypeName :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.TypeName)
edcTypeName = Lens.field @"typeName"
{-# DEPRECATED edcTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The details of the VPC of the Amazon ES destination.
--
-- /Note:/ Consider using 'vpcConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcVpcConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Core.Maybe Types.VpcConfiguration)
edcVpcConfiguration = Lens.field @"vpcConfiguration"
{-# DEPRECATED edcVpcConfiguration "Use generic-lens or generic-optics with 'vpcConfiguration' instead." #-}

instance Core.FromJSON ElasticsearchDestinationConfiguration where
  toJSON ElasticsearchDestinationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RoleARN" Core..= roleARN),
            Core.Just ("IndexName" Core..= indexName),
            Core.Just ("S3Configuration" Core..= s3Configuration),
            ("BufferingHints" Core..=) Core.<$> bufferingHints,
            ("CloudWatchLoggingOptions" Core..=)
              Core.<$> cloudWatchLoggingOptions,
            ("ClusterEndpoint" Core..=) Core.<$> clusterEndpoint,
            ("DomainARN" Core..=) Core.<$> domainARN,
            ("IndexRotationPeriod" Core..=) Core.<$> indexRotationPeriod,
            ("ProcessingConfiguration" Core..=)
              Core.<$> processingConfiguration,
            ("RetryOptions" Core..=) Core.<$> retryOptions,
            ("S3BackupMode" Core..=) Core.<$> s3BackupMode,
            ("TypeName" Core..=) Core.<$> typeName,
            ("VpcConfiguration" Core..=) Core.<$> vpcConfiguration
          ]
      )
