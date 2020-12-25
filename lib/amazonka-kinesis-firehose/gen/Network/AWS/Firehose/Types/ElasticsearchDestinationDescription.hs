{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
  ( ElasticsearchDestinationDescription (..),

    -- * Smart constructor
    mkElasticsearchDestinationDescription,

    -- * Lenses
    eddBufferingHints,
    eddCloudWatchLoggingOptions,
    eddClusterEndpoint,
    eddDomainARN,
    eddIndexName,
    eddIndexRotationPeriod,
    eddProcessingConfiguration,
    eddRetryOptions,
    eddRoleARN,
    eddS3BackupMode,
    eddS3DestinationDescription,
    eddTypeName,
    eddVpcConfigurationDescription,
  )
where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchBufferingHints as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchClusterEndpoint as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchDomainARN as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchIndexName as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchRetryOptions as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchS3BackupMode as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchTypeName as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.VpcConfigurationDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The destination description in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
  { -- | The buffering options.
    bufferingHints :: Core.Maybe Types.ElasticsearchBufferingHints,
    -- | The Amazon CloudWatch logging options.
    cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions,
    -- | The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
    clusterEndpoint :: Core.Maybe Types.ElasticsearchClusterEndpoint,
    -- | The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    --
    -- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
    domainARN :: Core.Maybe Types.ElasticsearchDomainARN,
    -- | The Elasticsearch index name.
    indexName :: Core.Maybe Types.ElasticsearchIndexName,
    -- | The Elasticsearch index rotation period
    indexRotationPeriod :: Core.Maybe Types.ElasticsearchIndexRotationPeriod,
    -- | The data processing configuration.
    processingConfiguration :: Core.Maybe Types.ProcessingConfiguration,
    -- | The Amazon ES retry options.
    retryOptions :: Core.Maybe Types.ElasticsearchRetryOptions,
    -- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The Amazon S3 backup mode.
    s3BackupMode :: Core.Maybe Types.ElasticsearchS3BackupMode,
    -- | The Amazon S3 destination.
    s3DestinationDescription :: Core.Maybe Types.S3DestinationDescription,
    -- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
    typeName :: Core.Maybe Types.ElasticsearchTypeName,
    -- | The details of the VPC of the Amazon ES destination.
    vpcConfigurationDescription :: Core.Maybe Types.VpcConfigurationDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchDestinationDescription' value with any optional fields omitted.
mkElasticsearchDestinationDescription ::
  ElasticsearchDestinationDescription
mkElasticsearchDestinationDescription =
  ElasticsearchDestinationDescription'
    { bufferingHints =
        Core.Nothing,
      cloudWatchLoggingOptions = Core.Nothing,
      clusterEndpoint = Core.Nothing,
      domainARN = Core.Nothing,
      indexName = Core.Nothing,
      indexRotationPeriod = Core.Nothing,
      processingConfiguration = Core.Nothing,
      retryOptions = Core.Nothing,
      roleARN = Core.Nothing,
      s3BackupMode = Core.Nothing,
      s3DestinationDescription = Core.Nothing,
      typeName = Core.Nothing,
      vpcConfigurationDescription = Core.Nothing
    }

-- | The buffering options.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddBufferingHints :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchBufferingHints)
eddBufferingHints = Lens.field @"bufferingHints"
{-# DEPRECATED eddBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The Amazon CloudWatch logging options.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.CloudWatchLoggingOptions)
eddCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# DEPRECATED eddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddClusterEndpoint :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchClusterEndpoint)
eddClusterEndpoint = Lens.field @"clusterEndpoint"
{-# DEPRECATED eddClusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead." #-}

-- | The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddDomainARN :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchDomainARN)
eddDomainARN = Lens.field @"domainARN"
{-# DEPRECATED eddDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddIndexName :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchIndexName)
eddIndexName = Lens.field @"indexName"
{-# DEPRECATED eddIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The Elasticsearch index rotation period
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchIndexRotationPeriod)
eddIndexRotationPeriod = Lens.field @"indexRotationPeriod"
{-# DEPRECATED eddIndexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ProcessingConfiguration)
eddProcessingConfiguration = Lens.field @"processingConfiguration"
{-# DEPRECATED eddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon ES retry options.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddRetryOptions :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchRetryOptions)
eddRetryOptions = Lens.field @"retryOptions"
{-# DEPRECATED eddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddRoleARN :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.RoleARN)
eddRoleARN = Lens.field @"roleARN"
{-# DEPRECATED eddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddS3BackupMode :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchS3BackupMode)
eddS3BackupMode = Lens.field @"s3BackupMode"
{-# DEPRECATED eddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddS3DestinationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.S3DestinationDescription)
eddS3DestinationDescription = Lens.field @"s3DestinationDescription"
{-# DEPRECATED eddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddTypeName :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.ElasticsearchTypeName)
eddTypeName = Lens.field @"typeName"
{-# DEPRECATED eddTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The details of the VPC of the Amazon ES destination.
--
-- /Note:/ Consider using 'vpcConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddVpcConfigurationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Core.Maybe Types.VpcConfigurationDescription)
eddVpcConfigurationDescription = Lens.field @"vpcConfigurationDescription"
{-# DEPRECATED eddVpcConfigurationDescription "Use generic-lens or generic-optics with 'vpcConfigurationDescription' instead." #-}

instance Core.FromJSON ElasticsearchDestinationDescription where
  parseJSON =
    Core.withObject "ElasticsearchDestinationDescription" Core.$
      \x ->
        ElasticsearchDestinationDescription'
          Core.<$> (x Core..:? "BufferingHints")
          Core.<*> (x Core..:? "CloudWatchLoggingOptions")
          Core.<*> (x Core..:? "ClusterEndpoint")
          Core.<*> (x Core..:? "DomainARN")
          Core.<*> (x Core..:? "IndexName")
          Core.<*> (x Core..:? "IndexRotationPeriod")
          Core.<*> (x Core..:? "ProcessingConfiguration")
          Core.<*> (x Core..:? "RetryOptions")
          Core.<*> (x Core..:? "RoleARN")
          Core.<*> (x Core..:? "S3BackupMode")
          Core.<*> (x Core..:? "S3DestinationDescription")
          Core.<*> (x Core..:? "TypeName")
          Core.<*> (x Core..:? "VpcConfigurationDescription")
