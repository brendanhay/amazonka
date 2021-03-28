{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
  ( ElasticsearchDestinationUpdate (..)
  -- * Smart constructor
  , mkElasticsearchDestinationUpdate
  -- * Lenses
  , eduBufferingHints
  , eduCloudWatchLoggingOptions
  , eduClusterEndpoint
  , eduDomainARN
  , eduIndexName
  , eduIndexRotationPeriod
  , eduProcessingConfiguration
  , eduRetryOptions
  , eduRoleARN
  , eduS3Update
  , eduTypeName
  ) where

import qualified Network.AWS.Firehose.Types.CloudWatchLoggingOptions as Types
import qualified Network.AWS.Firehose.Types.ClusterEndpoint as Types
import qualified Network.AWS.Firehose.Types.DomainARN as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchBufferingHints as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod as Types
import qualified Network.AWS.Firehose.Types.ElasticsearchRetryOptions as Types
import qualified Network.AWS.Firehose.Types.IndexName as Types
import qualified Network.AWS.Firehose.Types.ProcessingConfiguration as Types
import qualified Network.AWS.Firehose.Types.RoleARN as Types
import qualified Network.AWS.Firehose.Types.S3DestinationUpdate as Types
import qualified Network.AWS.Firehose.Types.TypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an update for a destination in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationUpdate' smart constructor.
data ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate'
  { bufferingHints :: Core.Maybe Types.ElasticsearchBufferingHints
    -- ^ The buffering options. If no value is specified, @ElasticsearchBufferingHints@ object default values are used. 
  , cloudWatchLoggingOptions :: Core.Maybe Types.CloudWatchLoggingOptions
    -- ^ The CloudWatch logging options for your delivery stream.
  , clusterEndpoint :: Core.Maybe Types.ClusterEndpoint
    -- ^ The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
  , domainARN :: Core.Maybe Types.DomainARN
    -- ^ The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in @RoleARN@ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The Elasticsearch index name.
  , indexRotationPeriod :: Core.Maybe Types.ElasticsearchIndexRotationPeriod
    -- ^ The Elasticsearch index rotation period. Index rotation appends a timestamp to @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
  , processingConfiguration :: Core.Maybe Types.ProcessingConfiguration
    -- ^ The data processing configuration.
  , retryOptions :: Core.Maybe Types.ElasticsearchRetryOptions
    -- ^ The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , s3Update :: Core.Maybe Types.S3DestinationUpdate
    -- ^ The Amazon S3 destination.
  , typeName :: Core.Maybe Types.TypeName
    -- ^ The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your delivery stream, Kinesis Data Firehose still delivers data to Elasticsearch with the old index name and type name. If you want to update your delivery stream with a new index name, provide an empty string for @TypeName@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticsearchDestinationUpdate' value with any optional fields omitted.
mkElasticsearchDestinationUpdate
    :: ElasticsearchDestinationUpdate
mkElasticsearchDestinationUpdate
  = ElasticsearchDestinationUpdate'{bufferingHints = Core.Nothing,
                                    cloudWatchLoggingOptions = Core.Nothing,
                                    clusterEndpoint = Core.Nothing, domainARN = Core.Nothing,
                                    indexName = Core.Nothing, indexRotationPeriod = Core.Nothing,
                                    processingConfiguration = Core.Nothing,
                                    retryOptions = Core.Nothing, roleARN = Core.Nothing,
                                    s3Update = Core.Nothing, typeName = Core.Nothing}

-- | The buffering options. If no value is specified, @ElasticsearchBufferingHints@ object default values are used. 
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduBufferingHints :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.ElasticsearchBufferingHints)
eduBufferingHints = Lens.field @"bufferingHints"
{-# INLINEABLE eduBufferingHints #-}
{-# DEPRECATED bufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead"  #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.CloudWatchLoggingOptions)
eduCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE eduCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduClusterEndpoint :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.ClusterEndpoint)
eduClusterEndpoint = Lens.field @"clusterEndpoint"
{-# INLINEABLE eduClusterEndpoint #-}
{-# DEPRECATED clusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead"  #-}

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in @RoleARN@ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduDomainARN :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.DomainARN)
eduDomainARN = Lens.field @"domainARN"
{-# INLINEABLE eduDomainARN #-}
{-# DEPRECATED domainARN "Use generic-lens or generic-optics with 'domainARN' instead"  #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduIndexName :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.IndexName)
eduIndexName = Lens.field @"indexName"
{-# INLINEABLE eduIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.ElasticsearchIndexRotationPeriod)
eduIndexRotationPeriod = Lens.field @"indexRotationPeriod"
{-# INLINEABLE eduIndexRotationPeriod #-}
{-# DEPRECATED indexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead"  #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.ProcessingConfiguration)
eduProcessingConfiguration = Lens.field @"processingConfiguration"
{-# INLINEABLE eduProcessingConfiguration #-}
{-# DEPRECATED processingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead"  #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduRetryOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.ElasticsearchRetryOptions)
eduRetryOptions = Lens.field @"retryOptions"
{-# INLINEABLE eduRetryOptions #-}
{-# DEPRECATED retryOptions "Use generic-lens or generic-optics with 'retryOptions' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduRoleARN :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.RoleARN)
eduRoleARN = Lens.field @"roleARN"
{-# INLINEABLE eduRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduS3Update :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.S3DestinationUpdate)
eduS3Update = Lens.field @"s3Update"
{-# INLINEABLE eduS3Update #-}
{-# DEPRECATED s3Update "Use generic-lens or generic-optics with 's3Update' instead"  #-}

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your delivery stream, Kinesis Data Firehose still delivers data to Elasticsearch with the old index name and type name. If you want to update your delivery stream with a new index name, provide an empty string for @TypeName@ . 
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduTypeName :: Lens.Lens' ElasticsearchDestinationUpdate (Core.Maybe Types.TypeName)
eduTypeName = Lens.field @"typeName"
{-# INLINEABLE eduTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.FromJSON ElasticsearchDestinationUpdate where
        toJSON ElasticsearchDestinationUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("BufferingHints" Core..=) Core.<$> bufferingHints,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("ClusterEndpoint" Core..=) Core.<$> clusterEndpoint,
                  ("DomainARN" Core..=) Core.<$> domainARN,
                  ("IndexName" Core..=) Core.<$> indexName,
                  ("IndexRotationPeriod" Core..=) Core.<$> indexRotationPeriod,
                  ("ProcessingConfiguration" Core..=) Core.<$>
                    processingConfiguration,
                  ("RetryOptions" Core..=) Core.<$> retryOptions,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("S3Update" Core..=) Core.<$> s3Update,
                  ("TypeName" Core..=) Core.<$> typeName])
