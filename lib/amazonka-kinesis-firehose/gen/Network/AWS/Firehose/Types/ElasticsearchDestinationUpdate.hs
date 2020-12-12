{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
  ( ElasticsearchDestinationUpdate (..),

    -- * Smart constructor
    mkElasticsearchDestinationUpdate,

    -- * Lenses
    eduIndexRotationPeriod,
    eduTypeName,
    eduDomainARN,
    eduCloudWatchLoggingOptions,
    eduS3Update,
    eduBufferingHints,
    eduRetryOptions,
    eduProcessingConfiguration,
    eduRoleARN,
    eduClusterEndpoint,
    eduIndexName,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an update for a destination in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationUpdate' smart constructor.
data ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate'
  { indexRotationPeriod ::
      Lude.Maybe
        ElasticsearchIndexRotationPeriod,
    typeName ::
      Lude.Maybe Lude.Text,
    domainARN ::
      Lude.Maybe Lude.Text,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    s3Update ::
      Lude.Maybe
        S3DestinationUpdate,
    bufferingHints ::
      Lude.Maybe
        ElasticsearchBufferingHints,
    retryOptions ::
      Lude.Maybe
        ElasticsearchRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    roleARN ::
      Lude.Maybe Lude.Text,
    clusterEndpoint ::
      Lude.Maybe Lude.Text,
    indexName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDestinationUpdate' with the minimum fields required to make a request.
--
-- * 'bufferingHints' - The buffering options. If no value is specified, @ElasticsearchBufferingHints@ object default values are used.
-- * 'cloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
-- * 'clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
-- * 'domainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in @RoleARN@ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
-- * 'indexName' - The Elasticsearch index name.
-- * 'indexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3Update' - The Amazon S3 destination.
-- * 'typeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your delivery stream, Kinesis Data Firehose still delivers data to Elasticsearch with the old index name and type name. If you want to update your delivery stream with a new index name, provide an empty string for @TypeName@ .
mkElasticsearchDestinationUpdate ::
  ElasticsearchDestinationUpdate
mkElasticsearchDestinationUpdate =
  ElasticsearchDestinationUpdate'
    { indexRotationPeriod =
        Lude.Nothing,
      typeName = Lude.Nothing,
      domainARN = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      s3Update = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      retryOptions = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      roleARN = Lude.Nothing,
      clusterEndpoint = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe ElasticsearchIndexRotationPeriod)
eduIndexRotationPeriod = Lens.lens (indexRotationPeriod :: ElasticsearchDestinationUpdate -> Lude.Maybe ElasticsearchIndexRotationPeriod) (\s a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduIndexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead." #-}

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
--
-- If you upgrade Elasticsearch from 6.x to 7.x and don’t update your delivery stream, Kinesis Data Firehose still delivers data to Elasticsearch with the old index name and type name. If you want to update your delivery stream with a new index name, provide an empty string for @TypeName@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduTypeName :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe Lude.Text)
eduTypeName = Lens.lens (typeName :: ElasticsearchDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in @RoleARN@ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduDomainARN :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe Lude.Text)
eduDomainARN = Lens.lens (domainARN :: ElasticsearchDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe CloudWatchLoggingOptions)
eduCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ElasticsearchDestinationUpdate -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3Update' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduS3Update :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe S3DestinationUpdate)
eduS3Update = Lens.lens (s3Update :: ElasticsearchDestinationUpdate -> Lude.Maybe S3DestinationUpdate) (\s a -> s {s3Update = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduS3Update "Use generic-lens or generic-optics with 's3Update' instead." #-}

-- | The buffering options. If no value is specified, @ElasticsearchBufferingHints@ object default values are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduBufferingHints :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe ElasticsearchBufferingHints)
eduBufferingHints = Lens.lens (bufferingHints :: ElasticsearchDestinationUpdate -> Lude.Maybe ElasticsearchBufferingHints) (\s a -> s {bufferingHints = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduRetryOptions :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe ElasticsearchRetryOptions)
eduRetryOptions = Lens.lens (retryOptions :: ElasticsearchDestinationUpdate -> Lude.Maybe ElasticsearchRetryOptions) (\s a -> s {retryOptions = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe ProcessingConfiguration)
eduProcessingConfiguration = Lens.lens (processingConfiguration :: ElasticsearchDestinationUpdate -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduRoleARN :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe Lude.Text)
eduRoleARN = Lens.lens (roleARN :: ElasticsearchDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduClusterEndpoint :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe Lude.Text)
eduClusterEndpoint = Lens.lens (clusterEndpoint :: ElasticsearchDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {clusterEndpoint = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduClusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead." #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eduIndexName :: Lens.Lens' ElasticsearchDestinationUpdate (Lude.Maybe Lude.Text)
eduIndexName = Lens.lens (indexName :: ElasticsearchDestinationUpdate -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ElasticsearchDestinationUpdate)
{-# DEPRECATED eduIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.ToJSON ElasticsearchDestinationUpdate where
  toJSON ElasticsearchDestinationUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IndexRotationPeriod" Lude..=) Lude.<$> indexRotationPeriod,
            ("TypeName" Lude..=) Lude.<$> typeName,
            ("DomainARN" Lude..=) Lude.<$> domainARN,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("S3Update" Lude..=) Lude.<$> s3Update,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("RoleARN" Lude..=) Lude.<$> roleARN,
            ("ClusterEndpoint" Lude..=) Lude.<$> clusterEndpoint,
            ("IndexName" Lude..=) Lude.<$> indexName
          ]
      )
