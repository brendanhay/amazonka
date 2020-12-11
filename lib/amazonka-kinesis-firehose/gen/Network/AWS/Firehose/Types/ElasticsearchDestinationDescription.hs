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
    eddIndexRotationPeriod,
    eddTypeName,
    eddS3BackupMode,
    eddDomainARN,
    eddVPCConfigurationDescription,
    eddCloudWatchLoggingOptions,
    eddS3DestinationDescription,
    eddBufferingHints,
    eddRetryOptions,
    eddProcessingConfiguration,
    eddRoleARN,
    eddClusterEndpoint,
    eddIndexName,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.VPCConfigurationDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The destination description in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
  { indexRotationPeriod ::
      Lude.Maybe
        ElasticsearchIndexRotationPeriod,
    typeName ::
      Lude.Maybe
        Lude.Text,
    s3BackupMode ::
      Lude.Maybe
        ElasticsearchS3BackupMode,
    domainARN ::
      Lude.Maybe
        Lude.Text,
    vpcConfigurationDescription ::
      Lude.Maybe
        VPCConfigurationDescription,
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    s3DestinationDescription ::
      Lude.Maybe
        S3DestinationDescription,
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
      Lude.Maybe
        Lude.Text,
    clusterEndpoint ::
      Lude.Maybe
        Lude.Text,
    indexName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDestinationDescription' with the minimum fields required to make a request.
--
-- * 'bufferingHints' - The buffering options.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options.
-- * 'clusterEndpoint' - The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
-- * 'domainARN' - The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
-- * 'indexName' - The Elasticsearch index name.
-- * 'indexRotationPeriod' - The Elasticsearch index rotation period
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The Amazon ES retry options.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupMode' - The Amazon S3 backup mode.
-- * 's3DestinationDescription' - The Amazon S3 destination.
-- * 'typeName' - The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
-- * 'vpcConfigurationDescription' - The details of the VPC of the Amazon ES destination.
mkElasticsearchDestinationDescription ::
  ElasticsearchDestinationDescription
mkElasticsearchDestinationDescription =
  ElasticsearchDestinationDescription'
    { indexRotationPeriod =
        Lude.Nothing,
      typeName = Lude.Nothing,
      s3BackupMode = Lude.Nothing,
      domainARN = Lude.Nothing,
      vpcConfigurationDescription = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      s3DestinationDescription = Lude.Nothing,
      bufferingHints = Lude.Nothing,
      retryOptions = Lude.Nothing,
      processingConfiguration = Lude.Nothing,
      roleARN = Lude.Nothing,
      clusterEndpoint = Lude.Nothing,
      indexName = Lude.Nothing
    }

-- | The Elasticsearch index rotation period
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe ElasticsearchIndexRotationPeriod)
eddIndexRotationPeriod = Lens.lens (indexRotationPeriod :: ElasticsearchDestinationDescription -> Lude.Maybe ElasticsearchIndexRotationPeriod) (\s a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddIndexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead." #-}

-- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddTypeName :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe Lude.Text)
eddTypeName = Lens.lens (typeName :: ElasticsearchDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon S3 backup mode.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddS3BackupMode :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe ElasticsearchS3BackupMode)
eddS3BackupMode = Lens.lens (s3BackupMode :: ElasticsearchDestinationDescription -> Lude.Maybe ElasticsearchS3BackupMode) (\s a -> s {s3BackupMode = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddDomainARN :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe Lude.Text)
eddDomainARN = Lens.lens (domainARN :: ElasticsearchDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The details of the VPC of the Amazon ES destination.
--
-- /Note:/ Consider using 'vpcConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddVPCConfigurationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe VPCConfigurationDescription)
eddVPCConfigurationDescription = Lens.lens (vpcConfigurationDescription :: ElasticsearchDestinationDescription -> Lude.Maybe VPCConfigurationDescription) (\s a -> s {vpcConfigurationDescription = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddVPCConfigurationDescription "Use generic-lens or generic-optics with 'vpcConfigurationDescription' instead." #-}

-- | The Amazon CloudWatch logging options.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe CloudWatchLoggingOptions)
eddCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ElasticsearchDestinationDescription -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The Amazon S3 destination.
--
-- /Note:/ Consider using 's3DestinationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddS3DestinationDescription :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe S3DestinationDescription)
eddS3DestinationDescription = Lens.lens (s3DestinationDescription :: ElasticsearchDestinationDescription -> Lude.Maybe S3DestinationDescription) (\s a -> s {s3DestinationDescription = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddS3DestinationDescription "Use generic-lens or generic-optics with 's3DestinationDescription' instead." #-}

-- | The buffering options.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddBufferingHints :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe ElasticsearchBufferingHints)
eddBufferingHints = Lens.lens (bufferingHints :: ElasticsearchDestinationDescription -> Lude.Maybe ElasticsearchBufferingHints) (\s a -> s {bufferingHints = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The Amazon ES retry options.
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddRetryOptions :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe ElasticsearchRetryOptions)
eddRetryOptions = Lens.lens (retryOptions :: ElasticsearchDestinationDescription -> Lude.Maybe ElasticsearchRetryOptions) (\s a -> s {retryOptions = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe ProcessingConfiguration)
eddProcessingConfiguration = Lens.lens (processingConfiguration :: ElasticsearchDestinationDescription -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddRoleARN :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe Lude.Text)
eddRoleARN = Lens.lens (roleARN :: ElasticsearchDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddClusterEndpoint :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe Lude.Text)
eddClusterEndpoint = Lens.lens (clusterEndpoint :: ElasticsearchDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {clusterEndpoint = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddClusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead." #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eddIndexName :: Lens.Lens' ElasticsearchDestinationDescription (Lude.Maybe Lude.Text)
eddIndexName = Lens.lens (indexName :: ElasticsearchDestinationDescription -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: ElasticsearchDestinationDescription)
{-# DEPRECATED eddIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Lude.FromJSON ElasticsearchDestinationDescription where
  parseJSON =
    Lude.withObject
      "ElasticsearchDestinationDescription"
      ( \x ->
          ElasticsearchDestinationDescription'
            Lude.<$> (x Lude..:? "IndexRotationPeriod")
            Lude.<*> (x Lude..:? "TypeName")
            Lude.<*> (x Lude..:? "S3BackupMode")
            Lude.<*> (x Lude..:? "DomainARN")
            Lude.<*> (x Lude..:? "VpcConfigurationDescription")
            Lude.<*> (x Lude..:? "CloudWatchLoggingOptions")
            Lude.<*> (x Lude..:? "S3DestinationDescription")
            Lude.<*> (x Lude..:? "BufferingHints")
            Lude.<*> (x Lude..:? "RetryOptions")
            Lude.<*> (x Lude..:? "ProcessingConfiguration")
            Lude.<*> (x Lude..:? "RoleARN")
            Lude.<*> (x Lude..:? "ClusterEndpoint")
            Lude.<*> (x Lude..:? "IndexName")
      )
