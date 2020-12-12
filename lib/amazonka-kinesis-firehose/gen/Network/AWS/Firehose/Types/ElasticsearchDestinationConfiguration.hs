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
    edcIndexRotationPeriod,
    edcTypeName,
    edcS3BackupMode,
    edcDomainARN,
    edcCloudWatchLoggingOptions,
    edcVPCConfiguration,
    edcBufferingHints,
    edcRetryOptions,
    edcProcessingConfiguration,
    edcClusterEndpoint,
    edcRoleARN,
    edcIndexName,
    edcS3Configuration,
  )
where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.VPCConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a destination in Amazon ES.
--
-- /See:/ 'mkElasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
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
    cloudWatchLoggingOptions ::
      Lude.Maybe
        CloudWatchLoggingOptions,
    vpcConfiguration ::
      Lude.Maybe
        VPCConfiguration,
    bufferingHints ::
      Lude.Maybe
        ElasticsearchBufferingHints,
    retryOptions ::
      Lude.Maybe
        ElasticsearchRetryOptions,
    processingConfiguration ::
      Lude.Maybe
        ProcessingConfiguration,
    clusterEndpoint ::
      Lude.Maybe
        Lude.Text,
    roleARN ::
      Lude.Text,
    indexName ::
      Lude.Text,
    s3Configuration ::
      S3DestinationConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'bufferingHints' - The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
-- * 'cloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
-- * 'clusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
-- * 'domainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
-- * 'indexName' - The Elasticsearch index name.
-- * 'indexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
-- * 'processingConfiguration' - The data processing configuration.
-- * 'retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 's3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
--
-- You can't change this backup mode after you create the delivery stream.
-- * 's3Configuration' - The configuration for the backup Amazon S3 location.
-- * 'typeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
--
-- For Elasticsearch 7.x, don't specify a @TypeName@ .
-- * 'vpcConfiguration' - The details of the VPC of the Amazon ES destination.
mkElasticsearchDestinationConfiguration ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'indexName'
  Lude.Text ->
  -- | 's3Configuration'
  S3DestinationConfiguration ->
  ElasticsearchDestinationConfiguration
mkElasticsearchDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    ElasticsearchDestinationConfiguration'
      { indexRotationPeriod =
          Lude.Nothing,
        typeName = Lude.Nothing,
        s3BackupMode = Lude.Nothing,
        domainARN = Lude.Nothing,
        cloudWatchLoggingOptions = Lude.Nothing,
        vpcConfiguration = Lude.Nothing,
        bufferingHints = Lude.Nothing,
        retryOptions = Lude.Nothing,
        processingConfiguration = Lude.Nothing,
        clusterEndpoint = Lude.Nothing,
        roleARN = pRoleARN_,
        indexName = pIndexName_,
        s3Configuration = pS3Configuration_
      }

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
--
-- /Note:/ Consider using 'indexRotationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcIndexRotationPeriod :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe ElasticsearchIndexRotationPeriod)
edcIndexRotationPeriod = Lens.lens (indexRotationPeriod :: ElasticsearchDestinationConfiguration -> Lude.Maybe ElasticsearchIndexRotationPeriod) (\s a -> s {indexRotationPeriod = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcIndexRotationPeriod "Use generic-lens or generic-optics with 'indexRotationPeriod' instead." #-}

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
--
-- For Elasticsearch 7.x, don't specify a @TypeName@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcTypeName :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe Lude.Text)
edcTypeName = Lens.lens (typeName :: ElasticsearchDestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
--
-- You can't change this backup mode after you create the delivery stream.
--
-- /Note:/ Consider using 's3BackupMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcS3BackupMode :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe ElasticsearchS3BackupMode)
edcS3BackupMode = Lens.lens (s3BackupMode :: ElasticsearchDestinationConfiguration -> Lude.Maybe ElasticsearchS3BackupMode) (\s a -> s {s3BackupMode = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcS3BackupMode "Use generic-lens or generic-optics with 's3BackupMode' instead." #-}

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- Specify either @ClusterEndpoint@ or @DomainARN@ .
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcDomainARN :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe Lude.Text)
edcDomainARN = Lens.lens (domainARN :: ElasticsearchDestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The Amazon CloudWatch logging options for your delivery stream.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcCloudWatchLoggingOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe CloudWatchLoggingOptions)
edcCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: ElasticsearchDestinationConfiguration -> Lude.Maybe CloudWatchLoggingOptions) (\s a -> s {cloudWatchLoggingOptions = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | The details of the VPC of the Amazon ES destination.
--
-- /Note:/ Consider using 'vpcConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcVPCConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe VPCConfiguration)
edcVPCConfiguration = Lens.lens (vpcConfiguration :: ElasticsearchDestinationConfiguration -> Lude.Maybe VPCConfiguration) (\s a -> s {vpcConfiguration = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcVPCConfiguration "Use generic-lens or generic-optics with 'vpcConfiguration' instead." #-}

-- | The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
--
-- /Note:/ Consider using 'bufferingHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcBufferingHints :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe ElasticsearchBufferingHints)
edcBufferingHints = Lens.lens (bufferingHints :: ElasticsearchDestinationConfiguration -> Lude.Maybe ElasticsearchBufferingHints) (\s a -> s {bufferingHints = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcBufferingHints "Use generic-lens or generic-optics with 'bufferingHints' instead." #-}

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'retryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcRetryOptions :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe ElasticsearchRetryOptions)
edcRetryOptions = Lens.lens (retryOptions :: ElasticsearchDestinationConfiguration -> Lude.Maybe ElasticsearchRetryOptions) (\s a -> s {retryOptions = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcRetryOptions "Use generic-lens or generic-optics with 'retryOptions' instead." #-}

-- | The data processing configuration.
--
-- /Note:/ Consider using 'processingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcProcessingConfiguration :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe ProcessingConfiguration)
edcProcessingConfiguration = Lens.lens (processingConfiguration :: ElasticsearchDestinationConfiguration -> Lude.Maybe ProcessingConfiguration) (\s a -> s {processingConfiguration = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcProcessingConfiguration "Use generic-lens or generic-optics with 'processingConfiguration' instead." #-}

-- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- /Note:/ Consider using 'clusterEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcClusterEndpoint :: Lens.Lens' ElasticsearchDestinationConfiguration (Lude.Maybe Lude.Text)
edcClusterEndpoint = Lens.lens (clusterEndpoint :: ElasticsearchDestinationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {clusterEndpoint = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcClusterEndpoint "Use generic-lens or generic-optics with 'clusterEndpoint' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcRoleARN :: Lens.Lens' ElasticsearchDestinationConfiguration Lude.Text
edcRoleARN = Lens.lens (roleARN :: ElasticsearchDestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Elasticsearch index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcIndexName :: Lens.Lens' ElasticsearchDestinationConfiguration Lude.Text
edcIndexName = Lens.lens (indexName :: ElasticsearchDestinationConfiguration -> Lude.Text) (\s a -> s {indexName = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The configuration for the backup Amazon S3 location.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edcS3Configuration :: Lens.Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
edcS3Configuration = Lens.lens (s3Configuration :: ElasticsearchDestinationConfiguration -> S3DestinationConfiguration) (\s a -> s {s3Configuration = a} :: ElasticsearchDestinationConfiguration)
{-# DEPRECATED edcS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

instance Lude.ToJSON ElasticsearchDestinationConfiguration where
  toJSON ElasticsearchDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IndexRotationPeriod" Lude..=) Lude.<$> indexRotationPeriod,
            ("TypeName" Lude..=) Lude.<$> typeName,
            ("S3BackupMode" Lude..=) Lude.<$> s3BackupMode,
            ("DomainARN" Lude..=) Lude.<$> domainARN,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("VpcConfiguration" Lude..=) Lude.<$> vpcConfiguration,
            ("BufferingHints" Lude..=) Lude.<$> bufferingHints,
            ("RetryOptions" Lude..=) Lude.<$> retryOptions,
            ("ProcessingConfiguration" Lude..=)
              Lude.<$> processingConfiguration,
            ("ClusterEndpoint" Lude..=) Lude.<$> clusterEndpoint,
            Lude.Just ("RoleARN" Lude..= roleARN),
            Lude.Just ("IndexName" Lude..= indexName),
            Lude.Just ("S3Configuration" Lude..= s3Configuration)
          ]
      )
