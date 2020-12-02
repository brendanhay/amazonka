{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.VPCConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a destination in Amazon ES.
--
--
--
-- /See:/ 'elasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
  { _edcIndexRotationPeriod ::
      !( Maybe
           ElasticsearchIndexRotationPeriod
       ),
    _edcTypeName ::
      !(Maybe Text),
    _edcS3BackupMode ::
      !( Maybe
           ElasticsearchS3BackupMode
       ),
    _edcDomainARN ::
      !(Maybe Text),
    _edcCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _edcVPCConfiguration ::
      !( Maybe
           VPCConfiguration
       ),
    _edcBufferingHints ::
      !( Maybe
           ElasticsearchBufferingHints
       ),
    _edcRetryOptions ::
      !( Maybe
           ElasticsearchRetryOptions
       ),
    _edcProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _edcClusterEndpoint ::
      !(Maybe Text),
    _edcRoleARN ::
      !Text,
    _edcIndexName ::
      !Text,
    _edcS3Configuration ::
      !S3DestinationConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
--
-- * 'edcTypeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time. For Elasticsearch 7.x, don't specify a @TypeName@ .
--
-- * 'edcS3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ . You can't change this backup mode after you create the delivery stream.
--
-- * 'edcDomainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . Specify either @ClusterEndpoint@ or @DomainARN@ .
--
-- * 'edcCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'edcVPCConfiguration' - The details of the VPC of the Amazon ES destination.
--
-- * 'edcBufferingHints' - The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
--
-- * 'edcRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- * 'edcProcessingConfiguration' - The data processing configuration.
--
-- * 'edcClusterEndpoint' - The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
--
-- * 'edcRoleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'edcIndexName' - The Elasticsearch index name.
--
-- * 'edcS3Configuration' - The configuration for the backup Amazon S3 location.
elasticsearchDestinationConfiguration ::
  -- | 'edcRoleARN'
  Text ->
  -- | 'edcIndexName'
  Text ->
  -- | 'edcS3Configuration'
  S3DestinationConfiguration ->
  ElasticsearchDestinationConfiguration
elasticsearchDestinationConfiguration
  pRoleARN_
  pIndexName_
  pS3Configuration_ =
    ElasticsearchDestinationConfiguration'
      { _edcIndexRotationPeriod =
          Nothing,
        _edcTypeName = Nothing,
        _edcS3BackupMode = Nothing,
        _edcDomainARN = Nothing,
        _edcCloudWatchLoggingOptions = Nothing,
        _edcVPCConfiguration = Nothing,
        _edcBufferingHints = Nothing,
        _edcRetryOptions = Nothing,
        _edcProcessingConfiguration = Nothing,
        _edcClusterEndpoint = Nothing,
        _edcRoleARN = pRoleARN_,
        _edcIndexName = pIndexName_,
        _edcS3Configuration = pS3Configuration_
      }

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
edcIndexRotationPeriod :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchIndexRotationPeriod)
edcIndexRotationPeriod = lens _edcIndexRotationPeriod (\s a -> s {_edcIndexRotationPeriod = a})

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time. For Elasticsearch 7.x, don't specify a @TypeName@ .
edcTypeName :: Lens' ElasticsearchDestinationConfiguration (Maybe Text)
edcTypeName = lens _edcTypeName (\s a -> s {_edcTypeName = a})

-- | Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ . You can't change this backup mode after you create the delivery stream.
edcS3BackupMode :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchS3BackupMode)
edcS3BackupMode = lens _edcS3BackupMode (\s a -> s {_edcS3BackupMode = a})

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . Specify either @ClusterEndpoint@ or @DomainARN@ .
edcDomainARN :: Lens' ElasticsearchDestinationConfiguration (Maybe Text)
edcDomainARN = lens _edcDomainARN (\s a -> s {_edcDomainARN = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
edcCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe CloudWatchLoggingOptions)
edcCloudWatchLoggingOptions = lens _edcCloudWatchLoggingOptions (\s a -> s {_edcCloudWatchLoggingOptions = a})

-- | The details of the VPC of the Amazon ES destination.
edcVPCConfiguration :: Lens' ElasticsearchDestinationConfiguration (Maybe VPCConfiguration)
edcVPCConfiguration = lens _edcVPCConfiguration (\s a -> s {_edcVPCConfiguration = a})

-- | The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
edcBufferingHints :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchBufferingHints)
edcBufferingHints = lens _edcBufferingHints (\s a -> s {_edcBufferingHints = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
edcRetryOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchRetryOptions)
edcRetryOptions = lens _edcRetryOptions (\s a -> s {_edcRetryOptions = a})

-- | The data processing configuration.
edcProcessingConfiguration :: Lens' ElasticsearchDestinationConfiguration (Maybe ProcessingConfiguration)
edcProcessingConfiguration = lens _edcProcessingConfiguration (\s a -> s {_edcProcessingConfiguration = a})

-- | The endpoint to use when communicating with the cluster. Specify either this @ClusterEndpoint@ or the @DomainARN@ field.
edcClusterEndpoint :: Lens' ElasticsearchDestinationConfiguration (Maybe Text)
edcClusterEndpoint = lens _edcClusterEndpoint (\s a -> s {_edcClusterEndpoint = a})

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
edcRoleARN :: Lens' ElasticsearchDestinationConfiguration Text
edcRoleARN = lens _edcRoleARN (\s a -> s {_edcRoleARN = a})

-- | The Elasticsearch index name.
edcIndexName :: Lens' ElasticsearchDestinationConfiguration Text
edcIndexName = lens _edcIndexName (\s a -> s {_edcIndexName = a})

-- | The configuration for the backup Amazon S3 location.
edcS3Configuration :: Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
edcS3Configuration = lens _edcS3Configuration (\s a -> s {_edcS3Configuration = a})

instance Hashable ElasticsearchDestinationConfiguration

instance NFData ElasticsearchDestinationConfiguration

instance ToJSON ElasticsearchDestinationConfiguration where
  toJSON ElasticsearchDestinationConfiguration' {..} =
    object
      ( catMaybes
          [ ("IndexRotationPeriod" .=) <$> _edcIndexRotationPeriod,
            ("TypeName" .=) <$> _edcTypeName,
            ("S3BackupMode" .=) <$> _edcS3BackupMode,
            ("DomainARN" .=) <$> _edcDomainARN,
            ("CloudWatchLoggingOptions" .=) <$> _edcCloudWatchLoggingOptions,
            ("VpcConfiguration" .=) <$> _edcVPCConfiguration,
            ("BufferingHints" .=) <$> _edcBufferingHints,
            ("RetryOptions" .=) <$> _edcRetryOptions,
            ("ProcessingConfiguration" .=) <$> _edcProcessingConfiguration,
            ("ClusterEndpoint" .=) <$> _edcClusterEndpoint,
            Just ("RoleARN" .= _edcRoleARN),
            Just ("IndexName" .= _edcIndexName),
            Just ("S3Configuration" .= _edcS3Configuration)
          ]
      )
