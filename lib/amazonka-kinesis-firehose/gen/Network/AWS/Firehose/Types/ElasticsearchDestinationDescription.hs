{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchDestinationDescription where

import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.VPCConfigurationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The destination description in Amazon ES.
--
--
--
-- /See:/ 'elasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
  { _eddIndexRotationPeriod ::
      !( Maybe
           ElasticsearchIndexRotationPeriod
       ),
    _eddTypeName ::
      !(Maybe Text),
    _eddS3BackupMode ::
      !( Maybe
           ElasticsearchS3BackupMode
       ),
    _eddDomainARN ::
      !(Maybe Text),
    _eddVPCConfigurationDescription ::
      !( Maybe
           VPCConfigurationDescription
       ),
    _eddCloudWatchLoggingOptions ::
      !( Maybe
           CloudWatchLoggingOptions
       ),
    _eddS3DestinationDescription ::
      !( Maybe
           S3DestinationDescription
       ),
    _eddBufferingHints ::
      !( Maybe
           ElasticsearchBufferingHints
       ),
    _eddRetryOptions ::
      !( Maybe
           ElasticsearchRetryOptions
       ),
    _eddProcessingConfiguration ::
      !( Maybe
           ProcessingConfiguration
       ),
    _eddRoleARN ::
      !(Maybe Text),
    _eddClusterEndpoint ::
      !(Maybe Text),
    _eddIndexName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eddIndexRotationPeriod' - The Elasticsearch index rotation period
--
-- * 'eddTypeName' - The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
--
-- * 'eddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'eddDomainARN' - The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
--
-- * 'eddVPCConfigurationDescription' - The details of the VPC of the Amazon ES destination.
--
-- * 'eddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options.
--
-- * 'eddS3DestinationDescription' - The Amazon S3 destination.
--
-- * 'eddBufferingHints' - The buffering options.
--
-- * 'eddRetryOptions' - The Amazon ES retry options.
--
-- * 'eddProcessingConfiguration' - The data processing configuration.
--
-- * 'eddRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'eddClusterEndpoint' - The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
--
-- * 'eddIndexName' - The Elasticsearch index name.
elasticsearchDestinationDescription ::
  ElasticsearchDestinationDescription
elasticsearchDestinationDescription =
  ElasticsearchDestinationDescription'
    { _eddIndexRotationPeriod =
        Nothing,
      _eddTypeName = Nothing,
      _eddS3BackupMode = Nothing,
      _eddDomainARN = Nothing,
      _eddVPCConfigurationDescription = Nothing,
      _eddCloudWatchLoggingOptions = Nothing,
      _eddS3DestinationDescription = Nothing,
      _eddBufferingHints = Nothing,
      _eddRetryOptions = Nothing,
      _eddProcessingConfiguration = Nothing,
      _eddRoleARN = Nothing,
      _eddClusterEndpoint = Nothing,
      _eddIndexName = Nothing
    }

-- | The Elasticsearch index rotation period
eddIndexRotationPeriod :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchIndexRotationPeriod)
eddIndexRotationPeriod = lens _eddIndexRotationPeriod (\s a -> s {_eddIndexRotationPeriod = a})

-- | The Elasticsearch type name. This applies to Elasticsearch 6.x and lower versions. For Elasticsearch 7.x, there's no value for @TypeName@ .
eddTypeName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddTypeName = lens _eddTypeName (\s a -> s {_eddTypeName = a})

-- | The Amazon S3 backup mode.
eddS3BackupMode :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchS3BackupMode)
eddS3BackupMode = lens _eddS3BackupMode (\s a -> s {_eddS3BackupMode = a})

-- | The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . Kinesis Data Firehose uses either @ClusterEndpoint@ or @DomainARN@ to send data to Amazon ES.
eddDomainARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddDomainARN = lens _eddDomainARN (\s a -> s {_eddDomainARN = a})

-- | The details of the VPC of the Amazon ES destination.
eddVPCConfigurationDescription :: Lens' ElasticsearchDestinationDescription (Maybe VPCConfigurationDescription)
eddVPCConfigurationDescription = lens _eddVPCConfigurationDescription (\s a -> s {_eddVPCConfigurationDescription = a})

-- | The Amazon CloudWatch logging options.
eddCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationDescription (Maybe CloudWatchLoggingOptions)
eddCloudWatchLoggingOptions = lens _eddCloudWatchLoggingOptions (\s a -> s {_eddCloudWatchLoggingOptions = a})

-- | The Amazon S3 destination.
eddS3DestinationDescription :: Lens' ElasticsearchDestinationDescription (Maybe S3DestinationDescription)
eddS3DestinationDescription = lens _eddS3DestinationDescription (\s a -> s {_eddS3DestinationDescription = a})

-- | The buffering options.
eddBufferingHints :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchBufferingHints)
eddBufferingHints = lens _eddBufferingHints (\s a -> s {_eddBufferingHints = a})

-- | The Amazon ES retry options.
eddRetryOptions :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchRetryOptions)
eddRetryOptions = lens _eddRetryOptions (\s a -> s {_eddRetryOptions = a})

-- | The data processing configuration.
eddProcessingConfiguration :: Lens' ElasticsearchDestinationDescription (Maybe ProcessingConfiguration)
eddProcessingConfiguration = lens _eddProcessingConfiguration (\s a -> s {_eddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
eddRoleARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddRoleARN = lens _eddRoleARN (\s a -> s {_eddRoleARN = a})

-- | The endpoint to use when communicating with the cluster. Kinesis Data Firehose uses either this @ClusterEndpoint@ or the @DomainARN@ field to send data to Amazon ES.
eddClusterEndpoint :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddClusterEndpoint = lens _eddClusterEndpoint (\s a -> s {_eddClusterEndpoint = a})

-- | The Elasticsearch index name.
eddIndexName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddIndexName = lens _eddIndexName (\s a -> s {_eddIndexName = a})

instance FromJSON ElasticsearchDestinationDescription where
  parseJSON =
    withObject
      "ElasticsearchDestinationDescription"
      ( \x ->
          ElasticsearchDestinationDescription'
            <$> (x .:? "IndexRotationPeriod")
            <*> (x .:? "TypeName")
            <*> (x .:? "S3BackupMode")
            <*> (x .:? "DomainARN")
            <*> (x .:? "VpcConfigurationDescription")
            <*> (x .:? "CloudWatchLoggingOptions")
            <*> (x .:? "S3DestinationDescription")
            <*> (x .:? "BufferingHints")
            <*> (x .:? "RetryOptions")
            <*> (x .:? "ProcessingConfiguration")
            <*> (x .:? "RoleARN")
            <*> (x .:? "ClusterEndpoint")
            <*> (x .:? "IndexName")
      )

instance Hashable ElasticsearchDestinationDescription

instance NFData ElasticsearchDestinationDescription
