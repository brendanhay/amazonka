{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Product where

import           Network.AWS.Firehose.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Describes hints for the buffering to perform before delivering data to the destination. Please note that these options are treated as hints, and therefore Firehose may choose to use different values when it is optimal.
--
--
--
-- /See:/ 'bufferingHints' smart constructor.
data BufferingHints = BufferingHints'
    { _bhSizeInMBs         :: !(Maybe Nat)
    , _bhIntervalInSeconds :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting SizeInMBs to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec set SizeInMBs to be 10 MB or higher.
--
-- * 'bhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300.
bufferingHints
    :: BufferingHints
bufferingHints =
    BufferingHints'
    { _bhSizeInMBs = Nothing
    , _bhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting SizeInMBs to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec set SizeInMBs to be 10 MB or higher.
bhSizeInMBs :: Lens' BufferingHints (Maybe Natural)
bhSizeInMBs = lens _bhSizeInMBs (\ s a -> s{_bhSizeInMBs = a}) . mapping _Nat;

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300.
bhIntervalInSeconds :: Lens' BufferingHints (Maybe Natural)
bhIntervalInSeconds = lens _bhIntervalInSeconds (\ s a -> s{_bhIntervalInSeconds = a}) . mapping _Nat;

instance FromJSON BufferingHints where
        parseJSON
          = withObject "BufferingHints"
              (\ x ->
                 BufferingHints' <$>
                   (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds"))

instance Hashable BufferingHints

instance NFData BufferingHints

instance ToJSON BufferingHints where
        toJSON BufferingHints'{..}
          = object
              (catMaybes
                 [("SizeInMBs" .=) <$> _bhSizeInMBs,
                  ("IntervalInSeconds" .=) <$> _bhIntervalInSeconds])

-- | Describes CloudWatch logging options for your delivery stream.
--
--
--
-- /See:/ 'cloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
    { _cwloEnabled       :: !(Maybe Bool)
    , _cwloLogGroupName  :: !(Maybe Text)
    , _cwloLogStreamName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudWatchLoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwloEnabled' - Enables or disables CloudWatch logging.
--
-- * 'cwloLogGroupName' - The CloudWatch group name for logging. This value is required if Enabled is true.
--
-- * 'cwloLogStreamName' - The CloudWatch log stream name for logging. This value is required if Enabled is true.
cloudWatchLoggingOptions
    :: CloudWatchLoggingOptions
cloudWatchLoggingOptions =
    CloudWatchLoggingOptions'
    { _cwloEnabled = Nothing
    , _cwloLogGroupName = Nothing
    , _cwloLogStreamName = Nothing
    }

-- | Enables or disables CloudWatch logging.
cwloEnabled :: Lens' CloudWatchLoggingOptions (Maybe Bool)
cwloEnabled = lens _cwloEnabled (\ s a -> s{_cwloEnabled = a});

-- | The CloudWatch group name for logging. This value is required if Enabled is true.
cwloLogGroupName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogGroupName = lens _cwloLogGroupName (\ s a -> s{_cwloLogGroupName = a});

-- | The CloudWatch log stream name for logging. This value is required if Enabled is true.
cwloLogStreamName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogStreamName = lens _cwloLogStreamName (\ s a -> s{_cwloLogStreamName = a});

instance FromJSON CloudWatchLoggingOptions where
        parseJSON
          = withObject "CloudWatchLoggingOptions"
              (\ x ->
                 CloudWatchLoggingOptions' <$>
                   (x .:? "Enabled") <*> (x .:? "LogGroupName") <*>
                     (x .:? "LogStreamName"))

instance Hashable CloudWatchLoggingOptions

instance NFData CloudWatchLoggingOptions

instance ToJSON CloudWatchLoggingOptions where
        toJSON CloudWatchLoggingOptions'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _cwloEnabled,
                  ("LogGroupName" .=) <$> _cwloLogGroupName,
                  ("LogStreamName" .=) <$> _cwloLogStreamName])

-- | Describes a @COPY@ command for Amazon Redshift.
--
--
--
-- /See:/ 'copyCommand' smart constructor.
data CopyCommand = CopyCommand'
    { _ccCopyOptions      :: !(Maybe Text)
    , _ccDataTableColumns :: !(Maybe Text)
    , _ccDataTableName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCopyOptions' - Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Firehose are as follows. @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
--
-- * 'ccDataTableColumns' - A comma-separated list of column names.
--
-- * 'ccDataTableName' - The name of the target table. The table must already exist in the database.
copyCommand
    :: Text -- ^ 'ccDataTableName'
    -> CopyCommand
copyCommand pDataTableName_ =
    CopyCommand'
    { _ccCopyOptions = Nothing
    , _ccDataTableColumns = Nothing
    , _ccDataTableName = pDataTableName_
    }

-- | Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Firehose are as follows. @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
ccCopyOptions :: Lens' CopyCommand (Maybe Text)
ccCopyOptions = lens _ccCopyOptions (\ s a -> s{_ccCopyOptions = a});

-- | A comma-separated list of column names.
ccDataTableColumns :: Lens' CopyCommand (Maybe Text)
ccDataTableColumns = lens _ccDataTableColumns (\ s a -> s{_ccDataTableColumns = a});

-- | The name of the target table. The table must already exist in the database.
ccDataTableName :: Lens' CopyCommand Text
ccDataTableName = lens _ccDataTableName (\ s a -> s{_ccDataTableName = a});

instance FromJSON CopyCommand where
        parseJSON
          = withObject "CopyCommand"
              (\ x ->
                 CopyCommand' <$>
                   (x .:? "CopyOptions") <*> (x .:? "DataTableColumns")
                     <*> (x .: "DataTableName"))

instance Hashable CopyCommand

instance NFData CopyCommand

instance ToJSON CopyCommand where
        toJSON CopyCommand'{..}
          = object
              (catMaybes
                 [("CopyOptions" .=) <$> _ccCopyOptions,
                  ("DataTableColumns" .=) <$> _ccDataTableColumns,
                  Just ("DataTableName" .= _ccDataTableName)])

-- | Contains information about a delivery stream.
--
--
--
-- /See:/ 'deliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
    { _dsdCreateTimestamp      :: !(Maybe POSIX)
    , _dsdLastUpdateTimestamp  :: !(Maybe POSIX)
    , _dsdDeliveryStreamName   :: !Text
    , _dsdDeliveryStreamARN    :: !Text
    , _dsdDeliveryStreamStatus :: !DeliveryStreamStatus
    , _dsdVersionId            :: !Text
    , _dsdDestinations         :: ![DestinationDescription]
    , _dsdHasMoreDestinations  :: !Bool
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdCreateTimestamp' - The date and time that the delivery stream was created.
--
-- * 'dsdLastUpdateTimestamp' - The date and time that the delivery stream was last updated.
--
-- * 'dsdDeliveryStreamName' - The name of the delivery stream.
--
-- * 'dsdDeliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream.
--
-- * 'dsdDeliveryStreamStatus' - The status of the delivery stream.
--
-- * 'dsdVersionId' - Used when calling the 'UpdateDestination' operation. Each time the destination is updated for the delivery stream, the VersionId is changed, and the current VersionId is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
--
-- * 'dsdDestinations' - The destinations.
--
-- * 'dsdHasMoreDestinations' - Indicates whether there are more destinations available to list.
deliveryStreamDescription
    :: Text -- ^ 'dsdDeliveryStreamName'
    -> Text -- ^ 'dsdDeliveryStreamARN'
    -> DeliveryStreamStatus -- ^ 'dsdDeliveryStreamStatus'
    -> Text -- ^ 'dsdVersionId'
    -> Bool -- ^ 'dsdHasMoreDestinations'
    -> DeliveryStreamDescription
deliveryStreamDescription pDeliveryStreamName_ pDeliveryStreamARN_ pDeliveryStreamStatus_ pVersionId_ pHasMoreDestinations_ =
    DeliveryStreamDescription'
    { _dsdCreateTimestamp = Nothing
    , _dsdLastUpdateTimestamp = Nothing
    , _dsdDeliveryStreamName = pDeliveryStreamName_
    , _dsdDeliveryStreamARN = pDeliveryStreamARN_
    , _dsdDeliveryStreamStatus = pDeliveryStreamStatus_
    , _dsdVersionId = pVersionId_
    , _dsdDestinations = mempty
    , _dsdHasMoreDestinations = pHasMoreDestinations_
    }

-- | The date and time that the delivery stream was created.
dsdCreateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdCreateTimestamp = lens _dsdCreateTimestamp (\ s a -> s{_dsdCreateTimestamp = a}) . mapping _Time;

-- | The date and time that the delivery stream was last updated.
dsdLastUpdateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdLastUpdateTimestamp = lens _dsdLastUpdateTimestamp (\ s a -> s{_dsdLastUpdateTimestamp = a}) . mapping _Time;

-- | The name of the delivery stream.
dsdDeliveryStreamName :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamName = lens _dsdDeliveryStreamName (\ s a -> s{_dsdDeliveryStreamName = a});

-- | The Amazon Resource Name (ARN) of the delivery stream.
dsdDeliveryStreamARN :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamARN = lens _dsdDeliveryStreamARN (\ s a -> s{_dsdDeliveryStreamARN = a});

-- | The status of the delivery stream.
dsdDeliveryStreamStatus :: Lens' DeliveryStreamDescription DeliveryStreamStatus
dsdDeliveryStreamStatus = lens _dsdDeliveryStreamStatus (\ s a -> s{_dsdDeliveryStreamStatus = a});

-- | Used when calling the 'UpdateDestination' operation. Each time the destination is updated for the delivery stream, the VersionId is changed, and the current VersionId is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
dsdVersionId :: Lens' DeliveryStreamDescription Text
dsdVersionId = lens _dsdVersionId (\ s a -> s{_dsdVersionId = a});

-- | The destinations.
dsdDestinations :: Lens' DeliveryStreamDescription [DestinationDescription]
dsdDestinations = lens _dsdDestinations (\ s a -> s{_dsdDestinations = a}) . _Coerce;

-- | Indicates whether there are more destinations available to list.
dsdHasMoreDestinations :: Lens' DeliveryStreamDescription Bool
dsdHasMoreDestinations = lens _dsdHasMoreDestinations (\ s a -> s{_dsdHasMoreDestinations = a});

instance FromJSON DeliveryStreamDescription where
        parseJSON
          = withObject "DeliveryStreamDescription"
              (\ x ->
                 DeliveryStreamDescription' <$>
                   (x .:? "CreateTimestamp") <*>
                     (x .:? "LastUpdateTimestamp")
                     <*> (x .: "DeliveryStreamName")
                     <*> (x .: "DeliveryStreamARN")
                     <*> (x .: "DeliveryStreamStatus")
                     <*> (x .: "VersionId")
                     <*> (x .:? "Destinations" .!= mempty)
                     <*> (x .: "HasMoreDestinations"))

instance Hashable DeliveryStreamDescription

instance NFData DeliveryStreamDescription

-- | Describes the destination for a delivery stream.
--
--
--
-- /See:/ 'destinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
    { _ddS3DestinationDescription            :: !(Maybe S3DestinationDescription)
    , _ddElasticsearchDestinationDescription :: !(Maybe ElasticsearchDestinationDescription)
    , _ddRedshiftDestinationDescription      :: !(Maybe RedshiftDestinationDescription)
    , _ddDestinationId                       :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddS3DestinationDescription' - The Amazon S3 destination.
--
-- * 'ddElasticsearchDestinationDescription' - The destination in Amazon ES.
--
-- * 'ddRedshiftDestinationDescription' - The destination in Amazon Redshift.
--
-- * 'ddDestinationId' - The ID of the destination.
destinationDescription
    :: Text -- ^ 'ddDestinationId'
    -> DestinationDescription
destinationDescription pDestinationId_ =
    DestinationDescription'
    { _ddS3DestinationDescription = Nothing
    , _ddElasticsearchDestinationDescription = Nothing
    , _ddRedshiftDestinationDescription = Nothing
    , _ddDestinationId = pDestinationId_
    }

-- | The Amazon S3 destination.
ddS3DestinationDescription :: Lens' DestinationDescription (Maybe S3DestinationDescription)
ddS3DestinationDescription = lens _ddS3DestinationDescription (\ s a -> s{_ddS3DestinationDescription = a});

-- | The destination in Amazon ES.
ddElasticsearchDestinationDescription :: Lens' DestinationDescription (Maybe ElasticsearchDestinationDescription)
ddElasticsearchDestinationDescription = lens _ddElasticsearchDestinationDescription (\ s a -> s{_ddElasticsearchDestinationDescription = a});

-- | The destination in Amazon Redshift.
ddRedshiftDestinationDescription :: Lens' DestinationDescription (Maybe RedshiftDestinationDescription)
ddRedshiftDestinationDescription = lens _ddRedshiftDestinationDescription (\ s a -> s{_ddRedshiftDestinationDescription = a});

-- | The ID of the destination.
ddDestinationId :: Lens' DestinationDescription Text
ddDestinationId = lens _ddDestinationId (\ s a -> s{_ddDestinationId = a});

instance FromJSON DestinationDescription where
        parseJSON
          = withObject "DestinationDescription"
              (\ x ->
                 DestinationDescription' <$>
                   (x .:? "S3DestinationDescription") <*>
                     (x .:? "ElasticsearchDestinationDescription")
                     <*> (x .:? "RedshiftDestinationDescription")
                     <*> (x .: "DestinationId"))

instance Hashable DestinationDescription

instance NFData DestinationDescription

-- | Describes the buffering to perform before delivering data to the Amazon ES destination.
--
--
--
-- /See:/ 'elasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
    { _ebhSizeInMBs         :: !(Maybe Nat)
    , _ebhIntervalInSeconds :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchBufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting __SizeInMBs__ to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, set __SizeInMBs__ to be 10 MB or higher.
--
-- * 'ebhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
elasticsearchBufferingHints
    :: ElasticsearchBufferingHints
elasticsearchBufferingHints =
    ElasticsearchBufferingHints'
    { _ebhSizeInMBs = Nothing
    , _ebhIntervalInSeconds = Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting __SizeInMBs__ to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, set __SizeInMBs__ to be 10 MB or higher.
ebhSizeInMBs :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhSizeInMBs = lens _ebhSizeInMBs (\ s a -> s{_ebhSizeInMBs = a}) . mapping _Nat;

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
ebhIntervalInSeconds :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhIntervalInSeconds = lens _ebhIntervalInSeconds (\ s a -> s{_ebhIntervalInSeconds = a}) . mapping _Nat;

instance FromJSON ElasticsearchBufferingHints where
        parseJSON
          = withObject "ElasticsearchBufferingHints"
              (\ x ->
                 ElasticsearchBufferingHints' <$>
                   (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds"))

instance Hashable ElasticsearchBufferingHints

instance NFData ElasticsearchBufferingHints

instance ToJSON ElasticsearchBufferingHints where
        toJSON ElasticsearchBufferingHints'{..}
          = object
              (catMaybes
                 [("SizeInMBs" .=) <$> _ebhSizeInMBs,
                  ("IntervalInSeconds" .=) <$> _ebhIntervalInSeconds])

-- | Describes the configuration of a destination in Amazon ES.
--
--
--
-- /See:/ 'elasticsearchDestinationConfiguration' smart constructor.
data ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration'
    { _edcIndexRotationPeriod      :: !(Maybe ElasticsearchIndexRotationPeriod)
    , _edcS3BackupMode             :: !(Maybe ElasticsearchS3BackupMode)
    , _edcCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _edcBufferingHints           :: !(Maybe ElasticsearchBufferingHints)
    , _edcRetryOptions             :: !(Maybe ElasticsearchRetryOptions)
    , _edcRoleARN                  :: !Text
    , _edcDomainARN                :: !Text
    , _edcIndexName                :: !Text
    , _edcTypeName                 :: !Text
    , _edcS3Configuration          :: !S3DestinationConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to the IndexName to facilitate expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for Amazon Elasticsearch Service Destination> . Default value is @OneDay@ .
--
-- * 'edcS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to FailedDocumentsOnly, Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with elasticsearch-failed/ appended to the key prefix. When set to AllDocuments, Firehose delivers all incoming records to Amazon S3, and also writes failed documents with elasticsearch-failed/ appended to the prefix. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for Amazon Elasticsearch Service Destination> . Default value is FailedDocumentsOnly.
--
-- * 'edcCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'edcBufferingHints' - Buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
--
-- * 'edcRetryOptions' - Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon ES. Default value is 300 (5 minutes).
--
-- * 'edcRoleARN' - The ARN of the IAM role to be assumed by Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Amazon S3 Bucket Access> .
--
-- * 'edcDomainARN' - The ARN of the Amazon ES domain. The IAM role must have permission for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming __RoleARN__ .
--
-- * 'edcIndexName' - The Elasticsearch index name.
--
-- * 'edcTypeName' - The Elasticsearch type name.
--
-- * 'edcS3Configuration' - Undocumented member.
elasticsearchDestinationConfiguration
    :: Text -- ^ 'edcRoleARN'
    -> Text -- ^ 'edcDomainARN'
    -> Text -- ^ 'edcIndexName'
    -> Text -- ^ 'edcTypeName'
    -> S3DestinationConfiguration -- ^ 'edcS3Configuration'
    -> ElasticsearchDestinationConfiguration
elasticsearchDestinationConfiguration pRoleARN_ pDomainARN_ pIndexName_ pTypeName_ pS3Configuration_ =
    ElasticsearchDestinationConfiguration'
    { _edcIndexRotationPeriod = Nothing
    , _edcS3BackupMode = Nothing
    , _edcCloudWatchLoggingOptions = Nothing
    , _edcBufferingHints = Nothing
    , _edcRetryOptions = Nothing
    , _edcRoleARN = pRoleARN_
    , _edcDomainARN = pDomainARN_
    , _edcIndexName = pIndexName_
    , _edcTypeName = pTypeName_
    , _edcS3Configuration = pS3Configuration_
    }

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the IndexName to facilitate expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for Amazon Elasticsearch Service Destination> . Default value is @OneDay@ .
edcIndexRotationPeriod :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchIndexRotationPeriod)
edcIndexRotationPeriod = lens _edcIndexRotationPeriod (\ s a -> s{_edcIndexRotationPeriod = a});

-- | Defines how documents should be delivered to Amazon S3. When set to FailedDocumentsOnly, Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with elasticsearch-failed/ appended to the key prefix. When set to AllDocuments, Firehose delivers all incoming records to Amazon S3, and also writes failed documents with elasticsearch-failed/ appended to the prefix. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for Amazon Elasticsearch Service Destination> . Default value is FailedDocumentsOnly.
edcS3BackupMode :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchS3BackupMode)
edcS3BackupMode = lens _edcS3BackupMode (\ s a -> s{_edcS3BackupMode = a});

-- | Describes CloudWatch logging options for your delivery stream.
edcCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe CloudWatchLoggingOptions)
edcCloudWatchLoggingOptions = lens _edcCloudWatchLoggingOptions (\ s a -> s{_edcCloudWatchLoggingOptions = a});

-- | Buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
edcBufferingHints :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchBufferingHints)
edcBufferingHints = lens _edcBufferingHints (\ s a -> s{_edcBufferingHints = a});

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon ES. Default value is 300 (5 minutes).
edcRetryOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchRetryOptions)
edcRetryOptions = lens _edcRetryOptions (\ s a -> s{_edcRetryOptions = a});

-- | The ARN of the IAM role to be assumed by Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Amazon S3 Bucket Access> .
edcRoleARN :: Lens' ElasticsearchDestinationConfiguration Text
edcRoleARN = lens _edcRoleARN (\ s a -> s{_edcRoleARN = a});

-- | The ARN of the Amazon ES domain. The IAM role must have permission for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming __RoleARN__ .
edcDomainARN :: Lens' ElasticsearchDestinationConfiguration Text
edcDomainARN = lens _edcDomainARN (\ s a -> s{_edcDomainARN = a});

-- | The Elasticsearch index name.
edcIndexName :: Lens' ElasticsearchDestinationConfiguration Text
edcIndexName = lens _edcIndexName (\ s a -> s{_edcIndexName = a});

-- | The Elasticsearch type name.
edcTypeName :: Lens' ElasticsearchDestinationConfiguration Text
edcTypeName = lens _edcTypeName (\ s a -> s{_edcTypeName = a});

-- | Undocumented member.
edcS3Configuration :: Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
edcS3Configuration = lens _edcS3Configuration (\ s a -> s{_edcS3Configuration = a});

instance Hashable
         ElasticsearchDestinationConfiguration

instance NFData ElasticsearchDestinationConfiguration

instance ToJSON ElasticsearchDestinationConfiguration
         where
        toJSON ElasticsearchDestinationConfiguration'{..}
          = object
              (catMaybes
                 [("IndexRotationPeriod" .=) <$>
                    _edcIndexRotationPeriod,
                  ("S3BackupMode" .=) <$> _edcS3BackupMode,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _edcCloudWatchLoggingOptions,
                  ("BufferingHints" .=) <$> _edcBufferingHints,
                  ("RetryOptions" .=) <$> _edcRetryOptions,
                  Just ("RoleARN" .= _edcRoleARN),
                  Just ("DomainARN" .= _edcDomainARN),
                  Just ("IndexName" .= _edcIndexName),
                  Just ("TypeName" .= _edcTypeName),
                  Just ("S3Configuration" .= _edcS3Configuration)])

-- | The destination description in Amazon ES.
--
--
--
-- /See:/ 'elasticsearchDestinationDescription' smart constructor.
data ElasticsearchDestinationDescription = ElasticsearchDestinationDescription'
    { _eddIndexRotationPeriod      :: !(Maybe ElasticsearchIndexRotationPeriod)
    , _eddTypeName                 :: !(Maybe Text)
    , _eddS3BackupMode             :: !(Maybe ElasticsearchS3BackupMode)
    , _eddDomainARN                :: !(Maybe Text)
    , _eddCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _eddS3DestinationDescription :: !(Maybe S3DestinationDescription)
    , _eddBufferingHints           :: !(Maybe ElasticsearchBufferingHints)
    , _eddRetryOptions             :: !(Maybe ElasticsearchRetryOptions)
    , _eddRoleARN                  :: !(Maybe Text)
    , _eddIndexName                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eddIndexRotationPeriod' - The Elasticsearch index rotation period
--
-- * 'eddTypeName' - The Elasticsearch type name.
--
-- * 'eddS3BackupMode' - Amazon S3 backup mode.
--
-- * 'eddDomainARN' - The ARN of the Amazon ES domain.
--
-- * 'eddCloudWatchLoggingOptions' - CloudWatch logging options.
--
-- * 'eddS3DestinationDescription' - Undocumented member.
--
-- * 'eddBufferingHints' - Buffering options.
--
-- * 'eddRetryOptions' - Elasticsearch retry options.
--
-- * 'eddRoleARN' - The ARN of the AWS credentials.
--
-- * 'eddIndexName' - The Elasticsearch index name.
elasticsearchDestinationDescription
    :: ElasticsearchDestinationDescription
elasticsearchDestinationDescription =
    ElasticsearchDestinationDescription'
    { _eddIndexRotationPeriod = Nothing
    , _eddTypeName = Nothing
    , _eddS3BackupMode = Nothing
    , _eddDomainARN = Nothing
    , _eddCloudWatchLoggingOptions = Nothing
    , _eddS3DestinationDescription = Nothing
    , _eddBufferingHints = Nothing
    , _eddRetryOptions = Nothing
    , _eddRoleARN = Nothing
    , _eddIndexName = Nothing
    }

-- | The Elasticsearch index rotation period
eddIndexRotationPeriod :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchIndexRotationPeriod)
eddIndexRotationPeriod = lens _eddIndexRotationPeriod (\ s a -> s{_eddIndexRotationPeriod = a});

-- | The Elasticsearch type name.
eddTypeName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddTypeName = lens _eddTypeName (\ s a -> s{_eddTypeName = a});

-- | Amazon S3 backup mode.
eddS3BackupMode :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchS3BackupMode)
eddS3BackupMode = lens _eddS3BackupMode (\ s a -> s{_eddS3BackupMode = a});

-- | The ARN of the Amazon ES domain.
eddDomainARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddDomainARN = lens _eddDomainARN (\ s a -> s{_eddDomainARN = a});

-- | CloudWatch logging options.
eddCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationDescription (Maybe CloudWatchLoggingOptions)
eddCloudWatchLoggingOptions = lens _eddCloudWatchLoggingOptions (\ s a -> s{_eddCloudWatchLoggingOptions = a});

-- | Undocumented member.
eddS3DestinationDescription :: Lens' ElasticsearchDestinationDescription (Maybe S3DestinationDescription)
eddS3DestinationDescription = lens _eddS3DestinationDescription (\ s a -> s{_eddS3DestinationDescription = a});

-- | Buffering options.
eddBufferingHints :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchBufferingHints)
eddBufferingHints = lens _eddBufferingHints (\ s a -> s{_eddBufferingHints = a});

-- | Elasticsearch retry options.
eddRetryOptions :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchRetryOptions)
eddRetryOptions = lens _eddRetryOptions (\ s a -> s{_eddRetryOptions = a});

-- | The ARN of the AWS credentials.
eddRoleARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddRoleARN = lens _eddRoleARN (\ s a -> s{_eddRoleARN = a});

-- | The Elasticsearch index name.
eddIndexName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddIndexName = lens _eddIndexName (\ s a -> s{_eddIndexName = a});

instance FromJSON ElasticsearchDestinationDescription
         where
        parseJSON
          = withObject "ElasticsearchDestinationDescription"
              (\ x ->
                 ElasticsearchDestinationDescription' <$>
                   (x .:? "IndexRotationPeriod") <*> (x .:? "TypeName")
                     <*> (x .:? "S3BackupMode")
                     <*> (x .:? "DomainARN")
                     <*> (x .:? "CloudWatchLoggingOptions")
                     <*> (x .:? "S3DestinationDescription")
                     <*> (x .:? "BufferingHints")
                     <*> (x .:? "RetryOptions")
                     <*> (x .:? "RoleARN")
                     <*> (x .:? "IndexName"))

instance Hashable ElasticsearchDestinationDescription

instance NFData ElasticsearchDestinationDescription

-- | Describes an update for a destination in Amazon ES.
--
--
--
-- /See:/ 'elasticsearchDestinationUpdate' smart constructor.
data ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate'
    { _eduIndexRotationPeriod      :: !(Maybe ElasticsearchIndexRotationPeriod)
    , _eduTypeName                 :: !(Maybe Text)
    , _eduDomainARN                :: !(Maybe Text)
    , _eduCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _eduS3Update                 :: !(Maybe S3DestinationUpdate)
    , _eduBufferingHints           :: !(Maybe ElasticsearchBufferingHints)
    , _eduRetryOptions             :: !(Maybe ElasticsearchRetryOptions)
    , _eduRoleARN                  :: !(Maybe Text)
    , _eduIndexName                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eduIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to the IndexName to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for Amazon Elasticsearch Service Destination> . Default value is @OneDay@ .
--
-- * 'eduTypeName' - The Elasticsearch type name.
--
-- * 'eduDomainARN' - The ARN of the Amazon ES domain. The IAM role must have permission for DescribeElasticsearchDomain, DescribeElasticsearchDomains , and DescribeElasticsearchDomainConfig after assuming __RoleARN__ .
--
-- * 'eduCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'eduS3Update' - Undocumented member.
--
-- * 'eduBufferingHints' - Buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
--
-- * 'eduRetryOptions' - Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon ES. Default value is 300 (5 minutes).
--
-- * 'eduRoleARN' - The ARN of the IAM role to be assumed by Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Amazon S3 Bucket Access> .
--
-- * 'eduIndexName' - The Elasticsearch index name.
elasticsearchDestinationUpdate
    :: ElasticsearchDestinationUpdate
elasticsearchDestinationUpdate =
    ElasticsearchDestinationUpdate'
    { _eduIndexRotationPeriod = Nothing
    , _eduTypeName = Nothing
    , _eduDomainARN = Nothing
    , _eduCloudWatchLoggingOptions = Nothing
    , _eduS3Update = Nothing
    , _eduBufferingHints = Nothing
    , _eduRetryOptions = Nothing
    , _eduRoleARN = Nothing
    , _eduIndexName = Nothing
    }

-- | The Elasticsearch index rotation period. Index rotation appends a timestamp to the IndexName to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for Amazon Elasticsearch Service Destination> . Default value is @OneDay@ .
eduIndexRotationPeriod :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchIndexRotationPeriod)
eduIndexRotationPeriod = lens _eduIndexRotationPeriod (\ s a -> s{_eduIndexRotationPeriod = a});

-- | The Elasticsearch type name.
eduTypeName :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduTypeName = lens _eduTypeName (\ s a -> s{_eduTypeName = a});

-- | The ARN of the Amazon ES domain. The IAM role must have permission for DescribeElasticsearchDomain, DescribeElasticsearchDomains , and DescribeElasticsearchDomainConfig after assuming __RoleARN__ .
eduDomainARN :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduDomainARN = lens _eduDomainARN (\ s a -> s{_eduDomainARN = a});

-- | Describes CloudWatch logging options for your delivery stream.
eduCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationUpdate (Maybe CloudWatchLoggingOptions)
eduCloudWatchLoggingOptions = lens _eduCloudWatchLoggingOptions (\ s a -> s{_eduCloudWatchLoggingOptions = a});

-- | Undocumented member.
eduS3Update :: Lens' ElasticsearchDestinationUpdate (Maybe S3DestinationUpdate)
eduS3Update = lens _eduS3Update (\ s a -> s{_eduS3Update = a});

-- | Buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
eduBufferingHints :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchBufferingHints)
eduBufferingHints = lens _eduBufferingHints (\ s a -> s{_eduBufferingHints = a});

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon ES. Default value is 300 (5 minutes).
eduRetryOptions :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchRetryOptions)
eduRetryOptions = lens _eduRetryOptions (\ s a -> s{_eduRetryOptions = a});

-- | The ARN of the IAM role to be assumed by Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Amazon S3 Bucket Access> .
eduRoleARN :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduRoleARN = lens _eduRoleARN (\ s a -> s{_eduRoleARN = a});

-- | The Elasticsearch index name.
eduIndexName :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduIndexName = lens _eduIndexName (\ s a -> s{_eduIndexName = a});

instance Hashable ElasticsearchDestinationUpdate

instance NFData ElasticsearchDestinationUpdate

instance ToJSON ElasticsearchDestinationUpdate where
        toJSON ElasticsearchDestinationUpdate'{..}
          = object
              (catMaybes
                 [("IndexRotationPeriod" .=) <$>
                    _eduIndexRotationPeriod,
                  ("TypeName" .=) <$> _eduTypeName,
                  ("DomainARN" .=) <$> _eduDomainARN,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _eduCloudWatchLoggingOptions,
                  ("S3Update" .=) <$> _eduS3Update,
                  ("BufferingHints" .=) <$> _eduBufferingHints,
                  ("RetryOptions" .=) <$> _eduRetryOptions,
                  ("RoleARN" .=) <$> _eduRoleARN,
                  ("IndexName" .=) <$> _eduIndexName])

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon ES.
--
--
--
-- /See:/ 'elasticsearchRetryOptions' smart constructor.
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions'
    { _eroDurationInSeconds :: Maybe Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eroDurationInSeconds' - After an initial failure to deliver to Amazon ES, the total amount of time during which Firehose re-attempts delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
elasticsearchRetryOptions
    :: ElasticsearchRetryOptions
elasticsearchRetryOptions =
    ElasticsearchRetryOptions'
    { _eroDurationInSeconds = Nothing
    }

-- | After an initial failure to deliver to Amazon ES, the total amount of time during which Firehose re-attempts delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
eroDurationInSeconds :: Lens' ElasticsearchRetryOptions (Maybe Natural)
eroDurationInSeconds = lens _eroDurationInSeconds (\ s a -> s{_eroDurationInSeconds = a}) . mapping _Nat;

instance FromJSON ElasticsearchRetryOptions where
        parseJSON
          = withObject "ElasticsearchRetryOptions"
              (\ x ->
                 ElasticsearchRetryOptions' <$>
                   (x .:? "DurationInSeconds"))

instance Hashable ElasticsearchRetryOptions

instance NFData ElasticsearchRetryOptions

instance ToJSON ElasticsearchRetryOptions where
        toJSON ElasticsearchRetryOptions'{..}
          = object
              (catMaybes
                 [("DurationInSeconds" .=) <$> _eroDurationInSeconds])

-- | Describes the encryption for a destination in Amazon S3.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
    { _ecNoEncryptionConfig  :: !(Maybe NoEncryptionConfig)
    , _ecKMSEncryptionConfig :: !(Maybe KMSEncryptionConfig)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecNoEncryptionConfig' - Specifically override existing encryption information to ensure no encryption is used.
--
-- * 'ecKMSEncryptionConfig' - The encryption key.
encryptionConfiguration
    :: EncryptionConfiguration
encryptionConfiguration =
    EncryptionConfiguration'
    { _ecNoEncryptionConfig = Nothing
    , _ecKMSEncryptionConfig = Nothing
    }

-- | Specifically override existing encryption information to ensure no encryption is used.
ecNoEncryptionConfig :: Lens' EncryptionConfiguration (Maybe NoEncryptionConfig)
ecNoEncryptionConfig = lens _ecNoEncryptionConfig (\ s a -> s{_ecNoEncryptionConfig = a});

-- | The encryption key.
ecKMSEncryptionConfig :: Lens' EncryptionConfiguration (Maybe KMSEncryptionConfig)
ecKMSEncryptionConfig = lens _ecKMSEncryptionConfig (\ s a -> s{_ecKMSEncryptionConfig = a});

instance FromJSON EncryptionConfiguration where
        parseJSON
          = withObject "EncryptionConfiguration"
              (\ x ->
                 EncryptionConfiguration' <$>
                   (x .:? "NoEncryptionConfig") <*>
                     (x .:? "KMSEncryptionConfig"))

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration'{..}
          = object
              (catMaybes
                 [("NoEncryptionConfig" .=) <$> _ecNoEncryptionConfig,
                  ("KMSEncryptionConfig" .=) <$>
                    _ecKMSEncryptionConfig])

-- | Describes an encryption key for a destination in Amazon S3.
--
--
--
-- /See:/ 'kmsEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
    { _kecAWSKMSKeyARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KMSEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kecAWSKMSKeyARN' - The ARN of the encryption key. Must belong to the same region as the destination Amazon S3 bucket.
kmsEncryptionConfig
    :: Text -- ^ 'kecAWSKMSKeyARN'
    -> KMSEncryptionConfig
kmsEncryptionConfig pAWSKMSKeyARN_ =
    KMSEncryptionConfig'
    { _kecAWSKMSKeyARN = pAWSKMSKeyARN_
    }

-- | The ARN of the encryption key. Must belong to the same region as the destination Amazon S3 bucket.
kecAWSKMSKeyARN :: Lens' KMSEncryptionConfig Text
kecAWSKMSKeyARN = lens _kecAWSKMSKeyARN (\ s a -> s{_kecAWSKMSKeyARN = a});

instance FromJSON KMSEncryptionConfig where
        parseJSON
          = withObject "KMSEncryptionConfig"
              (\ x ->
                 KMSEncryptionConfig' <$> (x .: "AWSKMSKeyARN"))

instance Hashable KMSEncryptionConfig

instance NFData KMSEncryptionConfig

instance ToJSON KMSEncryptionConfig where
        toJSON KMSEncryptionConfig'{..}
          = object
              (catMaybes
                 [Just ("AWSKMSKeyARN" .= _kecAWSKMSKeyARN)])

-- | Contains the result for an individual record from a 'PutRecordBatch' request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.
--
--
--
-- /See:/ 'putRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
    { _prbreRecordId     :: !(Maybe Text)
    , _prbreErrorCode    :: !(Maybe Text)
    , _prbreErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbreRecordId' - The ID of the record.
--
-- * 'prbreErrorCode' - The error code for an individual record result.
--
-- * 'prbreErrorMessage' - The error message for an individual record result.
putRecordBatchResponseEntry
    :: PutRecordBatchResponseEntry
putRecordBatchResponseEntry =
    PutRecordBatchResponseEntry'
    { _prbreRecordId = Nothing
    , _prbreErrorCode = Nothing
    , _prbreErrorMessage = Nothing
    }

-- | The ID of the record.
prbreRecordId :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreRecordId = lens _prbreRecordId (\ s a -> s{_prbreRecordId = a});

-- | The error code for an individual record result.
prbreErrorCode :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorCode = lens _prbreErrorCode (\ s a -> s{_prbreErrorCode = a});

-- | The error message for an individual record result.
prbreErrorMessage :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorMessage = lens _prbreErrorMessage (\ s a -> s{_prbreErrorMessage = a});

instance FromJSON PutRecordBatchResponseEntry where
        parseJSON
          = withObject "PutRecordBatchResponseEntry"
              (\ x ->
                 PutRecordBatchResponseEntry' <$>
                   (x .:? "RecordId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable PutRecordBatchResponseEntry

instance NFData PutRecordBatchResponseEntry

-- | The unit of data in a delivery stream.
--
--
--
-- /See:/ 'record' smart constructor.
newtype Record = Record'
    { _rData :: Base64
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rData' - The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KB.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
record
    :: ByteString -- ^ 'rData'
    -> Record
record pData_ =
    Record'
    { _rData = _Base64 # pData_
    }

-- | The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KB.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rData :: Lens' Record ByteString
rData = lens _rData (\ s a -> s{_rData = a}) . _Base64;

instance Hashable Record

instance NFData Record

instance ToJSON Record where
        toJSON Record'{..}
          = object (catMaybes [Just ("Data" .= _rData)])

-- | Describes the configuration of a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
    { _rdcCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _rdcRetryOptions             :: !(Maybe RedshiftRetryOptions)
    , _rdcRoleARN                  :: !Text
    , _rdcClusterJDBCURL           :: !Text
    , _rdcCopyCommand              :: !CopyCommand
    , _rdcUsername                 :: !(Sensitive Text)
    , _rdcPassword                 :: !(Sensitive Text)
    , _rdcS3Configuration          :: !S3DestinationConfiguration
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'rdcRetryOptions' - Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rdcRoleARN' - The ARN of the AWS credentials.
--
-- * 'rdcClusterJDBCURL' - The database connection string.
--
-- * 'rdcCopyCommand' - The @COPY@ command.
--
-- * 'rdcUsername' - The name of the user.
--
-- * 'rdcPassword' - The user password.
--
-- * 'rdcS3Configuration' - The S3 configuration for the intermediate location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in __RedshiftDestinationConfiguration.S3Configuration__ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
redshiftDestinationConfiguration
    :: Text -- ^ 'rdcRoleARN'
    -> Text -- ^ 'rdcClusterJDBCURL'
    -> CopyCommand -- ^ 'rdcCopyCommand'
    -> Text -- ^ 'rdcUsername'
    -> Text -- ^ 'rdcPassword'
    -> S3DestinationConfiguration -- ^ 'rdcS3Configuration'
    -> RedshiftDestinationConfiguration
redshiftDestinationConfiguration pRoleARN_ pClusterJDBCURL_ pCopyCommand_ pUsername_ pPassword_ pS3Configuration_ =
    RedshiftDestinationConfiguration'
    { _rdcCloudWatchLoggingOptions = Nothing
    , _rdcRetryOptions = Nothing
    , _rdcRoleARN = pRoleARN_
    , _rdcClusterJDBCURL = pClusterJDBCURL_
    , _rdcCopyCommand = pCopyCommand_
    , _rdcUsername = _Sensitive # pUsername_
    , _rdcPassword = _Sensitive # pPassword_
    , _rdcS3Configuration = pS3Configuration_
    }

-- | Describes CloudWatch logging options for your delivery stream.
rdcCloudWatchLoggingOptions :: Lens' RedshiftDestinationConfiguration (Maybe CloudWatchLoggingOptions)
rdcCloudWatchLoggingOptions = lens _rdcCloudWatchLoggingOptions (\ s a -> s{_rdcCloudWatchLoggingOptions = a});

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rdcRetryOptions :: Lens' RedshiftDestinationConfiguration (Maybe RedshiftRetryOptions)
rdcRetryOptions = lens _rdcRetryOptions (\ s a -> s{_rdcRetryOptions = a});

-- | The ARN of the AWS credentials.
rdcRoleARN :: Lens' RedshiftDestinationConfiguration Text
rdcRoleARN = lens _rdcRoleARN (\ s a -> s{_rdcRoleARN = a});

-- | The database connection string.
rdcClusterJDBCURL :: Lens' RedshiftDestinationConfiguration Text
rdcClusterJDBCURL = lens _rdcClusterJDBCURL (\ s a -> s{_rdcClusterJDBCURL = a});

-- | The @COPY@ command.
rdcCopyCommand :: Lens' RedshiftDestinationConfiguration CopyCommand
rdcCopyCommand = lens _rdcCopyCommand (\ s a -> s{_rdcCopyCommand = a});

-- | The name of the user.
rdcUsername :: Lens' RedshiftDestinationConfiguration Text
rdcUsername = lens _rdcUsername (\ s a -> s{_rdcUsername = a}) . _Sensitive;

-- | The user password.
rdcPassword :: Lens' RedshiftDestinationConfiguration Text
rdcPassword = lens _rdcPassword (\ s a -> s{_rdcPassword = a}) . _Sensitive;

-- | The S3 configuration for the intermediate location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in __RedshiftDestinationConfiguration.S3Configuration__ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
rdcS3Configuration :: Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
rdcS3Configuration = lens _rdcS3Configuration (\ s a -> s{_rdcS3Configuration = a});

instance Hashable RedshiftDestinationConfiguration

instance NFData RedshiftDestinationConfiguration

instance ToJSON RedshiftDestinationConfiguration
         where
        toJSON RedshiftDestinationConfiguration'{..}
          = object
              (catMaybes
                 [("CloudWatchLoggingOptions" .=) <$>
                    _rdcCloudWatchLoggingOptions,
                  ("RetryOptions" .=) <$> _rdcRetryOptions,
                  Just ("RoleARN" .= _rdcRoleARN),
                  Just ("ClusterJDBCURL" .= _rdcClusterJDBCURL),
                  Just ("CopyCommand" .= _rdcCopyCommand),
                  Just ("Username" .= _rdcUsername),
                  Just ("Password" .= _rdcPassword),
                  Just ("S3Configuration" .= _rdcS3Configuration)])

-- | Describes a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationDescription' smart constructor.
data RedshiftDestinationDescription = RedshiftDestinationDescription'
    { _rddCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _rddRetryOptions             :: !(Maybe RedshiftRetryOptions)
    , _rddRoleARN                  :: !Text
    , _rddClusterJDBCURL           :: !Text
    , _rddCopyCommand              :: !CopyCommand
    , _rddUsername                 :: !(Sensitive Text)
    , _rddS3DestinationDescription :: !S3DestinationDescription
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rddCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'rddRetryOptions' - Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rddRoleARN' - The ARN of the AWS credentials.
--
-- * 'rddClusterJDBCURL' - The database connection string.
--
-- * 'rddCopyCommand' - The @COPY@ command.
--
-- * 'rddUsername' - The name of the user.
--
-- * 'rddS3DestinationDescription' - The Amazon S3 destination.
redshiftDestinationDescription
    :: Text -- ^ 'rddRoleARN'
    -> Text -- ^ 'rddClusterJDBCURL'
    -> CopyCommand -- ^ 'rddCopyCommand'
    -> Text -- ^ 'rddUsername'
    -> S3DestinationDescription -- ^ 'rddS3DestinationDescription'
    -> RedshiftDestinationDescription
redshiftDestinationDescription pRoleARN_ pClusterJDBCURL_ pCopyCommand_ pUsername_ pS3DestinationDescription_ =
    RedshiftDestinationDescription'
    { _rddCloudWatchLoggingOptions = Nothing
    , _rddRetryOptions = Nothing
    , _rddRoleARN = pRoleARN_
    , _rddClusterJDBCURL = pClusterJDBCURL_
    , _rddCopyCommand = pCopyCommand_
    , _rddUsername = _Sensitive # pUsername_
    , _rddS3DestinationDescription = pS3DestinationDescription_
    }

-- | Describes CloudWatch logging options for your delivery stream.
rddCloudWatchLoggingOptions :: Lens' RedshiftDestinationDescription (Maybe CloudWatchLoggingOptions)
rddCloudWatchLoggingOptions = lens _rddCloudWatchLoggingOptions (\ s a -> s{_rddCloudWatchLoggingOptions = a});

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rddRetryOptions :: Lens' RedshiftDestinationDescription (Maybe RedshiftRetryOptions)
rddRetryOptions = lens _rddRetryOptions (\ s a -> s{_rddRetryOptions = a});

-- | The ARN of the AWS credentials.
rddRoleARN :: Lens' RedshiftDestinationDescription Text
rddRoleARN = lens _rddRoleARN (\ s a -> s{_rddRoleARN = a});

-- | The database connection string.
rddClusterJDBCURL :: Lens' RedshiftDestinationDescription Text
rddClusterJDBCURL = lens _rddClusterJDBCURL (\ s a -> s{_rddClusterJDBCURL = a});

-- | The @COPY@ command.
rddCopyCommand :: Lens' RedshiftDestinationDescription CopyCommand
rddCopyCommand = lens _rddCopyCommand (\ s a -> s{_rddCopyCommand = a});

-- | The name of the user.
rddUsername :: Lens' RedshiftDestinationDescription Text
rddUsername = lens _rddUsername (\ s a -> s{_rddUsername = a}) . _Sensitive;

-- | The Amazon S3 destination.
rddS3DestinationDescription :: Lens' RedshiftDestinationDescription S3DestinationDescription
rddS3DestinationDescription = lens _rddS3DestinationDescription (\ s a -> s{_rddS3DestinationDescription = a});

instance FromJSON RedshiftDestinationDescription
         where
        parseJSON
          = withObject "RedshiftDestinationDescription"
              (\ x ->
                 RedshiftDestinationDescription' <$>
                   (x .:? "CloudWatchLoggingOptions") <*>
                     (x .:? "RetryOptions")
                     <*> (x .: "RoleARN")
                     <*> (x .: "ClusterJDBCURL")
                     <*> (x .: "CopyCommand")
                     <*> (x .: "Username")
                     <*> (x .: "S3DestinationDescription"))

instance Hashable RedshiftDestinationDescription

instance NFData RedshiftDestinationDescription

-- | Describes an update for a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
    { _rduCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _rduUsername                 :: !(Maybe (Sensitive Text))
    , _rduS3Update                 :: !(Maybe S3DestinationUpdate)
    , _rduPassword                 :: !(Maybe (Sensitive Text))
    , _rduCopyCommand              :: !(Maybe CopyCommand)
    , _rduRetryOptions             :: !(Maybe RedshiftRetryOptions)
    , _rduClusterJDBCURL           :: !(Maybe Text)
    , _rduRoleARN                  :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rduCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'rduUsername' - The name of the user.
--
-- * 'rduS3Update' - The Amazon S3 destination. The compression formats @SNAPPY@ or @ZIP@ cannot be specified in __RedshiftDestinationUpdate.S3Update__ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- * 'rduPassword' - The user password.
--
-- * 'rduCopyCommand' - The @COPY@ command.
--
-- * 'rduRetryOptions' - Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rduClusterJDBCURL' - The database connection string.
--
-- * 'rduRoleARN' - The ARN of the AWS credentials.
redshiftDestinationUpdate
    :: RedshiftDestinationUpdate
redshiftDestinationUpdate =
    RedshiftDestinationUpdate'
    { _rduCloudWatchLoggingOptions = Nothing
    , _rduUsername = Nothing
    , _rduS3Update = Nothing
    , _rduPassword = Nothing
    , _rduCopyCommand = Nothing
    , _rduRetryOptions = Nothing
    , _rduClusterJDBCURL = Nothing
    , _rduRoleARN = Nothing
    }

-- | Describes CloudWatch logging options for your delivery stream.
rduCloudWatchLoggingOptions :: Lens' RedshiftDestinationUpdate (Maybe CloudWatchLoggingOptions)
rduCloudWatchLoggingOptions = lens _rduCloudWatchLoggingOptions (\ s a -> s{_rduCloudWatchLoggingOptions = a});

-- | The name of the user.
rduUsername :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduUsername = lens _rduUsername (\ s a -> s{_rduUsername = a}) . mapping _Sensitive;

-- | The Amazon S3 destination. The compression formats @SNAPPY@ or @ZIP@ cannot be specified in __RedshiftDestinationUpdate.S3Update__ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
rduS3Update :: Lens' RedshiftDestinationUpdate (Maybe S3DestinationUpdate)
rduS3Update = lens _rduS3Update (\ s a -> s{_rduS3Update = a});

-- | The user password.
rduPassword :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduPassword = lens _rduPassword (\ s a -> s{_rduPassword = a}) . mapping _Sensitive;

-- | The @COPY@ command.
rduCopyCommand :: Lens' RedshiftDestinationUpdate (Maybe CopyCommand)
rduCopyCommand = lens _rduCopyCommand (\ s a -> s{_rduCopyCommand = a});

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rduRetryOptions :: Lens' RedshiftDestinationUpdate (Maybe RedshiftRetryOptions)
rduRetryOptions = lens _rduRetryOptions (\ s a -> s{_rduRetryOptions = a});

-- | The database connection string.
rduClusterJDBCURL :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduClusterJDBCURL = lens _rduClusterJDBCURL (\ s a -> s{_rduClusterJDBCURL = a});

-- | The ARN of the AWS credentials.
rduRoleARN :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduRoleARN = lens _rduRoleARN (\ s a -> s{_rduRoleARN = a});

instance Hashable RedshiftDestinationUpdate

instance NFData RedshiftDestinationUpdate

instance ToJSON RedshiftDestinationUpdate where
        toJSON RedshiftDestinationUpdate'{..}
          = object
              (catMaybes
                 [("CloudWatchLoggingOptions" .=) <$>
                    _rduCloudWatchLoggingOptions,
                  ("Username" .=) <$> _rduUsername,
                  ("S3Update" .=) <$> _rduS3Update,
                  ("Password" .=) <$> _rduPassword,
                  ("CopyCommand" .=) <$> _rduCopyCommand,
                  ("RetryOptions" .=) <$> _rduRetryOptions,
                  ("ClusterJDBCURL" .=) <$> _rduClusterJDBCURL,
                  ("RoleARN" .=) <$> _rduRoleARN])

-- | Configures retry behavior in the event that Firehose is unable to deliver documents to Amazon Redshift.
--
--
--
-- /See:/ 'redshiftRetryOptions' smart constructor.
newtype RedshiftRetryOptions = RedshiftRetryOptions'
    { _rroDurationInSeconds :: Maybe Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedshiftRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rroDurationInSeconds' - The length of time during which Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
redshiftRetryOptions
    :: RedshiftRetryOptions
redshiftRetryOptions =
    RedshiftRetryOptions'
    { _rroDurationInSeconds = Nothing
    }

-- | The length of time during which Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
rroDurationInSeconds :: Lens' RedshiftRetryOptions (Maybe Natural)
rroDurationInSeconds = lens _rroDurationInSeconds (\ s a -> s{_rroDurationInSeconds = a}) . mapping _Nat;

instance FromJSON RedshiftRetryOptions where
        parseJSON
          = withObject "RedshiftRetryOptions"
              (\ x ->
                 RedshiftRetryOptions' <$>
                   (x .:? "DurationInSeconds"))

instance Hashable RedshiftRetryOptions

instance NFData RedshiftRetryOptions

instance ToJSON RedshiftRetryOptions where
        toJSON RedshiftRetryOptions'{..}
          = object
              (catMaybes
                 [("DurationInSeconds" .=) <$> _rroDurationInSeconds])

-- | Describes the configuration of a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
    { _sdcPrefix                   :: !(Maybe Text)
    , _sdcCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _sdcEncryptionConfiguration  :: !(Maybe EncryptionConfiguration)
    , _sdcCompressionFormat        :: !(Maybe CompressionFormat)
    , _sdcBufferingHints           :: !(Maybe BufferingHints)
    , _sdcRoleARN                  :: !Text
    , _sdcBucketARN                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
--
-- * 'sdcCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'sdcEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sdcCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sdcBufferingHints' - The buffering option. If no value is specified, __BufferingHints__ object default values are used.
--
-- * 'sdcRoleARN' - The ARN of the AWS credentials.
--
-- * 'sdcBucketARN' - The ARN of the S3 bucket.
s3DestinationConfiguration
    :: Text -- ^ 'sdcRoleARN'
    -> Text -- ^ 'sdcBucketARN'
    -> S3DestinationConfiguration
s3DestinationConfiguration pRoleARN_ pBucketARN_ =
    S3DestinationConfiguration'
    { _sdcPrefix = Nothing
    , _sdcCloudWatchLoggingOptions = Nothing
    , _sdcEncryptionConfiguration = Nothing
    , _sdcCompressionFormat = Nothing
    , _sdcBufferingHints = Nothing
    , _sdcRoleARN = pRoleARN_
    , _sdcBucketARN = pBucketARN_
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
sdcPrefix :: Lens' S3DestinationConfiguration (Maybe Text)
sdcPrefix = lens _sdcPrefix (\ s a -> s{_sdcPrefix = a});

-- | Describes CloudWatch logging options for your delivery stream.
sdcCloudWatchLoggingOptions :: Lens' S3DestinationConfiguration (Maybe CloudWatchLoggingOptions)
sdcCloudWatchLoggingOptions = lens _sdcCloudWatchLoggingOptions (\ s a -> s{_sdcCloudWatchLoggingOptions = a});

-- | The encryption configuration. If no value is specified, the default is no encryption.
sdcEncryptionConfiguration :: Lens' S3DestinationConfiguration (Maybe EncryptionConfiguration)
sdcEncryptionConfiguration = lens _sdcEncryptionConfiguration (\ s a -> s{_sdcEncryptionConfiguration = a});

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sdcCompressionFormat :: Lens' S3DestinationConfiguration (Maybe CompressionFormat)
sdcCompressionFormat = lens _sdcCompressionFormat (\ s a -> s{_sdcCompressionFormat = a});

-- | The buffering option. If no value is specified, __BufferingHints__ object default values are used.
sdcBufferingHints :: Lens' S3DestinationConfiguration (Maybe BufferingHints)
sdcBufferingHints = lens _sdcBufferingHints (\ s a -> s{_sdcBufferingHints = a});

-- | The ARN of the AWS credentials.
sdcRoleARN :: Lens' S3DestinationConfiguration Text
sdcRoleARN = lens _sdcRoleARN (\ s a -> s{_sdcRoleARN = a});

-- | The ARN of the S3 bucket.
sdcBucketARN :: Lens' S3DestinationConfiguration Text
sdcBucketARN = lens _sdcBucketARN (\ s a -> s{_sdcBucketARN = a});

instance Hashable S3DestinationConfiguration

instance NFData S3DestinationConfiguration

instance ToJSON S3DestinationConfiguration where
        toJSON S3DestinationConfiguration'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _sdcPrefix,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _sdcCloudWatchLoggingOptions,
                  ("EncryptionConfiguration" .=) <$>
                    _sdcEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _sdcCompressionFormat,
                  ("BufferingHints" .=) <$> _sdcBufferingHints,
                  Just ("RoleARN" .= _sdcRoleARN),
                  Just ("BucketARN" .= _sdcBucketARN)])

-- | Describes a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationDescription' smart constructor.
data S3DestinationDescription = S3DestinationDescription'
    { _sddPrefix                   :: !(Maybe Text)
    , _sddCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _sddRoleARN                  :: !Text
    , _sddBucketARN                :: !Text
    , _sddBufferingHints           :: !BufferingHints
    , _sddCompressionFormat        :: !CompressionFormat
    , _sddEncryptionConfiguration  :: !EncryptionConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sddPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
--
-- * 'sddCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'sddRoleARN' - The ARN of the AWS credentials.
--
-- * 'sddBucketARN' - The ARN of the S3 bucket.
--
-- * 'sddBufferingHints' - The buffering option. If no value is specified, __BufferingHints__ object default values are used.
--
-- * 'sddCompressionFormat' - The compression format. If no value is specified, the default is @NOCOMPRESSION@ .
--
-- * 'sddEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
s3DestinationDescription
    :: Text -- ^ 'sddRoleARN'
    -> Text -- ^ 'sddBucketARN'
    -> BufferingHints -- ^ 'sddBufferingHints'
    -> CompressionFormat -- ^ 'sddCompressionFormat'
    -> EncryptionConfiguration -- ^ 'sddEncryptionConfiguration'
    -> S3DestinationDescription
s3DestinationDescription pRoleARN_ pBucketARN_ pBufferingHints_ pCompressionFormat_ pEncryptionConfiguration_ =
    S3DestinationDescription'
    { _sddPrefix = Nothing
    , _sddCloudWatchLoggingOptions = Nothing
    , _sddRoleARN = pRoleARN_
    , _sddBucketARN = pBucketARN_
    , _sddBufferingHints = pBufferingHints_
    , _sddCompressionFormat = pCompressionFormat_
    , _sddEncryptionConfiguration = pEncryptionConfiguration_
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
sddPrefix :: Lens' S3DestinationDescription (Maybe Text)
sddPrefix = lens _sddPrefix (\ s a -> s{_sddPrefix = a});

-- | Describes CloudWatch logging options for your delivery stream.
sddCloudWatchLoggingOptions :: Lens' S3DestinationDescription (Maybe CloudWatchLoggingOptions)
sddCloudWatchLoggingOptions = lens _sddCloudWatchLoggingOptions (\ s a -> s{_sddCloudWatchLoggingOptions = a});

-- | The ARN of the AWS credentials.
sddRoleARN :: Lens' S3DestinationDescription Text
sddRoleARN = lens _sddRoleARN (\ s a -> s{_sddRoleARN = a});

-- | The ARN of the S3 bucket.
sddBucketARN :: Lens' S3DestinationDescription Text
sddBucketARN = lens _sddBucketARN (\ s a -> s{_sddBucketARN = a});

-- | The buffering option. If no value is specified, __BufferingHints__ object default values are used.
sddBufferingHints :: Lens' S3DestinationDescription BufferingHints
sddBufferingHints = lens _sddBufferingHints (\ s a -> s{_sddBufferingHints = a});

-- | The compression format. If no value is specified, the default is @NOCOMPRESSION@ .
sddCompressionFormat :: Lens' S3DestinationDescription CompressionFormat
sddCompressionFormat = lens _sddCompressionFormat (\ s a -> s{_sddCompressionFormat = a});

-- | The encryption configuration. If no value is specified, the default is no encryption.
sddEncryptionConfiguration :: Lens' S3DestinationDescription EncryptionConfiguration
sddEncryptionConfiguration = lens _sddEncryptionConfiguration (\ s a -> s{_sddEncryptionConfiguration = a});

instance FromJSON S3DestinationDescription where
        parseJSON
          = withObject "S3DestinationDescription"
              (\ x ->
                 S3DestinationDescription' <$>
                   (x .:? "Prefix") <*>
                     (x .:? "CloudWatchLoggingOptions")
                     <*> (x .: "RoleARN")
                     <*> (x .: "BucketARN")
                     <*> (x .: "BufferingHints")
                     <*> (x .: "CompressionFormat")
                     <*> (x .: "EncryptionConfiguration"))

instance Hashable S3DestinationDescription

instance NFData S3DestinationDescription

-- | Describes an update for a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
    { _sduPrefix                   :: !(Maybe Text)
    , _sduCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
    , _sduEncryptionConfiguration  :: !(Maybe EncryptionConfiguration)
    , _sduCompressionFormat        :: !(Maybe CompressionFormat)
    , _sduBufferingHints           :: !(Maybe BufferingHints)
    , _sduBucketARN                :: !(Maybe Text)
    , _sduRoleARN                  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3DestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sduPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
--
-- * 'sduCloudWatchLoggingOptions' - Describes CloudWatch logging options for your delivery stream.
--
-- * 'sduEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sduCompressionFormat' - The compression format. If no value is specified, the default is @NOCOMPRESSION@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sduBufferingHints' - The buffering option. If no value is specified, __BufferingHints__ object default values are used.
--
-- * 'sduBucketARN' - The ARN of the S3 bucket.
--
-- * 'sduRoleARN' - The ARN of the AWS credentials.
s3DestinationUpdate
    :: S3DestinationUpdate
s3DestinationUpdate =
    S3DestinationUpdate'
    { _sduPrefix = Nothing
    , _sduCloudWatchLoggingOptions = Nothing
    , _sduEncryptionConfiguration = Nothing
    , _sduCompressionFormat = Nothing
    , _sduBufferingHints = Nothing
    , _sduBucketARN = Nothing
    , _sduRoleARN = Nothing
    }

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered S3 files. You can specify an extra prefix to be added in front of the time format prefix. Note that if the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html Amazon S3 Object Name Format> in the <http://docs.aws.amazon.com/firehose/latest/dev/ Amazon Kinesis Firehose Developer Guide> .
sduPrefix :: Lens' S3DestinationUpdate (Maybe Text)
sduPrefix = lens _sduPrefix (\ s a -> s{_sduPrefix = a});

-- | Describes CloudWatch logging options for your delivery stream.
sduCloudWatchLoggingOptions :: Lens' S3DestinationUpdate (Maybe CloudWatchLoggingOptions)
sduCloudWatchLoggingOptions = lens _sduCloudWatchLoggingOptions (\ s a -> s{_sduCloudWatchLoggingOptions = a});

-- | The encryption configuration. If no value is specified, the default is no encryption.
sduEncryptionConfiguration :: Lens' S3DestinationUpdate (Maybe EncryptionConfiguration)
sduEncryptionConfiguration = lens _sduEncryptionConfiguration (\ s a -> s{_sduEncryptionConfiguration = a});

-- | The compression format. If no value is specified, the default is @NOCOMPRESSION@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sduCompressionFormat :: Lens' S3DestinationUpdate (Maybe CompressionFormat)
sduCompressionFormat = lens _sduCompressionFormat (\ s a -> s{_sduCompressionFormat = a});

-- | The buffering option. If no value is specified, __BufferingHints__ object default values are used.
sduBufferingHints :: Lens' S3DestinationUpdate (Maybe BufferingHints)
sduBufferingHints = lens _sduBufferingHints (\ s a -> s{_sduBufferingHints = a});

-- | The ARN of the S3 bucket.
sduBucketARN :: Lens' S3DestinationUpdate (Maybe Text)
sduBucketARN = lens _sduBucketARN (\ s a -> s{_sduBucketARN = a});

-- | The ARN of the AWS credentials.
sduRoleARN :: Lens' S3DestinationUpdate (Maybe Text)
sduRoleARN = lens _sduRoleARN (\ s a -> s{_sduRoleARN = a});

instance Hashable S3DestinationUpdate

instance NFData S3DestinationUpdate

instance ToJSON S3DestinationUpdate where
        toJSON S3DestinationUpdate'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _sduPrefix,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _sduCloudWatchLoggingOptions,
                  ("EncryptionConfiguration" .=) <$>
                    _sduEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _sduCompressionFormat,
                  ("BufferingHints" .=) <$> _sduBufferingHints,
                  ("BucketARN" .=) <$> _sduBucketARN,
                  ("RoleARN" .=) <$> _sduRoleARN])
