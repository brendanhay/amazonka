{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Product where

import Network.AWS.Firehose.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes hints for the buffering to perform before delivering data to the destination. These options are treated as hints, and therefore Kinesis Data Firehose might choose to use different values when it is optimal.
--
--
--
-- /See:/ 'bufferingHints' smart constructor.
data BufferingHints = BufferingHints'
  { _bhSizeInMBs         :: !(Maybe Nat)
  , _bhIntervalInSeconds :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- * 'bhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300.
bufferingHints
    :: BufferingHints
bufferingHints =
  BufferingHints' {_bhSizeInMBs = Nothing, _bhIntervalInSeconds = Nothing}


-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
bhSizeInMBs :: Lens' BufferingHints (Maybe Natural)
bhSizeInMBs = lens _bhSizeInMBs (\ s a -> s{_bhSizeInMBs = a}) . mapping _Nat

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300.
bhIntervalInSeconds :: Lens' BufferingHints (Maybe Natural)
bhIntervalInSeconds = lens _bhIntervalInSeconds (\ s a -> s{_bhIntervalInSeconds = a}) . mapping _Nat

instance FromJSON BufferingHints where
        parseJSON
          = withObject "BufferingHints"
              (\ x ->
                 BufferingHints' <$>
                   (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds"))

instance Hashable BufferingHints where

instance NFData BufferingHints where

instance ToJSON BufferingHints where
        toJSON BufferingHints'{..}
          = object
              (catMaybes
                 [("SizeInMBs" .=) <$> _bhSizeInMBs,
                  ("IntervalInSeconds" .=) <$> _bhIntervalInSeconds])

-- | Describes the Amazon CloudWatch logging options for your delivery stream.
--
--
--
-- /See:/ 'cloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { _cwloEnabled       :: !(Maybe Bool)
  , _cwloLogGroupName  :: !(Maybe Text)
  , _cwloLogStreamName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwloEnabled' - Enables or disables CloudWatch logging.
--
-- * 'cwloLogGroupName' - The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
--
-- * 'cwloLogStreamName' - The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
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
cwloEnabled = lens _cwloEnabled (\ s a -> s{_cwloEnabled = a})

-- | The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
cwloLogGroupName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogGroupName = lens _cwloLogGroupName (\ s a -> s{_cwloLogGroupName = a})

-- | The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
cwloLogStreamName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogStreamName = lens _cwloLogStreamName (\ s a -> s{_cwloLogStreamName = a})

instance FromJSON CloudWatchLoggingOptions where
        parseJSON
          = withObject "CloudWatchLoggingOptions"
              (\ x ->
                 CloudWatchLoggingOptions' <$>
                   (x .:? "Enabled") <*> (x .:? "LogGroupName") <*>
                     (x .:? "LogStreamName"))

instance Hashable CloudWatchLoggingOptions where

instance NFData CloudWatchLoggingOptions where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCopyOptions' - Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows: @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
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


-- | Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows: @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <http://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
ccCopyOptions :: Lens' CopyCommand (Maybe Text)
ccCopyOptions = lens _ccCopyOptions (\ s a -> s{_ccCopyOptions = a})

-- | A comma-separated list of column names.
ccDataTableColumns :: Lens' CopyCommand (Maybe Text)
ccDataTableColumns = lens _ccDataTableColumns (\ s a -> s{_ccDataTableColumns = a})

-- | The name of the target table. The table must already exist in the database.
ccDataTableName :: Lens' CopyCommand Text
ccDataTableName = lens _ccDataTableName (\ s a -> s{_ccDataTableName = a})

instance FromJSON CopyCommand where
        parseJSON
          = withObject "CopyCommand"
              (\ x ->
                 CopyCommand' <$>
                   (x .:? "CopyOptions") <*> (x .:? "DataTableColumns")
                     <*> (x .: "DataTableName"))

instance Hashable CopyCommand where

instance NFData CopyCommand where

instance ToJSON CopyCommand where
        toJSON CopyCommand'{..}
          = object
              (catMaybes
                 [("CopyOptions" .=) <$> _ccCopyOptions,
                  ("DataTableColumns" .=) <$> _ccDataTableColumns,
                  Just ("DataTableName" .= _ccDataTableName)])

-- | Specifies that you want Kinesis Data Firehose to convert data from the JSON format to the Parquet or ORC format before writing it to Amazon S3. Kinesis Data Firehose uses the serializer and deserializer that you specify, in addition to the column information from the AWS Glue table, to deserialize your input data from JSON and then serialize it to the Parquet or ORC format. For more information, see <https://docs.aws.amazon.com/firehose/latest/dev/record-format-conversion.html Kinesis Data Firehose Record Format Conversion> .
--
--
--
-- /See:/ 'dataFormatConversionConfiguration' smart constructor.
data DataFormatConversionConfiguration = DataFormatConversionConfiguration'
  { _dfccOutputFormatConfiguration :: !(Maybe OutputFormatConfiguration)
  , _dfccEnabled                   :: !(Maybe Bool)
  , _dfccSchemaConfiguration       :: !(Maybe SchemaConfiguration)
  , _dfccInputFormatConfiguration  :: !(Maybe InputFormatConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataFormatConversionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfccOutputFormatConfiguration' - Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format.
--
-- * 'dfccEnabled' - Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
--
-- * 'dfccSchemaConfiguration' - Specifies the AWS Glue Data Catalog table that contains the column information.
--
-- * 'dfccInputFormatConfiguration' - Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON.
dataFormatConversionConfiguration
    :: DataFormatConversionConfiguration
dataFormatConversionConfiguration =
  DataFormatConversionConfiguration'
    { _dfccOutputFormatConfiguration = Nothing
    , _dfccEnabled = Nothing
    , _dfccSchemaConfiguration = Nothing
    , _dfccInputFormatConfiguration = Nothing
    }


-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data to the Parquet or ORC format.
dfccOutputFormatConfiguration :: Lens' DataFormatConversionConfiguration (Maybe OutputFormatConfiguration)
dfccOutputFormatConfiguration = lens _dfccOutputFormatConfiguration (\ s a -> s{_dfccOutputFormatConfiguration = a})

-- | Defaults to @true@ . Set it to @false@ if you want to disable format conversion while preserving the configuration details.
dfccEnabled :: Lens' DataFormatConversionConfiguration (Maybe Bool)
dfccEnabled = lens _dfccEnabled (\ s a -> s{_dfccEnabled = a})

-- | Specifies the AWS Glue Data Catalog table that contains the column information.
dfccSchemaConfiguration :: Lens' DataFormatConversionConfiguration (Maybe SchemaConfiguration)
dfccSchemaConfiguration = lens _dfccSchemaConfiguration (\ s a -> s{_dfccSchemaConfiguration = a})

-- | Specifies the deserializer that you want Kinesis Data Firehose to use to convert the format of your data from JSON.
dfccInputFormatConfiguration :: Lens' DataFormatConversionConfiguration (Maybe InputFormatConfiguration)
dfccInputFormatConfiguration = lens _dfccInputFormatConfiguration (\ s a -> s{_dfccInputFormatConfiguration = a})

instance FromJSON DataFormatConversionConfiguration
         where
        parseJSON
          = withObject "DataFormatConversionConfiguration"
              (\ x ->
                 DataFormatConversionConfiguration' <$>
                   (x .:? "OutputFormatConfiguration") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "SchemaConfiguration")
                     <*> (x .:? "InputFormatConfiguration"))

instance Hashable DataFormatConversionConfiguration
         where

instance NFData DataFormatConversionConfiguration
         where

instance ToJSON DataFormatConversionConfiguration
         where
        toJSON DataFormatConversionConfiguration'{..}
          = object
              (catMaybes
                 [("OutputFormatConfiguration" .=) <$>
                    _dfccOutputFormatConfiguration,
                  ("Enabled" .=) <$> _dfccEnabled,
                  ("SchemaConfiguration" .=) <$>
                    _dfccSchemaConfiguration,
                  ("InputFormatConfiguration" .=) <$>
                    _dfccInputFormatConfiguration])

-- | Contains information about a delivery stream.
--
--
--
-- /See:/ 'deliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { _dsdDeliveryStreamEncryptionConfiguration :: !(Maybe DeliveryStreamEncryptionConfiguration)
  , _dsdCreateTimestamp :: !(Maybe POSIX)
  , _dsdSource :: !(Maybe SourceDescription)
  , _dsdLastUpdateTimestamp :: !(Maybe POSIX)
  , _dsdDeliveryStreamName :: !Text
  , _dsdDeliveryStreamARN :: !Text
  , _dsdDeliveryStreamStatus :: !DeliveryStreamStatus
  , _dsdDeliveryStreamType :: !DeliveryStreamType
  , _dsdVersionId :: !Text
  , _dsdDestinations :: ![DestinationDescription]
  , _dsdHasMoreDestinations :: !Bool
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdDeliveryStreamEncryptionConfiguration' - Indicates the server-side encryption (SSE) status for the delivery stream.
--
-- * 'dsdCreateTimestamp' - The date and time that the delivery stream was created.
--
-- * 'dsdSource' - If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
--
-- * 'dsdLastUpdateTimestamp' - The date and time that the delivery stream was last updated.
--
-- * 'dsdDeliveryStreamName' - The name of the delivery stream.
--
-- * 'dsdDeliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'dsdDeliveryStreamStatus' - The status of the delivery stream.
--
-- * 'dsdDeliveryStreamType' - The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
-- * 'dsdVersionId' - Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
--
-- * 'dsdDestinations' - The destinations.
--
-- * 'dsdHasMoreDestinations' - Indicates whether there are more destinations available to list.
deliveryStreamDescription
    :: Text -- ^ 'dsdDeliveryStreamName'
    -> Text -- ^ 'dsdDeliveryStreamARN'
    -> DeliveryStreamStatus -- ^ 'dsdDeliveryStreamStatus'
    -> DeliveryStreamType -- ^ 'dsdDeliveryStreamType'
    -> Text -- ^ 'dsdVersionId'
    -> Bool -- ^ 'dsdHasMoreDestinations'
    -> DeliveryStreamDescription
deliveryStreamDescription pDeliveryStreamName_ pDeliveryStreamARN_ pDeliveryStreamStatus_ pDeliveryStreamType_ pVersionId_ pHasMoreDestinations_ =
  DeliveryStreamDescription'
    { _dsdDeliveryStreamEncryptionConfiguration = Nothing
    , _dsdCreateTimestamp = Nothing
    , _dsdSource = Nothing
    , _dsdLastUpdateTimestamp = Nothing
    , _dsdDeliveryStreamName = pDeliveryStreamName_
    , _dsdDeliveryStreamARN = pDeliveryStreamARN_
    , _dsdDeliveryStreamStatus = pDeliveryStreamStatus_
    , _dsdDeliveryStreamType = pDeliveryStreamType_
    , _dsdVersionId = pVersionId_
    , _dsdDestinations = mempty
    , _dsdHasMoreDestinations = pHasMoreDestinations_
    }


-- | Indicates the server-side encryption (SSE) status for the delivery stream.
dsdDeliveryStreamEncryptionConfiguration :: Lens' DeliveryStreamDescription (Maybe DeliveryStreamEncryptionConfiguration)
dsdDeliveryStreamEncryptionConfiguration = lens _dsdDeliveryStreamEncryptionConfiguration (\ s a -> s{_dsdDeliveryStreamEncryptionConfiguration = a})

-- | The date and time that the delivery stream was created.
dsdCreateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdCreateTimestamp = lens _dsdCreateTimestamp (\ s a -> s{_dsdCreateTimestamp = a}) . mapping _Time

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
dsdSource :: Lens' DeliveryStreamDescription (Maybe SourceDescription)
dsdSource = lens _dsdSource (\ s a -> s{_dsdSource = a})

-- | The date and time that the delivery stream was last updated.
dsdLastUpdateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdLastUpdateTimestamp = lens _dsdLastUpdateTimestamp (\ s a -> s{_dsdLastUpdateTimestamp = a}) . mapping _Time

-- | The name of the delivery stream.
dsdDeliveryStreamName :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamName = lens _dsdDeliveryStreamName (\ s a -> s{_dsdDeliveryStreamName = a})

-- | The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dsdDeliveryStreamARN :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamARN = lens _dsdDeliveryStreamARN (\ s a -> s{_dsdDeliveryStreamARN = a})

-- | The status of the delivery stream.
dsdDeliveryStreamStatus :: Lens' DeliveryStreamDescription DeliveryStreamStatus
dsdDeliveryStreamStatus = lens _dsdDeliveryStreamStatus (\ s a -> s{_dsdDeliveryStreamStatus = a})

-- | The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
dsdDeliveryStreamType :: Lens' DeliveryStreamDescription DeliveryStreamType
dsdDeliveryStreamType = lens _dsdDeliveryStreamType (\ s a -> s{_dsdDeliveryStreamType = a})

-- | Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
dsdVersionId :: Lens' DeliveryStreamDescription Text
dsdVersionId = lens _dsdVersionId (\ s a -> s{_dsdVersionId = a})

-- | The destinations.
dsdDestinations :: Lens' DeliveryStreamDescription [DestinationDescription]
dsdDestinations = lens _dsdDestinations (\ s a -> s{_dsdDestinations = a}) . _Coerce

-- | Indicates whether there are more destinations available to list.
dsdHasMoreDestinations :: Lens' DeliveryStreamDescription Bool
dsdHasMoreDestinations = lens _dsdHasMoreDestinations (\ s a -> s{_dsdHasMoreDestinations = a})

instance FromJSON DeliveryStreamDescription where
        parseJSON
          = withObject "DeliveryStreamDescription"
              (\ x ->
                 DeliveryStreamDescription' <$>
                   (x .:? "DeliveryStreamEncryptionConfiguration") <*>
                     (x .:? "CreateTimestamp")
                     <*> (x .:? "Source")
                     <*> (x .:? "LastUpdateTimestamp")
                     <*> (x .: "DeliveryStreamName")
                     <*> (x .: "DeliveryStreamARN")
                     <*> (x .: "DeliveryStreamStatus")
                     <*> (x .: "DeliveryStreamType")
                     <*> (x .: "VersionId")
                     <*> (x .:? "Destinations" .!= mempty)
                     <*> (x .: "HasMoreDestinations"))

instance Hashable DeliveryStreamDescription where

instance NFData DeliveryStreamDescription where

-- | Indicates the server-side encryption (SSE) status for the delivery stream.
--
--
--
-- /See:/ 'deliveryStreamEncryptionConfiguration' smart constructor.
newtype DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { _dsecStatus :: Maybe DeliveryStreamEncryptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeliveryStreamEncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsecStatus' - For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' .
deliveryStreamEncryptionConfiguration
    :: DeliveryStreamEncryptionConfiguration
deliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration' {_dsecStatus = Nothing}


-- | For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' .
dsecStatus :: Lens' DeliveryStreamEncryptionConfiguration (Maybe DeliveryStreamEncryptionStatus)
dsecStatus = lens _dsecStatus (\ s a -> s{_dsecStatus = a})

instance FromJSON
           DeliveryStreamEncryptionConfiguration
         where
        parseJSON
          = withObject "DeliveryStreamEncryptionConfiguration"
              (\ x ->
                 DeliveryStreamEncryptionConfiguration' <$>
                   (x .:? "Status"))

instance Hashable
           DeliveryStreamEncryptionConfiguration
         where

instance NFData DeliveryStreamEncryptionConfiguration
         where

-- | The deserializer you want Kinesis Data Firehose to use for converting the input data from JSON. Kinesis Data Firehose then serializes the data to its final format using the 'Serializer' . Kinesis Data Firehose supports two types of deserializers: the <https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL#LanguageManualDDL-JSON Apache Hive JSON SerDe> and the <https://github.com/rcongiu/Hive-JSON-Serde OpenX JSON SerDe> .
--
--
--
-- /See:/ 'deserializer' smart constructor.
data Deserializer = Deserializer'
  { _dOpenXJSONSerDe :: !(Maybe OpenXJSONSerDe)
  , _dHiveJSONSerDe  :: !(Maybe HiveJSONSerDe)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deserializer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dOpenXJSONSerDe' - The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
-- * 'dHiveJSONSerDe' - The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
deserializer
    :: Deserializer
deserializer =
  Deserializer' {_dOpenXJSONSerDe = Nothing, _dHiveJSONSerDe = Nothing}


-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
dOpenXJSONSerDe :: Lens' Deserializer (Maybe OpenXJSONSerDe)
dOpenXJSONSerDe = lens _dOpenXJSONSerDe (\ s a -> s{_dOpenXJSONSerDe = a})

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
dHiveJSONSerDe :: Lens' Deserializer (Maybe HiveJSONSerDe)
dHiveJSONSerDe = lens _dHiveJSONSerDe (\ s a -> s{_dHiveJSONSerDe = a})

instance FromJSON Deserializer where
        parseJSON
          = withObject "Deserializer"
              (\ x ->
                 Deserializer' <$>
                   (x .:? "OpenXJsonSerDe") <*> (x .:? "HiveJsonSerDe"))

instance Hashable Deserializer where

instance NFData Deserializer where

instance ToJSON Deserializer where
        toJSON Deserializer'{..}
          = object
              (catMaybes
                 [("OpenXJsonSerDe" .=) <$> _dOpenXJSONSerDe,
                  ("HiveJsonSerDe" .=) <$> _dHiveJSONSerDe])

-- | Describes the destination for a delivery stream.
--
--
--
-- /See:/ 'destinationDescription' smart constructor.
data DestinationDescription = DestinationDescription'
  { _ddSplunkDestinationDescription :: !(Maybe SplunkDestinationDescription)
  , _ddS3DestinationDescription :: !(Maybe S3DestinationDescription)
  , _ddExtendedS3DestinationDescription :: !(Maybe ExtendedS3DestinationDescription)
  , _ddElasticsearchDestinationDescription :: !(Maybe ElasticsearchDestinationDescription)
  , _ddRedshiftDestinationDescription :: !(Maybe RedshiftDestinationDescription)
  , _ddDestinationId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddSplunkDestinationDescription' - The destination in Splunk.
--
-- * 'ddS3DestinationDescription' - [Deprecated] The destination in Amazon S3.
--
-- * 'ddExtendedS3DestinationDescription' - The destination in Amazon S3.
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
    { _ddSplunkDestinationDescription = Nothing
    , _ddS3DestinationDescription = Nothing
    , _ddExtendedS3DestinationDescription = Nothing
    , _ddElasticsearchDestinationDescription = Nothing
    , _ddRedshiftDestinationDescription = Nothing
    , _ddDestinationId = pDestinationId_
    }


-- | The destination in Splunk.
ddSplunkDestinationDescription :: Lens' DestinationDescription (Maybe SplunkDestinationDescription)
ddSplunkDestinationDescription = lens _ddSplunkDestinationDescription (\ s a -> s{_ddSplunkDestinationDescription = a})

-- | [Deprecated] The destination in Amazon S3.
ddS3DestinationDescription :: Lens' DestinationDescription (Maybe S3DestinationDescription)
ddS3DestinationDescription = lens _ddS3DestinationDescription (\ s a -> s{_ddS3DestinationDescription = a})

-- | The destination in Amazon S3.
ddExtendedS3DestinationDescription :: Lens' DestinationDescription (Maybe ExtendedS3DestinationDescription)
ddExtendedS3DestinationDescription = lens _ddExtendedS3DestinationDescription (\ s a -> s{_ddExtendedS3DestinationDescription = a})

-- | The destination in Amazon ES.
ddElasticsearchDestinationDescription :: Lens' DestinationDescription (Maybe ElasticsearchDestinationDescription)
ddElasticsearchDestinationDescription = lens _ddElasticsearchDestinationDescription (\ s a -> s{_ddElasticsearchDestinationDescription = a})

-- | The destination in Amazon Redshift.
ddRedshiftDestinationDescription :: Lens' DestinationDescription (Maybe RedshiftDestinationDescription)
ddRedshiftDestinationDescription = lens _ddRedshiftDestinationDescription (\ s a -> s{_ddRedshiftDestinationDescription = a})

-- | The ID of the destination.
ddDestinationId :: Lens' DestinationDescription Text
ddDestinationId = lens _ddDestinationId (\ s a -> s{_ddDestinationId = a})

instance FromJSON DestinationDescription where
        parseJSON
          = withObject "DestinationDescription"
              (\ x ->
                 DestinationDescription' <$>
                   (x .:? "SplunkDestinationDescription") <*>
                     (x .:? "S3DestinationDescription")
                     <*> (x .:? "ExtendedS3DestinationDescription")
                     <*> (x .:? "ElasticsearchDestinationDescription")
                     <*> (x .:? "RedshiftDestinationDescription")
                     <*> (x .: "DestinationId"))

instance Hashable DestinationDescription where

instance NFData DestinationDescription where

-- | Describes the buffering to perform before delivering data to the Amazon ES destination.
--
--
--
-- /See:/ 'elasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
  { _ebhSizeInMBs         :: !(Maybe Nat)
  , _ebhIntervalInSeconds :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchBufferingHints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebhSizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- * 'ebhIntervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
elasticsearchBufferingHints
    :: ElasticsearchBufferingHints
elasticsearchBufferingHints =
  ElasticsearchBufferingHints'
    {_ebhSizeInMBs = Nothing, _ebhIntervalInSeconds = Nothing}


-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5. We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
ebhSizeInMBs :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhSizeInMBs = lens _ebhSizeInMBs (\ s a -> s{_ebhSizeInMBs = a}) . mapping _Nat

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
ebhIntervalInSeconds :: Lens' ElasticsearchBufferingHints (Maybe Natural)
ebhIntervalInSeconds = lens _ebhIntervalInSeconds (\ s a -> s{_ebhIntervalInSeconds = a}) . mapping _Nat

instance FromJSON ElasticsearchBufferingHints where
        parseJSON
          = withObject "ElasticsearchBufferingHints"
              (\ x ->
                 ElasticsearchBufferingHints' <$>
                   (x .:? "SizeInMBs") <*> (x .:? "IntervalInSeconds"))

instance Hashable ElasticsearchBufferingHints where

instance NFData ElasticsearchBufferingHints where

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
  , _edcProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _edcRoleARN                  :: !Text
  , _edcDomainARN                :: !Text
  , _edcIndexName                :: !Text
  , _edcTypeName                 :: !Text
  , _edcS3Configuration          :: !S3DestinationConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a timestamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is
