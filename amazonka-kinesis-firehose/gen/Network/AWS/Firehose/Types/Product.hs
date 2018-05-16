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
  { _dsdCreateTimestamp      :: !(Maybe POSIX)
  , _dsdSource               :: !(Maybe SourceDescription)
  , _dsdLastUpdateTimestamp  :: !(Maybe POSIX)
  , _dsdDeliveryStreamName   :: !Text
  , _dsdDeliveryStreamARN    :: !Text
  , _dsdDeliveryStreamStatus :: !DeliveryStreamStatus
  , _dsdDeliveryStreamType   :: !DeliveryStreamType
  , _dsdVersionId            :: !Text
  , _dsdDestinations         :: ![DestinationDescription]
  , _dsdHasMoreDestinations  :: !Bool
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
    { _dsdCreateTimestamp = Nothing
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
                   (x .:? "CreateTimestamp") <*> (x .:? "Source") <*>
                     (x .:? "LastUpdateTimestamp")
                     <*> (x .: "DeliveryStreamName")
                     <*> (x .: "DeliveryStreamARN")
                     <*> (x .: "DeliveryStreamStatus")
                     <*> (x .: "DeliveryStreamType")
                     <*> (x .: "VersionId")
                     <*> (x .:? "Destinations" .!= mempty)
                     <*> (x .: "HasMoreDestinations"))

instance Hashable DeliveryStreamDescription where

instance NFData DeliveryStreamDescription where

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
-- * 'edcIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a time stamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
--
-- * 'edcS3BackupMode' - Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
--
-- * 'edcCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'edcBufferingHints' - The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
--
-- * 'edcRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- * 'edcProcessingConfiguration' - The data processing configuration.
--
-- * 'edcRoleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'edcDomainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'edcIndexName' - The Elasticsearch index name.
--
-- * 'edcTypeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
--
-- * 'edcS3Configuration' - The configuration for the backup Amazon S3 location.
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
    , _edcProcessingConfiguration = Nothing
    , _edcRoleARN = pRoleARN_
    , _edcDomainARN = pDomainARN_
    , _edcIndexName = pIndexName_
    , _edcTypeName = pTypeName_
    , _edcS3Configuration = pS3Configuration_
    }


-- | The Elasticsearch index rotation period. Index rotation appends a time stamp to the @IndexName@ to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . The default value is @OneDay@ .
edcIndexRotationPeriod :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchIndexRotationPeriod)
edcIndexRotationPeriod = lens _edcIndexRotationPeriod (\ s a -> s{_edcIndexRotationPeriod = a})

-- | Defines how documents should be delivered to Amazon S3. When it is set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any documents that could not be indexed to the configured Amazon S3 destination, with @elasticsearch-failed/@ appended to the key prefix. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents with @elasticsearch-failed/@ appended to the prefix. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-s3-backup Amazon S3 Backup for the Amazon ES Destination> . Default value is @FailedDocumentsOnly@ .
edcS3BackupMode :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchS3BackupMode)
edcS3BackupMode = lens _edcS3BackupMode (\ s a -> s{_edcS3BackupMode = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
edcCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe CloudWatchLoggingOptions)
edcCloudWatchLoggingOptions = lens _edcCloudWatchLoggingOptions (\ s a -> s{_edcCloudWatchLoggingOptions = a})

-- | The buffering options. If no value is specified, the default values for @ElasticsearchBufferingHints@ are used.
edcBufferingHints :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchBufferingHints)
edcBufferingHints = lens _edcBufferingHints (\ s a -> s{_edcBufferingHints = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
edcRetryOptions :: Lens' ElasticsearchDestinationConfiguration (Maybe ElasticsearchRetryOptions)
edcRetryOptions = lens _edcRetryOptions (\ s a -> s{_edcRetryOptions = a})

-- | The data processing configuration.
edcProcessingConfiguration :: Lens' ElasticsearchDestinationConfiguration (Maybe ProcessingConfiguration)
edcProcessingConfiguration = lens _edcProcessingConfiguration (\ s a -> s{_edcProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
edcRoleARN :: Lens' ElasticsearchDestinationConfiguration Text
edcRoleARN = lens _edcRoleARN (\ s a -> s{_edcRoleARN = a})

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
edcDomainARN :: Lens' ElasticsearchDestinationConfiguration Text
edcDomainARN = lens _edcDomainARN (\ s a -> s{_edcDomainARN = a})

-- | The Elasticsearch index name.
edcIndexName :: Lens' ElasticsearchDestinationConfiguration Text
edcIndexName = lens _edcIndexName (\ s a -> s{_edcIndexName = a})

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during run time.
edcTypeName :: Lens' ElasticsearchDestinationConfiguration Text
edcTypeName = lens _edcTypeName (\ s a -> s{_edcTypeName = a})

-- | The configuration for the backup Amazon S3 location.
edcS3Configuration :: Lens' ElasticsearchDestinationConfiguration S3DestinationConfiguration
edcS3Configuration = lens _edcS3Configuration (\ s a -> s{_edcS3Configuration = a})

instance Hashable
           ElasticsearchDestinationConfiguration
         where

instance NFData ElasticsearchDestinationConfiguration
         where

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
                  ("ProcessingConfiguration" .=) <$>
                    _edcProcessingConfiguration,
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
  , _eddProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _eddRoleARN                  :: !(Maybe Text)
  , _eddIndexName                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eddIndexRotationPeriod' - The Elasticsearch index rotation period
--
-- * 'eddTypeName' - The Elasticsearch type name.
--
-- * 'eddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'eddDomainARN' - The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
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
    , _eddProcessingConfiguration = Nothing
    , _eddRoleARN = Nothing
    , _eddIndexName = Nothing
    }


-- | The Elasticsearch index rotation period
eddIndexRotationPeriod :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchIndexRotationPeriod)
eddIndexRotationPeriod = lens _eddIndexRotationPeriod (\ s a -> s{_eddIndexRotationPeriod = a})

-- | The Elasticsearch type name.
eddTypeName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddTypeName = lens _eddTypeName (\ s a -> s{_eddTypeName = a})

-- | The Amazon S3 backup mode.
eddS3BackupMode :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchS3BackupMode)
eddS3BackupMode = lens _eddS3BackupMode (\ s a -> s{_eddS3BackupMode = a})

-- | The ARN of the Amazon ES domain. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
eddDomainARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddDomainARN = lens _eddDomainARN (\ s a -> s{_eddDomainARN = a})

-- | The Amazon CloudWatch logging options.
eddCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationDescription (Maybe CloudWatchLoggingOptions)
eddCloudWatchLoggingOptions = lens _eddCloudWatchLoggingOptions (\ s a -> s{_eddCloudWatchLoggingOptions = a})

-- | The Amazon S3 destination.
eddS3DestinationDescription :: Lens' ElasticsearchDestinationDescription (Maybe S3DestinationDescription)
eddS3DestinationDescription = lens _eddS3DestinationDescription (\ s a -> s{_eddS3DestinationDescription = a})

-- | The buffering options.
eddBufferingHints :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchBufferingHints)
eddBufferingHints = lens _eddBufferingHints (\ s a -> s{_eddBufferingHints = a})

-- | The Amazon ES retry options.
eddRetryOptions :: Lens' ElasticsearchDestinationDescription (Maybe ElasticsearchRetryOptions)
eddRetryOptions = lens _eddRetryOptions (\ s a -> s{_eddRetryOptions = a})

-- | The data processing configuration.
eddProcessingConfiguration :: Lens' ElasticsearchDestinationDescription (Maybe ProcessingConfiguration)
eddProcessingConfiguration = lens _eddProcessingConfiguration (\ s a -> s{_eddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
eddRoleARN :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddRoleARN = lens _eddRoleARN (\ s a -> s{_eddRoleARN = a})

-- | The Elasticsearch index name.
eddIndexName :: Lens' ElasticsearchDestinationDescription (Maybe Text)
eddIndexName = lens _eddIndexName (\ s a -> s{_eddIndexName = a})

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
                     <*> (x .:? "ProcessingConfiguration")
                     <*> (x .:? "RoleARN")
                     <*> (x .:? "IndexName"))

instance Hashable ElasticsearchDestinationDescription
         where

instance NFData ElasticsearchDestinationDescription
         where

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
  , _eduProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _eduRoleARN                  :: !(Maybe Text)
  , _eduIndexName                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eduIndexRotationPeriod' - The Elasticsearch index rotation period. Index rotation appends a time stamp to @IndexName@ to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
--
-- * 'eduTypeName' - The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
--
-- * 'eduDomainARN' - The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'eduCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'eduS3Update' - The Amazon S3 destination.
--
-- * 'eduBufferingHints' - The buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
--
-- * 'eduRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
--
-- * 'eduProcessingConfiguration' - The data processing configuration.
--
-- * 'eduRoleARN' - The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
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
    , _eduProcessingConfiguration = Nothing
    , _eduRoleARN = Nothing
    , _eduIndexName = Nothing
    }


-- | The Elasticsearch index rotation period. Index rotation appends a time stamp to @IndexName@ to facilitate the expiration of old data. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#es-index-rotation Index Rotation for the Amazon ES Destination> . Default value is @OneDay@ .
eduIndexRotationPeriod :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchIndexRotationPeriod)
eduIndexRotationPeriod = lens _eduIndexRotationPeriod (\ s a -> s{_eduIndexRotationPeriod = a})

-- | The Elasticsearch type name. For Elasticsearch 6.x, there can be only one type per index. If you try to specify a new type for an existing index that already has another type, Kinesis Data Firehose returns an error during runtime.
eduTypeName :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduTypeName = lens _eduTypeName (\ s a -> s{_eduTypeName = a})

-- | The ARN of the Amazon ES domain. The IAM role must have permissions for @DescribeElasticsearchDomain@ , @DescribeElasticsearchDomains@ , and @DescribeElasticsearchDomainConfig@ after assuming the IAM role specified in __RoleARN__ . For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
eduDomainARN :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduDomainARN = lens _eduDomainARN (\ s a -> s{_eduDomainARN = a})

-- | The CloudWatch logging options for your delivery stream.
eduCloudWatchLoggingOptions :: Lens' ElasticsearchDestinationUpdate (Maybe CloudWatchLoggingOptions)
eduCloudWatchLoggingOptions = lens _eduCloudWatchLoggingOptions (\ s a -> s{_eduCloudWatchLoggingOptions = a})

-- | The Amazon S3 destination.
eduS3Update :: Lens' ElasticsearchDestinationUpdate (Maybe S3DestinationUpdate)
eduS3Update = lens _eduS3Update (\ s a -> s{_eduS3Update = a})

-- | The buffering options. If no value is specified, __ElasticsearchBufferingHints__ object default values are used.
eduBufferingHints :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchBufferingHints)
eduBufferingHints = lens _eduBufferingHints (\ s a -> s{_eduBufferingHints = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES. The default value is 300 (5 minutes).
eduRetryOptions :: Lens' ElasticsearchDestinationUpdate (Maybe ElasticsearchRetryOptions)
eduRetryOptions = lens _eduRetryOptions (\ s a -> s{_eduRetryOptions = a})

-- | The data processing configuration.
eduProcessingConfiguration :: Lens' ElasticsearchDestinationUpdate (Maybe ProcessingConfiguration)
eduProcessingConfiguration = lens _eduProcessingConfiguration (\ s a -> s{_eduProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed by Kinesis Data Firehose for calling the Amazon ES Configuration API and for indexing documents. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Grant Kinesis Data Firehose Access to an Amazon S3 Destination> and <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
eduRoleARN :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduRoleARN = lens _eduRoleARN (\ s a -> s{_eduRoleARN = a})

-- | The Elasticsearch index name.
eduIndexName :: Lens' ElasticsearchDestinationUpdate (Maybe Text)
eduIndexName = lens _eduIndexName (\ s a -> s{_eduIndexName = a})

instance Hashable ElasticsearchDestinationUpdate
         where

instance NFData ElasticsearchDestinationUpdate where

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
                  ("ProcessingConfiguration" .=) <$>
                    _eduProcessingConfiguration,
                  ("RoleARN" .=) <$> _eduRoleARN,
                  ("IndexName" .=) <$> _eduIndexName])

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon ES.
--
--
--
-- /See:/ 'elasticsearchRetryOptions' smart constructor.
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions'
  { _eroDurationInSeconds :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eroDurationInSeconds' - After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
elasticsearchRetryOptions
    :: ElasticsearchRetryOptions
elasticsearchRetryOptions =
  ElasticsearchRetryOptions' {_eroDurationInSeconds = Nothing}


-- | After an initial failure to deliver to Amazon ES, the total amount of time during which Kinesis Data Firehose retries delivery (including the first attempt). After this time has elapsed, the failed documents are written to Amazon S3. Default value is 300 seconds (5 minutes). A value of 0 (zero) results in no retries.
eroDurationInSeconds :: Lens' ElasticsearchRetryOptions (Maybe Natural)
eroDurationInSeconds = lens _eroDurationInSeconds (\ s a -> s{_eroDurationInSeconds = a}) . mapping _Nat

instance FromJSON ElasticsearchRetryOptions where
        parseJSON
          = withObject "ElasticsearchRetryOptions"
              (\ x ->
                 ElasticsearchRetryOptions' <$>
                   (x .:? "DurationInSeconds"))

instance Hashable ElasticsearchRetryOptions where

instance NFData ElasticsearchRetryOptions where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecNoEncryptionConfig' - Specifically override existing encryption information to ensure that no encryption is used.
--
-- * 'ecKMSEncryptionConfig' - The encryption key.
encryptionConfiguration
    :: EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration'
    {_ecNoEncryptionConfig = Nothing, _ecKMSEncryptionConfig = Nothing}


-- | Specifically override existing encryption information to ensure that no encryption is used.
ecNoEncryptionConfig :: Lens' EncryptionConfiguration (Maybe NoEncryptionConfig)
ecNoEncryptionConfig = lens _ecNoEncryptionConfig (\ s a -> s{_ecNoEncryptionConfig = a})

-- | The encryption key.
ecKMSEncryptionConfig :: Lens' EncryptionConfiguration (Maybe KMSEncryptionConfig)
ecKMSEncryptionConfig = lens _ecKMSEncryptionConfig (\ s a -> s{_ecKMSEncryptionConfig = a})

instance FromJSON EncryptionConfiguration where
        parseJSON
          = withObject "EncryptionConfiguration"
              (\ x ->
                 EncryptionConfiguration' <$>
                   (x .:? "NoEncryptionConfig") <*>
                     (x .:? "KMSEncryptionConfig"))

instance Hashable EncryptionConfiguration where

instance NFData EncryptionConfiguration where

instance ToJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration'{..}
          = object
              (catMaybes
                 [("NoEncryptionConfig" .=) <$> _ecNoEncryptionConfig,
                  ("KMSEncryptionConfig" .=) <$>
                    _ecKMSEncryptionConfig])

-- | Describes the configuration of a destination in Amazon S3.
--
--
--
-- /See:/ 'extendedS3DestinationConfiguration' smart constructor.
data ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration'
  { _esdcS3BackupMode :: !(Maybe S3BackupMode)
  , _esdcPrefix :: !(Maybe Text)
  , _esdcCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _esdcS3BackupConfiguration :: !(Maybe S3DestinationConfiguration)
  , _esdcEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
  , _esdcCompressionFormat :: !(Maybe CompressionFormat)
  , _esdcBufferingHints :: !(Maybe BufferingHints)
  , _esdcDataFormatConversionConfiguration :: !(Maybe DataFormatConversionConfiguration)
  , _esdcProcessingConfiguration :: !(Maybe ProcessingConfiguration)
  , _esdcRoleARN :: !Text
  , _esdcBucketARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtendedS3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esdcS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'esdcPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 'esdcCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'esdcS3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- * 'esdcEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'esdcCompressionFormat' - The compression format. If no value is specified, the default is UNCOMPRESSED.
--
-- * 'esdcBufferingHints' - The buffering option.
--
-- * 'esdcDataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- * 'esdcProcessingConfiguration' - The data processing configuration.
--
-- * 'esdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esdcBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
extendedS3DestinationConfiguration
    :: Text -- ^ 'esdcRoleARN'
    -> Text -- ^ 'esdcBucketARN'
    -> ExtendedS3DestinationConfiguration
extendedS3DestinationConfiguration pRoleARN_ pBucketARN_ =
  ExtendedS3DestinationConfiguration'
    { _esdcS3BackupMode = Nothing
    , _esdcPrefix = Nothing
    , _esdcCloudWatchLoggingOptions = Nothing
    , _esdcS3BackupConfiguration = Nothing
    , _esdcEncryptionConfiguration = Nothing
    , _esdcCompressionFormat = Nothing
    , _esdcBufferingHints = Nothing
    , _esdcDataFormatConversionConfiguration = Nothing
    , _esdcProcessingConfiguration = Nothing
    , _esdcRoleARN = pRoleARN_
    , _esdcBucketARN = pBucketARN_
    }


-- | The Amazon S3 backup mode.
esdcS3BackupMode :: Lens' ExtendedS3DestinationConfiguration (Maybe S3BackupMode)
esdcS3BackupMode = lens _esdcS3BackupMode (\ s a -> s{_esdcS3BackupMode = a})

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
esdcPrefix :: Lens' ExtendedS3DestinationConfiguration (Maybe Text)
esdcPrefix = lens _esdcPrefix (\ s a -> s{_esdcPrefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
esdcCloudWatchLoggingOptions :: Lens' ExtendedS3DestinationConfiguration (Maybe CloudWatchLoggingOptions)
esdcCloudWatchLoggingOptions = lens _esdcCloudWatchLoggingOptions (\ s a -> s{_esdcCloudWatchLoggingOptions = a})

-- | The configuration for backup in Amazon S3.
esdcS3BackupConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe S3DestinationConfiguration)
esdcS3BackupConfiguration = lens _esdcS3BackupConfiguration (\ s a -> s{_esdcS3BackupConfiguration = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
esdcEncryptionConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe EncryptionConfiguration)
esdcEncryptionConfiguration = lens _esdcEncryptionConfiguration (\ s a -> s{_esdcEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is UNCOMPRESSED.
esdcCompressionFormat :: Lens' ExtendedS3DestinationConfiguration (Maybe CompressionFormat)
esdcCompressionFormat = lens _esdcCompressionFormat (\ s a -> s{_esdcCompressionFormat = a})

-- | The buffering option.
esdcBufferingHints :: Lens' ExtendedS3DestinationConfiguration (Maybe BufferingHints)
esdcBufferingHints = lens _esdcBufferingHints (\ s a -> s{_esdcBufferingHints = a})

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
esdcDataFormatConversionConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe DataFormatConversionConfiguration)
esdcDataFormatConversionConfiguration = lens _esdcDataFormatConversionConfiguration (\ s a -> s{_esdcDataFormatConversionConfiguration = a})

-- | The data processing configuration.
esdcProcessingConfiguration :: Lens' ExtendedS3DestinationConfiguration (Maybe ProcessingConfiguration)
esdcProcessingConfiguration = lens _esdcProcessingConfiguration (\ s a -> s{_esdcProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esdcRoleARN :: Lens' ExtendedS3DestinationConfiguration Text
esdcRoleARN = lens _esdcRoleARN (\ s a -> s{_esdcRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esdcBucketARN :: Lens' ExtendedS3DestinationConfiguration Text
esdcBucketARN = lens _esdcBucketARN (\ s a -> s{_esdcBucketARN = a})

instance Hashable ExtendedS3DestinationConfiguration
         where

instance NFData ExtendedS3DestinationConfiguration
         where

instance ToJSON ExtendedS3DestinationConfiguration
         where
        toJSON ExtendedS3DestinationConfiguration'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _esdcS3BackupMode,
                  ("Prefix" .=) <$> _esdcPrefix,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _esdcCloudWatchLoggingOptions,
                  ("S3BackupConfiguration" .=) <$>
                    _esdcS3BackupConfiguration,
                  ("EncryptionConfiguration" .=) <$>
                    _esdcEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _esdcCompressionFormat,
                  ("BufferingHints" .=) <$> _esdcBufferingHints,
                  ("DataFormatConversionConfiguration" .=) <$>
                    _esdcDataFormatConversionConfiguration,
                  ("ProcessingConfiguration" .=) <$>
                    _esdcProcessingConfiguration,
                  Just ("RoleARN" .= _esdcRoleARN),
                  Just ("BucketARN" .= _esdcBucketARN)])

-- | Describes a destination in Amazon S3.
--
--
--
-- /See:/ 'extendedS3DestinationDescription' smart constructor.
data ExtendedS3DestinationDescription = ExtendedS3DestinationDescription'
  { _esddS3BackupMode :: !(Maybe S3BackupMode)
  , _esddS3BackupDescription :: !(Maybe S3DestinationDescription)
  , _esddPrefix :: !(Maybe Text)
  , _esddCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _esddDataFormatConversionConfiguration :: !(Maybe DataFormatConversionConfiguration)
  , _esddProcessingConfiguration :: !(Maybe ProcessingConfiguration)
  , _esddRoleARN :: !Text
  , _esddBucketARN :: !Text
  , _esddBufferingHints :: !BufferingHints
  , _esddCompressionFormat :: !CompressionFormat
  , _esddEncryptionConfiguration :: !EncryptionConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtendedS3DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'esddS3BackupDescription' - The configuration for backup in Amazon S3.
--
-- * 'esddPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 'esddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'esddDataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- * 'esddProcessingConfiguration' - The data processing configuration.
--
-- * 'esddRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esddBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esddBufferingHints' - The buffering option.
--
-- * 'esddCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- * 'esddEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
extendedS3DestinationDescription
    :: Text -- ^ 'esddRoleARN'
    -> Text -- ^ 'esddBucketARN'
    -> BufferingHints -- ^ 'esddBufferingHints'
    -> CompressionFormat -- ^ 'esddCompressionFormat'
    -> EncryptionConfiguration -- ^ 'esddEncryptionConfiguration'
    -> ExtendedS3DestinationDescription
extendedS3DestinationDescription pRoleARN_ pBucketARN_ pBufferingHints_ pCompressionFormat_ pEncryptionConfiguration_ =
  ExtendedS3DestinationDescription'
    { _esddS3BackupMode = Nothing
    , _esddS3BackupDescription = Nothing
    , _esddPrefix = Nothing
    , _esddCloudWatchLoggingOptions = Nothing
    , _esddDataFormatConversionConfiguration = Nothing
    , _esddProcessingConfiguration = Nothing
    , _esddRoleARN = pRoleARN_
    , _esddBucketARN = pBucketARN_
    , _esddBufferingHints = pBufferingHints_
    , _esddCompressionFormat = pCompressionFormat_
    , _esddEncryptionConfiguration = pEncryptionConfiguration_
    }


-- | The Amazon S3 backup mode.
esddS3BackupMode :: Lens' ExtendedS3DestinationDescription (Maybe S3BackupMode)
esddS3BackupMode = lens _esddS3BackupMode (\ s a -> s{_esddS3BackupMode = a})

-- | The configuration for backup in Amazon S3.
esddS3BackupDescription :: Lens' ExtendedS3DestinationDescription (Maybe S3DestinationDescription)
esddS3BackupDescription = lens _esddS3BackupDescription (\ s a -> s{_esddS3BackupDescription = a})

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
esddPrefix :: Lens' ExtendedS3DestinationDescription (Maybe Text)
esddPrefix = lens _esddPrefix (\ s a -> s{_esddPrefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
esddCloudWatchLoggingOptions :: Lens' ExtendedS3DestinationDescription (Maybe CloudWatchLoggingOptions)
esddCloudWatchLoggingOptions = lens _esddCloudWatchLoggingOptions (\ s a -> s{_esddCloudWatchLoggingOptions = a})

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
esddDataFormatConversionConfiguration :: Lens' ExtendedS3DestinationDescription (Maybe DataFormatConversionConfiguration)
esddDataFormatConversionConfiguration = lens _esddDataFormatConversionConfiguration (\ s a -> s{_esddDataFormatConversionConfiguration = a})

-- | The data processing configuration.
esddProcessingConfiguration :: Lens' ExtendedS3DestinationDescription (Maybe ProcessingConfiguration)
esddProcessingConfiguration = lens _esddProcessingConfiguration (\ s a -> s{_esddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esddRoleARN :: Lens' ExtendedS3DestinationDescription Text
esddRoleARN = lens _esddRoleARN (\ s a -> s{_esddRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esddBucketARN :: Lens' ExtendedS3DestinationDescription Text
esddBucketARN = lens _esddBucketARN (\ s a -> s{_esddBucketARN = a})

-- | The buffering option.
esddBufferingHints :: Lens' ExtendedS3DestinationDescription BufferingHints
esddBufferingHints = lens _esddBufferingHints (\ s a -> s{_esddBufferingHints = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
esddCompressionFormat :: Lens' ExtendedS3DestinationDescription CompressionFormat
esddCompressionFormat = lens _esddCompressionFormat (\ s a -> s{_esddCompressionFormat = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
esddEncryptionConfiguration :: Lens' ExtendedS3DestinationDescription EncryptionConfiguration
esddEncryptionConfiguration = lens _esddEncryptionConfiguration (\ s a -> s{_esddEncryptionConfiguration = a})

instance FromJSON ExtendedS3DestinationDescription
         where
        parseJSON
          = withObject "ExtendedS3DestinationDescription"
              (\ x ->
                 ExtendedS3DestinationDescription' <$>
                   (x .:? "S3BackupMode") <*>
                     (x .:? "S3BackupDescription")
                     <*> (x .:? "Prefix")
                     <*> (x .:? "CloudWatchLoggingOptions")
                     <*> (x .:? "DataFormatConversionConfiguration")
                     <*> (x .:? "ProcessingConfiguration")
                     <*> (x .: "RoleARN")
                     <*> (x .: "BucketARN")
                     <*> (x .: "BufferingHints")
                     <*> (x .: "CompressionFormat")
                     <*> (x .: "EncryptionConfiguration"))

instance Hashable ExtendedS3DestinationDescription
         where

instance NFData ExtendedS3DestinationDescription
         where

-- | Describes an update for a destination in Amazon S3.
--
--
--
-- /See:/ 'extendedS3DestinationUpdate' smart constructor.
data ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate'
  { _esduS3BackupMode :: !(Maybe S3BackupMode)
  , _esduPrefix :: !(Maybe Text)
  , _esduCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _esduS3BackupUpdate :: !(Maybe S3DestinationUpdate)
  , _esduEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
  , _esduCompressionFormat :: !(Maybe CompressionFormat)
  , _esduBufferingHints :: !(Maybe BufferingHints)
  , _esduDataFormatConversionConfiguration :: !(Maybe DataFormatConversionConfiguration)
  , _esduBucketARN :: !(Maybe Text)
  , _esduProcessingConfiguration :: !(Maybe ProcessingConfiguration)
  , _esduRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtendedS3DestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esduS3BackupMode' - Enables or disables Amazon S3 backup mode.
--
-- * 'esduPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 'esduCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'esduS3BackupUpdate' - The Amazon S3 destination for backup.
--
-- * 'esduEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'esduCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- * 'esduBufferingHints' - The buffering option.
--
-- * 'esduDataFormatConversionConfiguration' - The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
--
-- * 'esduBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'esduProcessingConfiguration' - The data processing configuration.
--
-- * 'esduRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
extendedS3DestinationUpdate
    :: ExtendedS3DestinationUpdate
extendedS3DestinationUpdate =
  ExtendedS3DestinationUpdate'
    { _esduS3BackupMode = Nothing
    , _esduPrefix = Nothing
    , _esduCloudWatchLoggingOptions = Nothing
    , _esduS3BackupUpdate = Nothing
    , _esduEncryptionConfiguration = Nothing
    , _esduCompressionFormat = Nothing
    , _esduBufferingHints = Nothing
    , _esduDataFormatConversionConfiguration = Nothing
    , _esduBucketARN = Nothing
    , _esduProcessingConfiguration = Nothing
    , _esduRoleARN = Nothing
    }


-- | Enables or disables Amazon S3 backup mode.
esduS3BackupMode :: Lens' ExtendedS3DestinationUpdate (Maybe S3BackupMode)
esduS3BackupMode = lens _esduS3BackupMode (\ s a -> s{_esduS3BackupMode = a})

-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
esduPrefix :: Lens' ExtendedS3DestinationUpdate (Maybe Text)
esduPrefix = lens _esduPrefix (\ s a -> s{_esduPrefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
esduCloudWatchLoggingOptions :: Lens' ExtendedS3DestinationUpdate (Maybe CloudWatchLoggingOptions)
esduCloudWatchLoggingOptions = lens _esduCloudWatchLoggingOptions (\ s a -> s{_esduCloudWatchLoggingOptions = a})

-- | The Amazon S3 destination for backup.
esduS3BackupUpdate :: Lens' ExtendedS3DestinationUpdate (Maybe S3DestinationUpdate)
esduS3BackupUpdate = lens _esduS3BackupUpdate (\ s a -> s{_esduS3BackupUpdate = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
esduEncryptionConfiguration :: Lens' ExtendedS3DestinationUpdate (Maybe EncryptionConfiguration)
esduEncryptionConfiguration = lens _esduEncryptionConfiguration (\ s a -> s{_esduEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
esduCompressionFormat :: Lens' ExtendedS3DestinationUpdate (Maybe CompressionFormat)
esduCompressionFormat = lens _esduCompressionFormat (\ s a -> s{_esduCompressionFormat = a})

-- | The buffering option.
esduBufferingHints :: Lens' ExtendedS3DestinationUpdate (Maybe BufferingHints)
esduBufferingHints = lens _esduBufferingHints (\ s a -> s{_esduBufferingHints = a})

-- | The serializer, deserializer, and schema for converting data from the JSON format to the Parquet or ORC format before writing it to Amazon S3.
esduDataFormatConversionConfiguration :: Lens' ExtendedS3DestinationUpdate (Maybe DataFormatConversionConfiguration)
esduDataFormatConversionConfiguration = lens _esduDataFormatConversionConfiguration (\ s a -> s{_esduDataFormatConversionConfiguration = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esduBucketARN :: Lens' ExtendedS3DestinationUpdate (Maybe Text)
esduBucketARN = lens _esduBucketARN (\ s a -> s{_esduBucketARN = a})

-- | The data processing configuration.
esduProcessingConfiguration :: Lens' ExtendedS3DestinationUpdate (Maybe ProcessingConfiguration)
esduProcessingConfiguration = lens _esduProcessingConfiguration (\ s a -> s{_esduProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
esduRoleARN :: Lens' ExtendedS3DestinationUpdate (Maybe Text)
esduRoleARN = lens _esduRoleARN (\ s a -> s{_esduRoleARN = a})

instance Hashable ExtendedS3DestinationUpdate where

instance NFData ExtendedS3DestinationUpdate where

instance ToJSON ExtendedS3DestinationUpdate where
        toJSON ExtendedS3DestinationUpdate'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _esduS3BackupMode,
                  ("Prefix" .=) <$> _esduPrefix,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _esduCloudWatchLoggingOptions,
                  ("S3BackupUpdate" .=) <$> _esduS3BackupUpdate,
                  ("EncryptionConfiguration" .=) <$>
                    _esduEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _esduCompressionFormat,
                  ("BufferingHints" .=) <$> _esduBufferingHints,
                  ("DataFormatConversionConfiguration" .=) <$>
                    _esduDataFormatConversionConfiguration,
                  ("BucketARN" .=) <$> _esduBucketARN,
                  ("ProcessingConfiguration" .=) <$>
                    _esduProcessingConfiguration,
                  ("RoleARN" .=) <$> _esduRoleARN])

-- | The native Hive / HCatalog JsonSerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the OpenX SerDe.
--
--
--
-- /See:/ 'hiveJSONSerDe' smart constructor.
newtype HiveJSONSerDe = HiveJSONSerDe'
  { _hjsdTimestampFormats :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HiveJSONSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hjsdTimestampFormats' - Indicates how you want Kinesis Data Firehose to parse the date and time stamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse time stamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
hiveJSONSerDe
    :: HiveJSONSerDe
hiveJSONSerDe = HiveJSONSerDe' {_hjsdTimestampFormats = Nothing}


-- | Indicates how you want Kinesis Data Firehose to parse the date and time stamps that may be present in your input data JSON. To specify these format strings, follow the pattern syntax of JodaTime's DateTimeFormat format strings. For more information, see <https://www.joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html Class DateTimeFormat> . You can also use the special value @millis@ to parse time stamps in epoch milliseconds. If you don't specify a format, Kinesis Data Firehose uses @java.sql.Timestamp::valueOf@ by default.
hjsdTimestampFormats :: Lens' HiveJSONSerDe [Text]
hjsdTimestampFormats = lens _hjsdTimestampFormats (\ s a -> s{_hjsdTimestampFormats = a}) . _Default . _Coerce

instance FromJSON HiveJSONSerDe where
        parseJSON
          = withObject "HiveJSONSerDe"
              (\ x ->
                 HiveJSONSerDe' <$>
                   (x .:? "TimestampFormats" .!= mempty))

instance Hashable HiveJSONSerDe where

instance NFData HiveJSONSerDe where

instance ToJSON HiveJSONSerDe where
        toJSON HiveJSONSerDe'{..}
          = object
              (catMaybes
                 [("TimestampFormats" .=) <$> _hjsdTimestampFormats])

-- | Specifies the deserializer you want to use to convert the format of the input data.
--
--
--
-- /See:/ 'inputFormatConfiguration' smart constructor.
newtype InputFormatConfiguration = InputFormatConfiguration'
  { _ifcDeserializer :: Maybe Deserializer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputFormatConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifcDeserializer' - Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
inputFormatConfiguration
    :: InputFormatConfiguration
inputFormatConfiguration =
  InputFormatConfiguration' {_ifcDeserializer = Nothing}


-- | Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
ifcDeserializer :: Lens' InputFormatConfiguration (Maybe Deserializer)
ifcDeserializer = lens _ifcDeserializer (\ s a -> s{_ifcDeserializer = a})

instance FromJSON InputFormatConfiguration where
        parseJSON
          = withObject "InputFormatConfiguration"
              (\ x ->
                 InputFormatConfiguration' <$> (x .:? "Deserializer"))

instance Hashable InputFormatConfiguration where

instance NFData InputFormatConfiguration where

instance ToJSON InputFormatConfiguration where
        toJSON InputFormatConfiguration'{..}
          = object
              (catMaybes
                 [("Deserializer" .=) <$> _ifcDeserializer])

-- | Describes an encryption key for a destination in Amazon S3.
--
--
--
-- /See:/ 'kmsEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
  { _kecAWSKMSKeyARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KMSEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kecAWSKMSKeyARN' - The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
kmsEncryptionConfig
    :: Text -- ^ 'kecAWSKMSKeyARN'
    -> KMSEncryptionConfig
kmsEncryptionConfig pAWSKMSKeyARN_ =
  KMSEncryptionConfig' {_kecAWSKMSKeyARN = pAWSKMSKeyARN_}


-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
kecAWSKMSKeyARN :: Lens' KMSEncryptionConfig Text
kecAWSKMSKeyARN = lens _kecAWSKMSKeyARN (\ s a -> s{_kecAWSKMSKeyARN = a})

instance FromJSON KMSEncryptionConfig where
        parseJSON
          = withObject "KMSEncryptionConfig"
              (\ x ->
                 KMSEncryptionConfig' <$> (x .: "AWSKMSKeyARN"))

instance Hashable KMSEncryptionConfig where

instance NFData KMSEncryptionConfig where

instance ToJSON KMSEncryptionConfig where
        toJSON KMSEncryptionConfig'{..}
          = object
              (catMaybes
                 [Just ("AWSKMSKeyARN" .= _kecAWSKMSKeyARN)])

-- | The stream and role Amazon Resource Names (ARNs) for a Kinesis data stream used as the source for a delivery stream.
--
--
--
-- /See:/ 'kinesisStreamSourceConfiguration' smart constructor.
data KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration'
  { _ksscKinesisStreamARN :: !Text
  , _ksscRoleARN          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamSourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksscKinesisStreamARN' - The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- * 'ksscRoleARN' - The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kinesisStreamSourceConfiguration
    :: Text -- ^ 'ksscKinesisStreamARN'
    -> Text -- ^ 'ksscRoleARN'
    -> KinesisStreamSourceConfiguration
kinesisStreamSourceConfiguration pKinesisStreamARN_ pRoleARN_ =
  KinesisStreamSourceConfiguration'
    {_ksscKinesisStreamARN = pKinesisStreamARN_, _ksscRoleARN = pRoleARN_}


-- | The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
ksscKinesisStreamARN :: Lens' KinesisStreamSourceConfiguration Text
ksscKinesisStreamARN = lens _ksscKinesisStreamARN (\ s a -> s{_ksscKinesisStreamARN = a})

-- | The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
ksscRoleARN :: Lens' KinesisStreamSourceConfiguration Text
ksscRoleARN = lens _ksscRoleARN (\ s a -> s{_ksscRoleARN = a})

instance Hashable KinesisStreamSourceConfiguration
         where

instance NFData KinesisStreamSourceConfiguration
         where

instance ToJSON KinesisStreamSourceConfiguration
         where
        toJSON KinesisStreamSourceConfiguration'{..}
          = object
              (catMaybes
                 [Just ("KinesisStreamARN" .= _ksscKinesisStreamARN),
                  Just ("RoleARN" .= _ksscRoleARN)])

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
--
--
-- /See:/ 'kinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { _kssdDeliveryStartTimestamp :: !(Maybe POSIX)
  , _kssdKinesisStreamARN       :: !(Maybe Text)
  , _kssdRoleARN                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kssdDeliveryStartTimestamp' - Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this time stamp.
--
-- * 'kssdKinesisStreamARN' - The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- * 'kssdRoleARN' - The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kinesisStreamSourceDescription
    :: KinesisStreamSourceDescription
kinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { _kssdDeliveryStartTimestamp = Nothing
    , _kssdKinesisStreamARN = Nothing
    , _kssdRoleARN = Nothing
    }


-- | Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this time stamp.
kssdDeliveryStartTimestamp :: Lens' KinesisStreamSourceDescription (Maybe UTCTime)
kssdDeliveryStartTimestamp = lens _kssdDeliveryStartTimestamp (\ s a -> s{_kssdDeliveryStartTimestamp = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
kssdKinesisStreamARN :: Lens' KinesisStreamSourceDescription (Maybe Text)
kssdKinesisStreamARN = lens _kssdKinesisStreamARN (\ s a -> s{_kssdKinesisStreamARN = a})

-- | The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kssdRoleARN :: Lens' KinesisStreamSourceDescription (Maybe Text)
kssdRoleARN = lens _kssdRoleARN (\ s a -> s{_kssdRoleARN = a})

instance FromJSON KinesisStreamSourceDescription
         where
        parseJSON
          = withObject "KinesisStreamSourceDescription"
              (\ x ->
                 KinesisStreamSourceDescription' <$>
                   (x .:? "DeliveryStartTimestamp") <*>
                     (x .:? "KinesisStreamARN")
                     <*> (x .:? "RoleARN"))

instance Hashable KinesisStreamSourceDescription
         where

instance NFData KinesisStreamSourceDescription where

-- | The OpenX SerDe. Used by Kinesis Data Firehose for deserializing data, which means converting it from the JSON format in preparation for serializing it to the Parquet or ORC format. This is one of two deserializers you can choose, depending on which one offers the functionality you need. The other option is the native Hive / HCatalog JsonSerDe.
--
--
--
-- /See:/ 'openXJSONSerDe' smart constructor.
data OpenXJSONSerDe = OpenXJSONSerDe'
  { _oxjsdColumnToJSONKeyMappings            :: !(Maybe (Map Text Text))
  , _oxjsdCaseInsensitive                    :: !(Maybe Bool)
  , _oxjsdConvertDotsInJSONKeysToUnderscores :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OpenXJSONSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oxjsdColumnToJSONKeyMappings' - Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
--
-- * 'oxjsdCaseInsensitive' - When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
--
-- * 'oxjsdConvertDotsInJSONKeysToUnderscores' - When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option. The default is @false@ .
openXJSONSerDe
    :: OpenXJSONSerDe
openXJSONSerDe =
  OpenXJSONSerDe'
    { _oxjsdColumnToJSONKeyMappings = Nothing
    , _oxjsdCaseInsensitive = Nothing
    , _oxjsdConvertDotsInJSONKeysToUnderscores = Nothing
    }


-- | Maps column names to JSON keys that aren't identical to the column names. This is useful when the JSON contains keys that are Hive keywords. For example, @timestamp@ is a Hive keyword. If you have a JSON key named @timestamp@ , set this parameter to @{"ts": "timestamp"}@ to map this key to a column named @ts@ .
oxjsdColumnToJSONKeyMappings :: Lens' OpenXJSONSerDe (HashMap Text Text)
oxjsdColumnToJSONKeyMappings = lens _oxjsdColumnToJSONKeyMappings (\ s a -> s{_oxjsdColumnToJSONKeyMappings = a}) . _Default . _Map

-- | When set to @true@ , which is the default, Kinesis Data Firehose converts JSON keys to lowercase before deserializing them.
oxjsdCaseInsensitive :: Lens' OpenXJSONSerDe (Maybe Bool)
oxjsdCaseInsensitive = lens _oxjsdCaseInsensitive (\ s a -> s{_oxjsdCaseInsensitive = a})

-- | When set to @true@ , specifies that the names of the keys include dots and that you want Kinesis Data Firehose to replace them with underscores. This is useful because Apache Hive does not allow dots in column names. For example, if the JSON contains a key whose name is "a.b", you can define the column name to be "a_b" when using this option. The default is @false@ .
oxjsdConvertDotsInJSONKeysToUnderscores :: Lens' OpenXJSONSerDe (Maybe Bool)
oxjsdConvertDotsInJSONKeysToUnderscores = lens _oxjsdConvertDotsInJSONKeysToUnderscores (\ s a -> s{_oxjsdConvertDotsInJSONKeysToUnderscores = a})

instance FromJSON OpenXJSONSerDe where
        parseJSON
          = withObject "OpenXJSONSerDe"
              (\ x ->
                 OpenXJSONSerDe' <$>
                   (x .:? "ColumnToJsonKeyMappings" .!= mempty) <*>
                     (x .:? "CaseInsensitive")
                     <*> (x .:? "ConvertDotsInJsonKeysToUnderscores"))

instance Hashable OpenXJSONSerDe where

instance NFData OpenXJSONSerDe where

instance ToJSON OpenXJSONSerDe where
        toJSON OpenXJSONSerDe'{..}
          = object
              (catMaybes
                 [("ColumnToJsonKeyMappings" .=) <$>
                    _oxjsdColumnToJSONKeyMappings,
                  ("CaseInsensitive" .=) <$> _oxjsdCaseInsensitive,
                  ("ConvertDotsInJsonKeysToUnderscores" .=) <$>
                    _oxjsdConvertDotsInJSONKeysToUnderscores])

-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
--
--
-- /See:/ 'orcSerDe' smart constructor.
data OrcSerDe = OrcSerDe'
  { _osdBloomFilterFalsePositiveProbability :: !(Maybe Double)
  , _osdDictionaryKeyThreshold              :: !(Maybe Double)
  , _osdEnablePadding                       :: !(Maybe Bool)
  , _osdCompression                         :: !(Maybe OrcCompression)
  , _osdBloomFilterColumns                  :: !(Maybe [Text])
  , _osdRowIndexStride                      :: !(Maybe Nat)
  , _osdFormatVersion                       :: !(Maybe OrcFormatVersion)
  , _osdBlockSizeBytes                      :: !(Maybe Nat)
  , _osdStripeSizeBytes                     :: !(Maybe Nat)
  , _osdPaddingTolerance                    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrcSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osdBloomFilterFalsePositiveProbability' - The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
--
-- * 'osdDictionaryKeyThreshold' - Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
--
-- * 'osdEnablePadding' - Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
--
-- * 'osdCompression' - The compression code to use over data blocks. The default is @SNAPPY@ .
--
-- * 'osdBloomFilterColumns' - The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
--
-- * 'osdRowIndexStride' - The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
--
-- * 'osdFormatVersion' - The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
--
-- * 'osdBlockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
--
-- * 'osdStripeSizeBytes' - The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
--
-- * 'osdPaddingTolerance' - A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size. For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task. Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
orcSerDe
    :: OrcSerDe
orcSerDe =
  OrcSerDe'
    { _osdBloomFilterFalsePositiveProbability = Nothing
    , _osdDictionaryKeyThreshold = Nothing
    , _osdEnablePadding = Nothing
    , _osdCompression = Nothing
    , _osdBloomFilterColumns = Nothing
    , _osdRowIndexStride = Nothing
    , _osdFormatVersion = Nothing
    , _osdBlockSizeBytes = Nothing
    , _osdStripeSizeBytes = Nothing
    , _osdPaddingTolerance = Nothing
    }


-- | The Bloom filter false positive probability (FPP). The lower the FPP, the bigger the Bloom filter. The default value is 0.05, the minimum is 0, and the maximum is 1.
osdBloomFilterFalsePositiveProbability :: Lens' OrcSerDe (Maybe Double)
osdBloomFilterFalsePositiveProbability = lens _osdBloomFilterFalsePositiveProbability (\ s a -> s{_osdBloomFilterFalsePositiveProbability = a})

-- | Represents the fraction of the total number of non-null rows. To turn off dictionary encoding, set this fraction to a number that is less than the number of distinct keys in a dictionary. To always use dictionary encoding, set this threshold to 1.
osdDictionaryKeyThreshold :: Lens' OrcSerDe (Maybe Double)
osdDictionaryKeyThreshold = lens _osdDictionaryKeyThreshold (\ s a -> s{_osdDictionaryKeyThreshold = a})

-- | Set this to @true@ to indicate that you want stripes to be padded to the HDFS block boundaries. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is @false@ .
osdEnablePadding :: Lens' OrcSerDe (Maybe Bool)
osdEnablePadding = lens _osdEnablePadding (\ s a -> s{_osdEnablePadding = a})

-- | The compression code to use over data blocks. The default is @SNAPPY@ .
osdCompression :: Lens' OrcSerDe (Maybe OrcCompression)
osdCompression = lens _osdCompression (\ s a -> s{_osdCompression = a})

-- | The column names for which you want Kinesis Data Firehose to create bloom filters. The default is @null@ .
osdBloomFilterColumns :: Lens' OrcSerDe [Text]
osdBloomFilterColumns = lens _osdBloomFilterColumns (\ s a -> s{_osdBloomFilterColumns = a}) . _Default . _Coerce

-- | The number of rows between index entries. The default is 10,000 and the minimum is 1,000.
osdRowIndexStride :: Lens' OrcSerDe (Maybe Natural)
osdRowIndexStride = lens _osdRowIndexStride (\ s a -> s{_osdRowIndexStride = a}) . mapping _Nat

-- | The version of the file to write. The possible values are @V0_11@ and @V0_12@ . The default is @V0_12@ .
osdFormatVersion :: Lens' OrcSerDe (Maybe OrcFormatVersion)
osdFormatVersion = lens _osdFormatVersion (\ s a -> s{_osdFormatVersion = a})

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
osdBlockSizeBytes :: Lens' OrcSerDe (Maybe Natural)
osdBlockSizeBytes = lens _osdBlockSizeBytes (\ s a -> s{_osdBlockSizeBytes = a}) . mapping _Nat

-- | The number of bytes in each stripe. The default is 64 MiB and the minimum is 8 MiB.
osdStripeSizeBytes :: Lens' OrcSerDe (Maybe Natural)
osdStripeSizeBytes = lens _osdStripeSizeBytes (\ s a -> s{_osdStripeSizeBytes = a}) . mapping _Nat

-- | A number between 0 and 1 that defines the tolerance for block padding as a decimal fraction of stripe size. The default value is 0.05, which means 5 percent of stripe size. For the default values of 64 MiB ORC stripes and 256 MiB HDFS blocks, the default block padding tolerance of 5 percent reserves a maximum of 3.2 MiB for padding within the 256 MiB block. In such a case, if the available size within the block is more than 3.2 MiB, a new, smaller stripe is inserted to fit within that space. This ensures that no stripe crosses block boundaries and causes remote reads within a node-local task. Kinesis Data Firehose ignores this parameter when 'OrcSerDe$EnablePadding' is @false@ .
osdPaddingTolerance :: Lens' OrcSerDe (Maybe Double)
osdPaddingTolerance = lens _osdPaddingTolerance (\ s a -> s{_osdPaddingTolerance = a})

instance FromJSON OrcSerDe where
        parseJSON
          = withObject "OrcSerDe"
              (\ x ->
                 OrcSerDe' <$>
                   (x .:? "BloomFilterFalsePositiveProbability") <*>
                     (x .:? "DictionaryKeyThreshold")
                     <*> (x .:? "EnablePadding")
                     <*> (x .:? "Compression")
                     <*> (x .:? "BloomFilterColumns" .!= mempty)
                     <*> (x .:? "RowIndexStride")
                     <*> (x .:? "FormatVersion")
                     <*> (x .:? "BlockSizeBytes")
                     <*> (x .:? "StripeSizeBytes")
                     <*> (x .:? "PaddingTolerance"))

instance Hashable OrcSerDe where

instance NFData OrcSerDe where

instance ToJSON OrcSerDe where
        toJSON OrcSerDe'{..}
          = object
              (catMaybes
                 [("BloomFilterFalsePositiveProbability" .=) <$>
                    _osdBloomFilterFalsePositiveProbability,
                  ("DictionaryKeyThreshold" .=) <$>
                    _osdDictionaryKeyThreshold,
                  ("EnablePadding" .=) <$> _osdEnablePadding,
                  ("Compression" .=) <$> _osdCompression,
                  ("BloomFilterColumns" .=) <$> _osdBloomFilterColumns,
                  ("RowIndexStride" .=) <$> _osdRowIndexStride,
                  ("FormatVersion" .=) <$> _osdFormatVersion,
                  ("BlockSizeBytes" .=) <$> _osdBlockSizeBytes,
                  ("StripeSizeBytes" .=) <$> _osdStripeSizeBytes,
                  ("PaddingTolerance" .=) <$> _osdPaddingTolerance])

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data before it writes it to Amazon S3.
--
--
--
-- /See:/ 'outputFormatConfiguration' smart constructor.
newtype OutputFormatConfiguration = OutputFormatConfiguration'
  { _ofcSerializer :: Maybe Serializer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputFormatConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofcSerializer' - Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
outputFormatConfiguration
    :: OutputFormatConfiguration
outputFormatConfiguration =
  OutputFormatConfiguration' {_ofcSerializer = Nothing}


-- | Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
ofcSerializer :: Lens' OutputFormatConfiguration (Maybe Serializer)
ofcSerializer = lens _ofcSerializer (\ s a -> s{_ofcSerializer = a})

instance FromJSON OutputFormatConfiguration where
        parseJSON
          = withObject "OutputFormatConfiguration"
              (\ x ->
                 OutputFormatConfiguration' <$> (x .:? "Serializer"))

instance Hashable OutputFormatConfiguration where

instance NFData OutputFormatConfiguration where

instance ToJSON OutputFormatConfiguration where
        toJSON OutputFormatConfiguration'{..}
          = object
              (catMaybes [("Serializer" .=) <$> _ofcSerializer])

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
--
--
--
-- /See:/ 'parquetSerDe' smart constructor.
data ParquetSerDe = ParquetSerDe'
  { _psdWriterVersion               :: !(Maybe ParquetWriterVersion)
  , _psdCompression                 :: !(Maybe ParquetCompression)
  , _psdMaxPaddingBytes             :: !(Maybe Nat)
  , _psdEnableDictionaryCompression :: !(Maybe Bool)
  , _psdPageSizeBytes               :: !(Maybe Nat)
  , _psdBlockSizeBytes              :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParquetSerDe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psdWriterVersion' - Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
--
-- * 'psdCompression' - The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ration is more important than speed.
--
-- * 'psdMaxPaddingBytes' - The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
--
-- * 'psdEnableDictionaryCompression' - Indicates whether to enable dictionary compression.
--
-- * 'psdPageSizeBytes' - The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
--
-- * 'psdBlockSizeBytes' - The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
parquetSerDe
    :: ParquetSerDe
parquetSerDe =
  ParquetSerDe'
    { _psdWriterVersion = Nothing
    , _psdCompression = Nothing
    , _psdMaxPaddingBytes = Nothing
    , _psdEnableDictionaryCompression = Nothing
    , _psdPageSizeBytes = Nothing
    , _psdBlockSizeBytes = Nothing
    }


-- | Indicates the version of row format to output. The possible values are @V1@ and @V2@ . The default is @V1@ .
psdWriterVersion :: Lens' ParquetSerDe (Maybe ParquetWriterVersion)
psdWriterVersion = lens _psdWriterVersion (\ s a -> s{_psdWriterVersion = a})

-- | The compression code to use over data blocks. The possible values are @UNCOMPRESSED@ , @SNAPPY@ , and @GZIP@ , with the default being @SNAPPY@ . Use @SNAPPY@ for higher decompression speed. Use @GZIP@ if the compression ration is more important than speed.
psdCompression :: Lens' ParquetSerDe (Maybe ParquetCompression)
psdCompression = lens _psdCompression (\ s a -> s{_psdCompression = a})

-- | The maximum amount of padding to apply. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 0.
psdMaxPaddingBytes :: Lens' ParquetSerDe (Maybe Natural)
psdMaxPaddingBytes = lens _psdMaxPaddingBytes (\ s a -> s{_psdMaxPaddingBytes = a}) . mapping _Nat

-- | Indicates whether to enable dictionary compression.
psdEnableDictionaryCompression :: Lens' ParquetSerDe (Maybe Bool)
psdEnableDictionaryCompression = lens _psdEnableDictionaryCompression (\ s a -> s{_psdEnableDictionaryCompression = a})

-- | The Parquet page size. Column chunks are divided into pages. A page is conceptually an indivisible unit (in terms of compression and encoding). The minimum value is 64 KiB and the default is 1 MiB.
psdPageSizeBytes :: Lens' ParquetSerDe (Maybe Natural)
psdPageSizeBytes = lens _psdPageSizeBytes (\ s a -> s{_psdPageSizeBytes = a}) . mapping _Nat

-- | The Hadoop Distributed File System (HDFS) block size. This is useful if you intend to copy the data from Amazon S3 to HDFS before querying. The default is 256 MiB and the minimum is 64 MiB. Kinesis Data Firehose uses this value for padding calculations.
psdBlockSizeBytes :: Lens' ParquetSerDe (Maybe Natural)
psdBlockSizeBytes = lens _psdBlockSizeBytes (\ s a -> s{_psdBlockSizeBytes = a}) . mapping _Nat

instance FromJSON ParquetSerDe where
        parseJSON
          = withObject "ParquetSerDe"
              (\ x ->
                 ParquetSerDe' <$>
                   (x .:? "WriterVersion") <*> (x .:? "Compression") <*>
                     (x .:? "MaxPaddingBytes")
                     <*> (x .:? "EnableDictionaryCompression")
                     <*> (x .:? "PageSizeBytes")
                     <*> (x .:? "BlockSizeBytes"))

instance Hashable ParquetSerDe where

instance NFData ParquetSerDe where

instance ToJSON ParquetSerDe where
        toJSON ParquetSerDe'{..}
          = object
              (catMaybes
                 [("WriterVersion" .=) <$> _psdWriterVersion,
                  ("Compression" .=) <$> _psdCompression,
                  ("MaxPaddingBytes" .=) <$> _psdMaxPaddingBytes,
                  ("EnableDictionaryCompression" .=) <$>
                    _psdEnableDictionaryCompression,
                  ("PageSizeBytes" .=) <$> _psdPageSizeBytes,
                  ("BlockSizeBytes" .=) <$> _psdBlockSizeBytes])

-- | Describes a data processing configuration.
--
--
--
-- /See:/ 'processingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { _pcEnabled    :: !(Maybe Bool)
  , _pcProcessors :: !(Maybe [Processor])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcEnabled' - Enables or disables data processing.
--
-- * 'pcProcessors' - The data processors.
processingConfiguration
    :: ProcessingConfiguration
processingConfiguration =
  ProcessingConfiguration' {_pcEnabled = Nothing, _pcProcessors = Nothing}


-- | Enables or disables data processing.
pcEnabled :: Lens' ProcessingConfiguration (Maybe Bool)
pcEnabled = lens _pcEnabled (\ s a -> s{_pcEnabled = a})

-- | The data processors.
pcProcessors :: Lens' ProcessingConfiguration [Processor]
pcProcessors = lens _pcProcessors (\ s a -> s{_pcProcessors = a}) . _Default . _Coerce

instance FromJSON ProcessingConfiguration where
        parseJSON
          = withObject "ProcessingConfiguration"
              (\ x ->
                 ProcessingConfiguration' <$>
                   (x .:? "Enabled") <*>
                     (x .:? "Processors" .!= mempty))

instance Hashable ProcessingConfiguration where

instance NFData ProcessingConfiguration where

instance ToJSON ProcessingConfiguration where
        toJSON ProcessingConfiguration'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _pcEnabled,
                  ("Processors" .=) <$> _pcProcessors])

-- | Describes a data processor.
--
--
--
-- /See:/ 'processor' smart constructor.
data Processor = Processor'
  { _pParameters :: !(Maybe [ProcessorParameter])
  , _pType       :: !ProcessorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Processor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameters' - The processor parameters.
--
-- * 'pType' - The type of processor.
processor
    :: ProcessorType -- ^ 'pType'
    -> Processor
processor pType_ = Processor' {_pParameters = Nothing, _pType = pType_}


-- | The processor parameters.
pParameters :: Lens' Processor [ProcessorParameter]
pParameters = lens _pParameters (\ s a -> s{_pParameters = a}) . _Default . _Coerce

-- | The type of processor.
pType :: Lens' Processor ProcessorType
pType = lens _pType (\ s a -> s{_pType = a})

instance FromJSON Processor where
        parseJSON
          = withObject "Processor"
              (\ x ->
                 Processor' <$>
                   (x .:? "Parameters" .!= mempty) <*> (x .: "Type"))

instance Hashable Processor where

instance NFData Processor where

instance ToJSON Processor where
        toJSON Processor'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _pParameters,
                  Just ("Type" .= _pType)])

-- | Describes the processor parameter.
--
--
--
-- /See:/ 'processorParameter' smart constructor.
data ProcessorParameter = ProcessorParameter'
  { _ppParameterName  :: !ProcessorParameterName
  , _ppParameterValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProcessorParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppParameterName' - The name of the parameter.
--
-- * 'ppParameterValue' - The parameter value.
processorParameter
    :: ProcessorParameterName -- ^ 'ppParameterName'
    -> Text -- ^ 'ppParameterValue'
    -> ProcessorParameter
processorParameter pParameterName_ pParameterValue_ =
  ProcessorParameter'
    {_ppParameterName = pParameterName_, _ppParameterValue = pParameterValue_}


-- | The name of the parameter.
ppParameterName :: Lens' ProcessorParameter ProcessorParameterName
ppParameterName = lens _ppParameterName (\ s a -> s{_ppParameterName = a})

-- | The parameter value.
ppParameterValue :: Lens' ProcessorParameter Text
ppParameterValue = lens _ppParameterValue (\ s a -> s{_ppParameterValue = a})

instance FromJSON ProcessorParameter where
        parseJSON
          = withObject "ProcessorParameter"
              (\ x ->
                 ProcessorParameter' <$>
                   (x .: "ParameterName") <*> (x .: "ParameterValue"))

instance Hashable ProcessorParameter where

instance NFData ProcessorParameter where

instance ToJSON ProcessorParameter where
        toJSON ProcessorParameter'{..}
          = object
              (catMaybes
                 [Just ("ParameterName" .= _ppParameterName),
                  Just ("ParameterValue" .= _ppParameterValue)])

-- | Contains the result for an individual record from a 'PutRecordBatch' request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.
--
--
--
-- /See:/ 'putRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { _prbreRecordId     :: !(Maybe Text)
  , _prbreErrorCode    :: !(Maybe Text)
  , _prbreErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
prbreRecordId = lens _prbreRecordId (\ s a -> s{_prbreRecordId = a})

-- | The error code for an individual record result.
prbreErrorCode :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorCode = lens _prbreErrorCode (\ s a -> s{_prbreErrorCode = a})

-- | The error message for an individual record result.
prbreErrorMessage :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorMessage = lens _prbreErrorMessage (\ s a -> s{_prbreErrorMessage = a})

instance FromJSON PutRecordBatchResponseEntry where
        parseJSON
          = withObject "PutRecordBatchResponseEntry"
              (\ x ->
                 PutRecordBatchResponseEntry' <$>
                   (x .:? "RecordId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable PutRecordBatchResponseEntry where

instance NFData PutRecordBatchResponseEntry where

-- | The unit of data in a delivery stream.
--
--
--
-- /See:/ 'record' smart constructor.
newtype Record = Record'
  { _rData :: Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rData' - The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KB.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
record
    :: ByteString -- ^ 'rData'
    -> Record
record pData_ = Record' {_rData = _Base64 # pData_}


-- | The data blob, which is base64-encoded when the blob is serialized. The maximum size of the data blob, before base64-encoding, is 1,000 KB.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rData :: Lens' Record ByteString
rData = lens _rData (\ s a -> s{_rData = a}) . _Base64

instance Hashable Record where

instance NFData Record where

instance ToJSON Record where
        toJSON Record'{..}
          = object (catMaybes [Just ("Data" .= _rData)])

-- | Describes the configuration of a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationConfiguration' smart constructor.
data RedshiftDestinationConfiguration = RedshiftDestinationConfiguration'
  { _rdcS3BackupMode             :: !(Maybe RedshiftS3BackupMode)
  , _rdcCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _rdcS3BackupConfiguration    :: !(Maybe S3DestinationConfiguration)
  , _rdcRetryOptions             :: !(Maybe RedshiftRetryOptions)
  , _rdcProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _rdcRoleARN                  :: !Text
  , _rdcClusterJDBCURL           :: !Text
  , _rdcCopyCommand              :: !CopyCommand
  , _rdcUsername                 :: !(Sensitive Text)
  , _rdcPassword                 :: !(Sensitive Text)
  , _rdcS3Configuration          :: !S3DestinationConfiguration
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedshiftDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'rdcCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'rdcS3BackupConfiguration' - The configuration for backup in Amazon S3.
--
-- * 'rdcRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rdcProcessingConfiguration' - The data processing configuration.
--
-- * 'rdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'rdcClusterJDBCURL' - The database connection string.
--
-- * 'rdcCopyCommand' - The @COPY@ command.
--
-- * 'rdcUsername' - The name of the user.
--
-- * 'rdcPassword' - The user password.
--
-- * 'rdcS3Configuration' - The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
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
    { _rdcS3BackupMode = Nothing
    , _rdcCloudWatchLoggingOptions = Nothing
    , _rdcS3BackupConfiguration = Nothing
    , _rdcRetryOptions = Nothing
    , _rdcProcessingConfiguration = Nothing
    , _rdcRoleARN = pRoleARN_
    , _rdcClusterJDBCURL = pClusterJDBCURL_
    , _rdcCopyCommand = pCopyCommand_
    , _rdcUsername = _Sensitive # pUsername_
    , _rdcPassword = _Sensitive # pPassword_
    , _rdcS3Configuration = pS3Configuration_
    }


-- | The Amazon S3 backup mode.
rdcS3BackupMode :: Lens' RedshiftDestinationConfiguration (Maybe RedshiftS3BackupMode)
rdcS3BackupMode = lens _rdcS3BackupMode (\ s a -> s{_rdcS3BackupMode = a})

-- | The CloudWatch logging options for your delivery stream.
rdcCloudWatchLoggingOptions :: Lens' RedshiftDestinationConfiguration (Maybe CloudWatchLoggingOptions)
rdcCloudWatchLoggingOptions = lens _rdcCloudWatchLoggingOptions (\ s a -> s{_rdcCloudWatchLoggingOptions = a})

-- | The configuration for backup in Amazon S3.
rdcS3BackupConfiguration :: Lens' RedshiftDestinationConfiguration (Maybe S3DestinationConfiguration)
rdcS3BackupConfiguration = lens _rdcS3BackupConfiguration (\ s a -> s{_rdcS3BackupConfiguration = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rdcRetryOptions :: Lens' RedshiftDestinationConfiguration (Maybe RedshiftRetryOptions)
rdcRetryOptions = lens _rdcRetryOptions (\ s a -> s{_rdcRetryOptions = a})

-- | The data processing configuration.
rdcProcessingConfiguration :: Lens' RedshiftDestinationConfiguration (Maybe ProcessingConfiguration)
rdcProcessingConfiguration = lens _rdcProcessingConfiguration (\ s a -> s{_rdcProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rdcRoleARN :: Lens' RedshiftDestinationConfiguration Text
rdcRoleARN = lens _rdcRoleARN (\ s a -> s{_rdcRoleARN = a})

-- | The database connection string.
rdcClusterJDBCURL :: Lens' RedshiftDestinationConfiguration Text
rdcClusterJDBCURL = lens _rdcClusterJDBCURL (\ s a -> s{_rdcClusterJDBCURL = a})

-- | The @COPY@ command.
rdcCopyCommand :: Lens' RedshiftDestinationConfiguration CopyCommand
rdcCopyCommand = lens _rdcCopyCommand (\ s a -> s{_rdcCopyCommand = a})

-- | The name of the user.
rdcUsername :: Lens' RedshiftDestinationConfiguration Text
rdcUsername = lens _rdcUsername (\ s a -> s{_rdcUsername = a}) . _Sensitive

-- | The user password.
rdcPassword :: Lens' RedshiftDestinationConfiguration Text
rdcPassword = lens _rdcPassword (\ s a -> s{_rdcPassword = a}) . _Sensitive

-- | The configuration for the intermediate Amazon S3 location from which Amazon Redshift obtains data. Restrictions are described in the topic for 'CreateDeliveryStream' . The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationConfiguration.S3Configuration@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
rdcS3Configuration :: Lens' RedshiftDestinationConfiguration S3DestinationConfiguration
rdcS3Configuration = lens _rdcS3Configuration (\ s a -> s{_rdcS3Configuration = a})

instance Hashable RedshiftDestinationConfiguration
         where

instance NFData RedshiftDestinationConfiguration
         where

instance ToJSON RedshiftDestinationConfiguration
         where
        toJSON RedshiftDestinationConfiguration'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _rdcS3BackupMode,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _rdcCloudWatchLoggingOptions,
                  ("S3BackupConfiguration" .=) <$>
                    _rdcS3BackupConfiguration,
                  ("RetryOptions" .=) <$> _rdcRetryOptions,
                  ("ProcessingConfiguration" .=) <$>
                    _rdcProcessingConfiguration,
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
  { _rddS3BackupMode             :: !(Maybe RedshiftS3BackupMode)
  , _rddS3BackupDescription      :: !(Maybe S3DestinationDescription)
  , _rddCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _rddRetryOptions             :: !(Maybe RedshiftRetryOptions)
  , _rddProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _rddRoleARN                  :: !Text
  , _rddClusterJDBCURL           :: !Text
  , _rddCopyCommand              :: !CopyCommand
  , _rddUsername                 :: !(Sensitive Text)
  , _rddS3DestinationDescription :: !S3DestinationDescription
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedshiftDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rddS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'rddS3BackupDescription' - The configuration for backup in Amazon S3.
--
-- * 'rddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'rddRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rddProcessingConfiguration' - The data processing configuration.
--
-- * 'rddRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
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
    { _rddS3BackupMode = Nothing
    , _rddS3BackupDescription = Nothing
    , _rddCloudWatchLoggingOptions = Nothing
    , _rddRetryOptions = Nothing
    , _rddProcessingConfiguration = Nothing
    , _rddRoleARN = pRoleARN_
    , _rddClusterJDBCURL = pClusterJDBCURL_
    , _rddCopyCommand = pCopyCommand_
    , _rddUsername = _Sensitive # pUsername_
    , _rddS3DestinationDescription = pS3DestinationDescription_
    }


-- | The Amazon S3 backup mode.
rddS3BackupMode :: Lens' RedshiftDestinationDescription (Maybe RedshiftS3BackupMode)
rddS3BackupMode = lens _rddS3BackupMode (\ s a -> s{_rddS3BackupMode = a})

-- | The configuration for backup in Amazon S3.
rddS3BackupDescription :: Lens' RedshiftDestinationDescription (Maybe S3DestinationDescription)
rddS3BackupDescription = lens _rddS3BackupDescription (\ s a -> s{_rddS3BackupDescription = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
rddCloudWatchLoggingOptions :: Lens' RedshiftDestinationDescription (Maybe CloudWatchLoggingOptions)
rddCloudWatchLoggingOptions = lens _rddCloudWatchLoggingOptions (\ s a -> s{_rddCloudWatchLoggingOptions = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rddRetryOptions :: Lens' RedshiftDestinationDescription (Maybe RedshiftRetryOptions)
rddRetryOptions = lens _rddRetryOptions (\ s a -> s{_rddRetryOptions = a})

-- | The data processing configuration.
rddProcessingConfiguration :: Lens' RedshiftDestinationDescription (Maybe ProcessingConfiguration)
rddProcessingConfiguration = lens _rddProcessingConfiguration (\ s a -> s{_rddProcessingConfiguration = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rddRoleARN :: Lens' RedshiftDestinationDescription Text
rddRoleARN = lens _rddRoleARN (\ s a -> s{_rddRoleARN = a})

-- | The database connection string.
rddClusterJDBCURL :: Lens' RedshiftDestinationDescription Text
rddClusterJDBCURL = lens _rddClusterJDBCURL (\ s a -> s{_rddClusterJDBCURL = a})

-- | The @COPY@ command.
rddCopyCommand :: Lens' RedshiftDestinationDescription CopyCommand
rddCopyCommand = lens _rddCopyCommand (\ s a -> s{_rddCopyCommand = a})

-- | The name of the user.
rddUsername :: Lens' RedshiftDestinationDescription Text
rddUsername = lens _rddUsername (\ s a -> s{_rddUsername = a}) . _Sensitive

-- | The Amazon S3 destination.
rddS3DestinationDescription :: Lens' RedshiftDestinationDescription S3DestinationDescription
rddS3DestinationDescription = lens _rddS3DestinationDescription (\ s a -> s{_rddS3DestinationDescription = a})

instance FromJSON RedshiftDestinationDescription
         where
        parseJSON
          = withObject "RedshiftDestinationDescription"
              (\ x ->
                 RedshiftDestinationDescription' <$>
                   (x .:? "S3BackupMode") <*>
                     (x .:? "S3BackupDescription")
                     <*> (x .:? "CloudWatchLoggingOptions")
                     <*> (x .:? "RetryOptions")
                     <*> (x .:? "ProcessingConfiguration")
                     <*> (x .: "RoleARN")
                     <*> (x .: "ClusterJDBCURL")
                     <*> (x .: "CopyCommand")
                     <*> (x .: "Username")
                     <*> (x .: "S3DestinationDescription"))

instance Hashable RedshiftDestinationDescription
         where

instance NFData RedshiftDestinationDescription where

-- | Describes an update for a destination in Amazon Redshift.
--
--
--
-- /See:/ 'redshiftDestinationUpdate' smart constructor.
data RedshiftDestinationUpdate = RedshiftDestinationUpdate'
  { _rduS3BackupMode             :: !(Maybe RedshiftS3BackupMode)
  , _rduCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _rduUsername                 :: !(Maybe (Sensitive Text))
  , _rduS3Update                 :: !(Maybe S3DestinationUpdate)
  , _rduPassword                 :: !(Maybe (Sensitive Text))
  , _rduS3BackupUpdate           :: !(Maybe S3DestinationUpdate)
  , _rduCopyCommand              :: !(Maybe CopyCommand)
  , _rduRetryOptions             :: !(Maybe RedshiftRetryOptions)
  , _rduProcessingConfiguration  :: !(Maybe ProcessingConfiguration)
  , _rduClusterJDBCURL           :: !(Maybe Text)
  , _rduRoleARN                  :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedshiftDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rduS3BackupMode' - The Amazon S3 backup mode.
--
-- * 'rduCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'rduUsername' - The name of the user.
--
-- * 'rduS3Update' - The Amazon S3 destination. The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
--
-- * 'rduPassword' - The user password.
--
-- * 'rduS3BackupUpdate' - The Amazon S3 destination for backup.
--
-- * 'rduCopyCommand' - The @COPY@ command.
--
-- * 'rduRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
--
-- * 'rduProcessingConfiguration' - The data processing configuration.
--
-- * 'rduClusterJDBCURL' - The database connection string.
--
-- * 'rduRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
redshiftDestinationUpdate
    :: RedshiftDestinationUpdate
redshiftDestinationUpdate =
  RedshiftDestinationUpdate'
    { _rduS3BackupMode = Nothing
    , _rduCloudWatchLoggingOptions = Nothing
    , _rduUsername = Nothing
    , _rduS3Update = Nothing
    , _rduPassword = Nothing
    , _rduS3BackupUpdate = Nothing
    , _rduCopyCommand = Nothing
    , _rduRetryOptions = Nothing
    , _rduProcessingConfiguration = Nothing
    , _rduClusterJDBCURL = Nothing
    , _rduRoleARN = Nothing
    }


-- | The Amazon S3 backup mode.
rduS3BackupMode :: Lens' RedshiftDestinationUpdate (Maybe RedshiftS3BackupMode)
rduS3BackupMode = lens _rduS3BackupMode (\ s a -> s{_rduS3BackupMode = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
rduCloudWatchLoggingOptions :: Lens' RedshiftDestinationUpdate (Maybe CloudWatchLoggingOptions)
rduCloudWatchLoggingOptions = lens _rduCloudWatchLoggingOptions (\ s a -> s{_rduCloudWatchLoggingOptions = a})

-- | The name of the user.
rduUsername :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduUsername = lens _rduUsername (\ s a -> s{_rduUsername = a}) . mapping _Sensitive

-- | The Amazon S3 destination. The compression formats @SNAPPY@ or @ZIP@ cannot be specified in @RedshiftDestinationUpdate.S3Update@ because the Amazon Redshift @COPY@ operation that reads from the S3 bucket doesn't support these compression formats.
rduS3Update :: Lens' RedshiftDestinationUpdate (Maybe S3DestinationUpdate)
rduS3Update = lens _rduS3Update (\ s a -> s{_rduS3Update = a})

-- | The user password.
rduPassword :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduPassword = lens _rduPassword (\ s a -> s{_rduPassword = a}) . mapping _Sensitive

-- | The Amazon S3 destination for backup.
rduS3BackupUpdate :: Lens' RedshiftDestinationUpdate (Maybe S3DestinationUpdate)
rduS3BackupUpdate = lens _rduS3BackupUpdate (\ s a -> s{_rduS3BackupUpdate = a})

-- | The @COPY@ command.
rduCopyCommand :: Lens' RedshiftDestinationUpdate (Maybe CopyCommand)
rduCopyCommand = lens _rduCopyCommand (\ s a -> s{_rduCopyCommand = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift. Default value is 3600 (60 minutes).
rduRetryOptions :: Lens' RedshiftDestinationUpdate (Maybe RedshiftRetryOptions)
rduRetryOptions = lens _rduRetryOptions (\ s a -> s{_rduRetryOptions = a})

-- | The data processing configuration.
rduProcessingConfiguration :: Lens' RedshiftDestinationUpdate (Maybe ProcessingConfiguration)
rduProcessingConfiguration = lens _rduProcessingConfiguration (\ s a -> s{_rduProcessingConfiguration = a})

-- | The database connection string.
rduClusterJDBCURL :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduClusterJDBCURL = lens _rduClusterJDBCURL (\ s a -> s{_rduClusterJDBCURL = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rduRoleARN :: Lens' RedshiftDestinationUpdate (Maybe Text)
rduRoleARN = lens _rduRoleARN (\ s a -> s{_rduRoleARN = a})

instance Hashable RedshiftDestinationUpdate where

instance NFData RedshiftDestinationUpdate where

instance ToJSON RedshiftDestinationUpdate where
        toJSON RedshiftDestinationUpdate'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _rduS3BackupMode,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _rduCloudWatchLoggingOptions,
                  ("Username" .=) <$> _rduUsername,
                  ("S3Update" .=) <$> _rduS3Update,
                  ("Password" .=) <$> _rduPassword,
                  ("S3BackupUpdate" .=) <$> _rduS3BackupUpdate,
                  ("CopyCommand" .=) <$> _rduCopyCommand,
                  ("RetryOptions" .=) <$> _rduRetryOptions,
                  ("ProcessingConfiguration" .=) <$>
                    _rduProcessingConfiguration,
                  ("ClusterJDBCURL" .=) <$> _rduClusterJDBCURL,
                  ("RoleARN" .=) <$> _rduRoleARN])

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Amazon Redshift.
--
--
--
-- /See:/ 'redshiftRetryOptions' smart constructor.
newtype RedshiftRetryOptions = RedshiftRetryOptions'
  { _rroDurationInSeconds :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedshiftRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rroDurationInSeconds' - The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
redshiftRetryOptions
    :: RedshiftRetryOptions
redshiftRetryOptions = RedshiftRetryOptions' {_rroDurationInSeconds = Nothing}


-- | The length of time during which Kinesis Data Firehose retries delivery after a failure, starting from the initial request and including the first attempt. The default value is 3600 seconds (60 minutes). Kinesis Data Firehose does not retry if the value of @DurationInSeconds@ is 0 (zero) or if the first delivery attempt takes longer than the current value.
rroDurationInSeconds :: Lens' RedshiftRetryOptions (Maybe Natural)
rroDurationInSeconds = lens _rroDurationInSeconds (\ s a -> s{_rroDurationInSeconds = a}) . mapping _Nat

instance FromJSON RedshiftRetryOptions where
        parseJSON
          = withObject "RedshiftRetryOptions"
              (\ x ->
                 RedshiftRetryOptions' <$>
                   (x .:? "DurationInSeconds"))

instance Hashable RedshiftRetryOptions where

instance NFData RedshiftRetryOptions where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 'sdcCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'sdcEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sdcCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sdcBufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- * 'sdcRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'sdcBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
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


-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
sdcPrefix :: Lens' S3DestinationConfiguration (Maybe Text)
sdcPrefix = lens _sdcPrefix (\ s a -> s{_sdcPrefix = a})

-- | The CloudWatch logging options for your delivery stream.
sdcCloudWatchLoggingOptions :: Lens' S3DestinationConfiguration (Maybe CloudWatchLoggingOptions)
sdcCloudWatchLoggingOptions = lens _sdcCloudWatchLoggingOptions (\ s a -> s{_sdcCloudWatchLoggingOptions = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
sdcEncryptionConfiguration :: Lens' S3DestinationConfiguration (Maybe EncryptionConfiguration)
sdcEncryptionConfiguration = lens _sdcEncryptionConfiguration (\ s a -> s{_sdcEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sdcCompressionFormat :: Lens' S3DestinationConfiguration (Maybe CompressionFormat)
sdcCompressionFormat = lens _sdcCompressionFormat (\ s a -> s{_sdcCompressionFormat = a})

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
sdcBufferingHints :: Lens' S3DestinationConfiguration (Maybe BufferingHints)
sdcBufferingHints = lens _sdcBufferingHints (\ s a -> s{_sdcBufferingHints = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sdcRoleARN :: Lens' S3DestinationConfiguration Text
sdcRoleARN = lens _sdcRoleARN (\ s a -> s{_sdcRoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sdcBucketARN :: Lens' S3DestinationConfiguration Text
sdcBucketARN = lens _sdcBucketARN (\ s a -> s{_sdcBucketARN = a})

instance Hashable S3DestinationConfiguration where

instance NFData S3DestinationConfiguration where

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
  { _s3Prefix                   :: !(Maybe Text)
  , _s3CloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _s3RoleARN                  :: !Text
  , _s3BucketARN                :: !Text
  , _s3BufferingHints           :: !BufferingHints
  , _s3CompressionFormat        :: !CompressionFormat
  , _s3EncryptionConfiguration  :: !EncryptionConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3DestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 's3Prefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 's3CloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 's3RoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 's3BucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 's3BufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- * 's3CompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
--
-- * 's3EncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
s3DestinationDescription
    :: Text -- ^ 's3RoleARN'
    -> Text -- ^ 's3BucketARN'
    -> BufferingHints -- ^ 's3BufferingHints'
    -> CompressionFormat -- ^ 's3CompressionFormat'
    -> EncryptionConfiguration -- ^ 's3EncryptionConfiguration'
    -> S3DestinationDescription
s3DestinationDescription pRoleARN_ pBucketARN_ pBufferingHints_ pCompressionFormat_ pEncryptionConfiguration_ =
  S3DestinationDescription'
    { _s3Prefix = Nothing
    , _s3CloudWatchLoggingOptions = Nothing
    , _s3RoleARN = pRoleARN_
    , _s3BucketARN = pBucketARN_
    , _s3BufferingHints = pBufferingHints_
    , _s3CompressionFormat = pCompressionFormat_
    , _s3EncryptionConfiguration = pEncryptionConfiguration_
    }


-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
s3Prefix :: Lens' S3DestinationDescription (Maybe Text)
s3Prefix = lens _s3Prefix (\ s a -> s{_s3Prefix = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
s3CloudWatchLoggingOptions :: Lens' S3DestinationDescription (Maybe CloudWatchLoggingOptions)
s3CloudWatchLoggingOptions = lens _s3CloudWatchLoggingOptions (\ s a -> s{_s3CloudWatchLoggingOptions = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
s3RoleARN :: Lens' S3DestinationDescription Text
s3RoleARN = lens _s3RoleARN (\ s a -> s{_s3RoleARN = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
s3BucketARN :: Lens' S3DestinationDescription Text
s3BucketARN = lens _s3BucketARN (\ s a -> s{_s3BucketARN = a})

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
s3BufferingHints :: Lens' S3DestinationDescription BufferingHints
s3BufferingHints = lens _s3BufferingHints (\ s a -> s{_s3BufferingHints = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ .
s3CompressionFormat :: Lens' S3DestinationDescription CompressionFormat
s3CompressionFormat = lens _s3CompressionFormat (\ s a -> s{_s3CompressionFormat = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
s3EncryptionConfiguration :: Lens' S3DestinationDescription EncryptionConfiguration
s3EncryptionConfiguration = lens _s3EncryptionConfiguration (\ s a -> s{_s3EncryptionConfiguration = a})

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

instance Hashable S3DestinationDescription where

instance NFData S3DestinationDescription where

-- | Describes an update for a destination in Amazon S3.
--
--
--
-- /See:/ 's3DestinationUpdate' smart constructor.
data S3DestinationUpdate = S3DestinationUpdate'
  { _sPrefix                   :: !(Maybe Text)
  , _sCloudWatchLoggingOptions :: !(Maybe CloudWatchLoggingOptions)
  , _sEncryptionConfiguration  :: !(Maybe EncryptionConfiguration)
  , _sCompressionFormat        :: !(Maybe CompressionFormat)
  , _sBufferingHints           :: !(Maybe BufferingHints)
  , _sBucketARN                :: !(Maybe Text)
  , _sRoleARN                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3DestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPrefix' - The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
--
-- * 'sCloudWatchLoggingOptions' - The CloudWatch logging options for your delivery stream.
--
-- * 'sEncryptionConfiguration' - The encryption configuration. If no value is specified, the default is no encryption.
--
-- * 'sCompressionFormat' - The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
--
-- * 'sBufferingHints' - The buffering option. If no value is specified, @BufferingHints@ object default values are used.
--
-- * 'sBucketARN' - The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'sRoleARN' - The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
s3DestinationUpdate
    :: S3DestinationUpdate
s3DestinationUpdate =
  S3DestinationUpdate'
    { _sPrefix = Nothing
    , _sCloudWatchLoggingOptions = Nothing
    , _sEncryptionConfiguration = Nothing
    , _sCompressionFormat = Nothing
    , _sBufferingHints = Nothing
    , _sBucketARN = Nothing
    , _sRoleARN = Nothing
    }


-- | The "YYYY/MM/DD/HH" time format prefix is automatically used for delivered Amazon S3 files. You can specify an extra prefix to be added in front of the time format prefix. If the prefix ends with a slash, it appears as a folder in the S3 bucket. For more information, see <http://docs.aws.amazon.com/firehose/latest/dev/basic-deliver.html#s3-object-name Amazon S3 Object Name Format> in the /Amazon Kinesis Data Firehose Developer Guide/ .
sPrefix :: Lens' S3DestinationUpdate (Maybe Text)
sPrefix = lens _sPrefix (\ s a -> s{_sPrefix = a})

-- | The CloudWatch logging options for your delivery stream.
sCloudWatchLoggingOptions :: Lens' S3DestinationUpdate (Maybe CloudWatchLoggingOptions)
sCloudWatchLoggingOptions = lens _sCloudWatchLoggingOptions (\ s a -> s{_sCloudWatchLoggingOptions = a})

-- | The encryption configuration. If no value is specified, the default is no encryption.
sEncryptionConfiguration :: Lens' S3DestinationUpdate (Maybe EncryptionConfiguration)
sEncryptionConfiguration = lens _sEncryptionConfiguration (\ s a -> s{_sEncryptionConfiguration = a})

-- | The compression format. If no value is specified, the default is @UNCOMPRESSED@ . The compression formats @SNAPPY@ or @ZIP@ cannot be specified for Amazon Redshift destinations because they are not supported by the Amazon Redshift @COPY@ operation that reads from the S3 bucket.
sCompressionFormat :: Lens' S3DestinationUpdate (Maybe CompressionFormat)
sCompressionFormat = lens _sCompressionFormat (\ s a -> s{_sCompressionFormat = a})

-- | The buffering option. If no value is specified, @BufferingHints@ object default values are used.
sBufferingHints :: Lens' S3DestinationUpdate (Maybe BufferingHints)
sBufferingHints = lens _sBufferingHints (\ s a -> s{_sBufferingHints = a})

-- | The ARN of the S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sBucketARN :: Lens' S3DestinationUpdate (Maybe Text)
sBucketARN = lens _sBucketARN (\ s a -> s{_sBucketARN = a})

-- | The Amazon Resource Name (ARN) of the AWS credentials. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
sRoleARN :: Lens' S3DestinationUpdate (Maybe Text)
sRoleARN = lens _sRoleARN (\ s a -> s{_sRoleARN = a})

instance Hashable S3DestinationUpdate where

instance NFData S3DestinationUpdate where

instance ToJSON S3DestinationUpdate where
        toJSON S3DestinationUpdate'{..}
          = object
              (catMaybes
                 [("Prefix" .=) <$> _sPrefix,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _sCloudWatchLoggingOptions,
                  ("EncryptionConfiguration" .=) <$>
                    _sEncryptionConfiguration,
                  ("CompressionFormat" .=) <$> _sCompressionFormat,
                  ("BufferingHints" .=) <$> _sBufferingHints,
                  ("BucketARN" .=) <$> _sBucketARN,
                  ("RoleARN" .=) <$> _sRoleARN])

-- | Specifies the schema to which you want Kinesis Data Firehose to configure your data before it writes it to Amazon S3.
--
--
--
-- /See:/ 'schemaConfiguration' smart constructor.
data SchemaConfiguration = SchemaConfiguration'
  { _scVersionId    :: !(Maybe Text)
  , _scCatalogId    :: !(Maybe Text)
  , _scDatabaseName :: !(Maybe Text)
  , _scRegion       :: !(Maybe Text)
  , _scTableName    :: !(Maybe Text)
  , _scRoleARN      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SchemaConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scVersionId' - Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
--
-- * 'scCatalogId' - The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
--
-- * 'scDatabaseName' - Specifies the name of the AWS Glue database that contains the schema for the output data.
--
-- * 'scRegion' - If you don't specify an AWS Region, the default is the current Region.
--
-- * 'scTableName' - Specifies the AWS Glue table that contains the column information that constitutes your data schema.
--
-- * 'scRoleARN' - The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
schemaConfiguration
    :: SchemaConfiguration
schemaConfiguration =
  SchemaConfiguration'
    { _scVersionId = Nothing
    , _scCatalogId = Nothing
    , _scDatabaseName = Nothing
    , _scRegion = Nothing
    , _scTableName = Nothing
    , _scRoleARN = Nothing
    }


-- | Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
scVersionId :: Lens' SchemaConfiguration (Maybe Text)
scVersionId = lens _scVersionId (\ s a -> s{_scVersionId = a})

-- | The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
scCatalogId :: Lens' SchemaConfiguration (Maybe Text)
scCatalogId = lens _scCatalogId (\ s a -> s{_scCatalogId = a})

-- | Specifies the name of the AWS Glue database that contains the schema for the output data.
scDatabaseName :: Lens' SchemaConfiguration (Maybe Text)
scDatabaseName = lens _scDatabaseName (\ s a -> s{_scDatabaseName = a})

-- | If you don't specify an AWS Region, the default is the current Region.
scRegion :: Lens' SchemaConfiguration (Maybe Text)
scRegion = lens _scRegion (\ s a -> s{_scRegion = a})

-- | Specifies the AWS Glue table that contains the column information that constitutes your data schema.
scTableName :: Lens' SchemaConfiguration (Maybe Text)
scTableName = lens _scTableName (\ s a -> s{_scTableName = a})

-- | The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
scRoleARN :: Lens' SchemaConfiguration (Maybe Text)
scRoleARN = lens _scRoleARN (\ s a -> s{_scRoleARN = a})

instance FromJSON SchemaConfiguration where
        parseJSON
          = withObject "SchemaConfiguration"
              (\ x ->
                 SchemaConfiguration' <$>
                   (x .:? "VersionId") <*> (x .:? "CatalogId") <*>
                     (x .:? "DatabaseName")
                     <*> (x .:? "Region")
                     <*> (x .:? "TableName")
                     <*> (x .:? "RoleARN"))

instance Hashable SchemaConfiguration where

instance NFData SchemaConfiguration where

instance ToJSON SchemaConfiguration where
        toJSON SchemaConfiguration'{..}
          = object
              (catMaybes
                 [("VersionId" .=) <$> _scVersionId,
                  ("CatalogId" .=) <$> _scCatalogId,
                  ("DatabaseName" .=) <$> _scDatabaseName,
                  ("Region" .=) <$> _scRegion,
                  ("TableName" .=) <$> _scTableName,
                  ("RoleARN" .=) <$> _scRoleARN])

-- | The serializer that you want Kinesis Data Firehose to use to convert data to the target format before writing it to Amazon S3. Kinesis Data Firehose supports two types of serializers: the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/orc/OrcSerde.html ORC SerDe> and the <https://hive.apache.org/javadocs/r1.2.2/api/org/apache/hadoop/hive/ql/io/parquet/serde/ParquetHiveSerDe.html Parquet SerDe> .
--
--
--
-- /See:/ 'serializer' smart constructor.
data Serializer = Serializer'
  { _sOrcSerDe     :: !(Maybe OrcSerDe)
  , _sParquetSerDe :: !(Maybe ParquetSerDe)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Serializer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOrcSerDe' - A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
--
-- * 'sParquetSerDe' - A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
serializer
    :: Serializer
serializer = Serializer' {_sOrcSerDe = Nothing, _sParquetSerDe = Nothing}


-- | A serializer to use for converting data to the ORC format before storing it in Amazon S3. For more information, see <https://orc.apache.org/docs/ Apache ORC> .
sOrcSerDe :: Lens' Serializer (Maybe OrcSerDe)
sOrcSerDe = lens _sOrcSerDe (\ s a -> s{_sOrcSerDe = a})

-- | A serializer to use for converting data to the Parquet format before storing it in Amazon S3. For more information, see <https://parquet.apache.org/documentation/latest/ Apache Parquet> .
sParquetSerDe :: Lens' Serializer (Maybe ParquetSerDe)
sParquetSerDe = lens _sParquetSerDe (\ s a -> s{_sParquetSerDe = a})

instance FromJSON Serializer where
        parseJSON
          = withObject "Serializer"
              (\ x ->
                 Serializer' <$>
                   (x .:? "OrcSerDe") <*> (x .:? "ParquetSerDe"))

instance Hashable Serializer where

instance NFData Serializer where

instance ToJSON Serializer where
        toJSON Serializer'{..}
          = object
              (catMaybes
                 [("OrcSerDe" .=) <$> _sOrcSerDe,
                  ("ParquetSerDe" .=) <$> _sParquetSerDe])

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
--
--
-- /See:/ 'sourceDescription' smart constructor.
newtype SourceDescription = SourceDescription'
  { _sdKinesisStreamSourceDescription :: Maybe KinesisStreamSourceDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdKinesisStreamSourceDescription' - The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
sourceDescription
    :: SourceDescription
sourceDescription =
  SourceDescription' {_sdKinesisStreamSourceDescription = Nothing}


-- | The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
sdKinesisStreamSourceDescription :: Lens' SourceDescription (Maybe KinesisStreamSourceDescription)
sdKinesisStreamSourceDescription = lens _sdKinesisStreamSourceDescription (\ s a -> s{_sdKinesisStreamSourceDescription = a})

instance FromJSON SourceDescription where
        parseJSON
          = withObject "SourceDescription"
              (\ x ->
                 SourceDescription' <$>
                   (x .:? "KinesisStreamSourceDescription"))

instance Hashable SourceDescription where

instance NFData SourceDescription where

-- | Describes the configuration of a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationConfiguration' smart constructor.
data SplunkDestinationConfiguration = SplunkDestinationConfiguration'
  { _splS3BackupMode                      :: !(Maybe SplunkS3BackupMode)
  , _splCloudWatchLoggingOptions          :: !(Maybe CloudWatchLoggingOptions)
  , _splHECAcknowledgmentTimeoutInSeconds :: !(Maybe Nat)
  , _splRetryOptions                      :: !(Maybe SplunkRetryOptions)
  , _splProcessingConfiguration           :: !(Maybe ProcessingConfiguration)
  , _splHECEndpoint                       :: !Text
  , _splHECEndpointType                   :: !HECEndpointType
  , _splHECToken                          :: !Text
  , _splS3Configuration                   :: !S3DestinationConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SplunkDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'splS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
--
-- * 'splCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'splHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'splRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'splProcessingConfiguration' - The data processing configuration.
--
-- * 'splHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'splHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'splHECToken' - This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'splS3Configuration' - The configuration for the backup Amazon S3 location.
splunkDestinationConfiguration
    :: Text -- ^ 'splHECEndpoint'
    -> HECEndpointType -- ^ 'splHECEndpointType'
    -> Text -- ^ 'splHECToken'
    -> S3DestinationConfiguration -- ^ 'splS3Configuration'
    -> SplunkDestinationConfiguration
splunkDestinationConfiguration pHECEndpoint_ pHECEndpointType_ pHECToken_ pS3Configuration_ =
  SplunkDestinationConfiguration'
    { _splS3BackupMode = Nothing
    , _splCloudWatchLoggingOptions = Nothing
    , _splHECAcknowledgmentTimeoutInSeconds = Nothing
    , _splRetryOptions = Nothing
    , _splProcessingConfiguration = Nothing
    , _splHECEndpoint = pHECEndpoint_
    , _splHECEndpointType = pHECEndpointType_
    , _splHECToken = pHECToken_
    , _splS3Configuration = pS3Configuration_
    }


-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
splS3BackupMode :: Lens' SplunkDestinationConfiguration (Maybe SplunkS3BackupMode)
splS3BackupMode = lens _splS3BackupMode (\ s a -> s{_splS3BackupMode = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
splCloudWatchLoggingOptions :: Lens' SplunkDestinationConfiguration (Maybe CloudWatchLoggingOptions)
splCloudWatchLoggingOptions = lens _splCloudWatchLoggingOptions (\ s a -> s{_splCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
splHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationConfiguration (Maybe Natural)
splHECAcknowledgmentTimeoutInSeconds = lens _splHECAcknowledgmentTimeoutInSeconds (\ s a -> s{_splHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk, or if it doesn't receive an acknowledgment of receipt from Splunk.
splRetryOptions :: Lens' SplunkDestinationConfiguration (Maybe SplunkRetryOptions)
splRetryOptions = lens _splRetryOptions (\ s a -> s{_splRetryOptions = a})

-- | The data processing configuration.
splProcessingConfiguration :: Lens' SplunkDestinationConfiguration (Maybe ProcessingConfiguration)
splProcessingConfiguration = lens _splProcessingConfiguration (\ s a -> s{_splProcessingConfiguration = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
splHECEndpoint :: Lens' SplunkDestinationConfiguration Text
splHECEndpoint = lens _splHECEndpoint (\ s a -> s{_splHECEndpoint = a})

-- | This type can be either "Raw" or "Event."
splHECEndpointType :: Lens' SplunkDestinationConfiguration HECEndpointType
splHECEndpointType = lens _splHECEndpointType (\ s a -> s{_splHECEndpointType = a})

-- | This is a GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
splHECToken :: Lens' SplunkDestinationConfiguration Text
splHECToken = lens _splHECToken (\ s a -> s{_splHECToken = a})

-- | The configuration for the backup Amazon S3 location.
splS3Configuration :: Lens' SplunkDestinationConfiguration S3DestinationConfiguration
splS3Configuration = lens _splS3Configuration (\ s a -> s{_splS3Configuration = a})

instance Hashable SplunkDestinationConfiguration
         where

instance NFData SplunkDestinationConfiguration where

instance ToJSON SplunkDestinationConfiguration where
        toJSON SplunkDestinationConfiguration'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _splS3BackupMode,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _splCloudWatchLoggingOptions,
                  ("HECAcknowledgmentTimeoutInSeconds" .=) <$>
                    _splHECAcknowledgmentTimeoutInSeconds,
                  ("RetryOptions" .=) <$> _splRetryOptions,
                  ("ProcessingConfiguration" .=) <$>
                    _splProcessingConfiguration,
                  Just ("HECEndpoint" .= _splHECEndpoint),
                  Just ("HECEndpointType" .= _splHECEndpointType),
                  Just ("HECToken" .= _splHECToken),
                  Just ("S3Configuration" .= _splS3Configuration)])

-- | Describes a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationDescription' smart constructor.
data SplunkDestinationDescription = SplunkDestinationDescription'
  { _sddS3BackupMode                      :: !(Maybe SplunkS3BackupMode)
  , _sddHECToken                          :: !(Maybe Text)
  , _sddHECEndpointType                   :: !(Maybe HECEndpointType)
  , _sddCloudWatchLoggingOptions          :: !(Maybe CloudWatchLoggingOptions)
  , _sddHECAcknowledgmentTimeoutInSeconds :: !(Maybe Nat)
  , _sddS3DestinationDescription          :: !(Maybe S3DestinationDescription)
  , _sddHECEndpoint                       :: !(Maybe Text)
  , _sddRetryOptions                      :: !(Maybe SplunkRetryOptions)
  , _sddProcessingConfiguration           :: !(Maybe ProcessingConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SplunkDestinationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sddS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
--
-- * 'sddHECToken' - A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'sddHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'sddCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'sddHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'sddS3DestinationDescription' - The Amazon S3 destination.>
--
-- * 'sddHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'sddRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'sddProcessingConfiguration' - The data processing configuration.
splunkDestinationDescription
    :: SplunkDestinationDescription
splunkDestinationDescription =
  SplunkDestinationDescription'
    { _sddS3BackupMode = Nothing
    , _sddHECToken = Nothing
    , _sddHECEndpointType = Nothing
    , _sddCloudWatchLoggingOptions = Nothing
    , _sddHECAcknowledgmentTimeoutInSeconds = Nothing
    , _sddS3DestinationDescription = Nothing
    , _sddHECEndpoint = Nothing
    , _sddRetryOptions = Nothing
    , _sddProcessingConfiguration = Nothing
    }


-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
sddS3BackupMode :: Lens' SplunkDestinationDescription (Maybe SplunkS3BackupMode)
sddS3BackupMode = lens _sddS3BackupMode (\ s a -> s{_sddS3BackupMode = a})

-- | A GUID you obtain from your Splunk cluster when you create a new HEC endpoint.
sddHECToken :: Lens' SplunkDestinationDescription (Maybe Text)
sddHECToken = lens _sddHECToken (\ s a -> s{_sddHECToken = a})

-- | This type can be either "Raw" or "Event."
sddHECEndpointType :: Lens' SplunkDestinationDescription (Maybe HECEndpointType)
sddHECEndpointType = lens _sddHECEndpointType (\ s a -> s{_sddHECEndpointType = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
sddCloudWatchLoggingOptions :: Lens' SplunkDestinationDescription (Maybe CloudWatchLoggingOptions)
sddCloudWatchLoggingOptions = lens _sddCloudWatchLoggingOptions (\ s a -> s{_sddCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends it data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
sddHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationDescription (Maybe Natural)
sddHECAcknowledgmentTimeoutInSeconds = lens _sddHECAcknowledgmentTimeoutInSeconds (\ s a -> s{_sddHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | The Amazon S3 destination.>
sddS3DestinationDescription :: Lens' SplunkDestinationDescription (Maybe S3DestinationDescription)
sddS3DestinationDescription = lens _sddS3DestinationDescription (\ s a -> s{_sddS3DestinationDescription = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
sddHECEndpoint :: Lens' SplunkDestinationDescription (Maybe Text)
sddHECEndpoint = lens _sddHECEndpoint (\ s a -> s{_sddHECEndpoint = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
sddRetryOptions :: Lens' SplunkDestinationDescription (Maybe SplunkRetryOptions)
sddRetryOptions = lens _sddRetryOptions (\ s a -> s{_sddRetryOptions = a})

-- | The data processing configuration.
sddProcessingConfiguration :: Lens' SplunkDestinationDescription (Maybe ProcessingConfiguration)
sddProcessingConfiguration = lens _sddProcessingConfiguration (\ s a -> s{_sddProcessingConfiguration = a})

instance FromJSON SplunkDestinationDescription where
        parseJSON
          = withObject "SplunkDestinationDescription"
              (\ x ->
                 SplunkDestinationDescription' <$>
                   (x .:? "S3BackupMode") <*> (x .:? "HECToken") <*>
                     (x .:? "HECEndpointType")
                     <*> (x .:? "CloudWatchLoggingOptions")
                     <*> (x .:? "HECAcknowledgmentTimeoutInSeconds")
                     <*> (x .:? "S3DestinationDescription")
                     <*> (x .:? "HECEndpoint")
                     <*> (x .:? "RetryOptions")
                     <*> (x .:? "ProcessingConfiguration"))

instance Hashable SplunkDestinationDescription where

instance NFData SplunkDestinationDescription where

-- | Describes an update for a destination in Splunk.
--
--
--
-- /See:/ 'splunkDestinationUpdate' smart constructor.
data SplunkDestinationUpdate = SplunkDestinationUpdate'
  { _sduS3BackupMode                      :: !(Maybe SplunkS3BackupMode)
  , _sduHECToken                          :: !(Maybe Text)
  , _sduHECEndpointType                   :: !(Maybe HECEndpointType)
  , _sduCloudWatchLoggingOptions          :: !(Maybe CloudWatchLoggingOptions)
  , _sduHECAcknowledgmentTimeoutInSeconds :: !(Maybe Nat)
  , _sduS3Update                          :: !(Maybe S3DestinationUpdate)
  , _sduHECEndpoint                       :: !(Maybe Text)
  , _sduRetryOptions                      :: !(Maybe SplunkRetryOptions)
  , _sduProcessingConfiguration           :: !(Maybe ProcessingConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SplunkDestinationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sduS3BackupMode' - Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
--
-- * 'sduHECToken' - A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
--
-- * 'sduHECEndpointType' - This type can be either "Raw" or "Event."
--
-- * 'sduCloudWatchLoggingOptions' - The Amazon CloudWatch logging options for your delivery stream.
--
-- * 'sduHECAcknowledgmentTimeoutInSeconds' - The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
--
-- * 'sduS3Update' - Your update to the configuration of the backup Amazon S3 location.
--
-- * 'sduHECEndpoint' - The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
--
-- * 'sduRetryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
--
-- * 'sduProcessingConfiguration' - The data processing configuration.
splunkDestinationUpdate
    :: SplunkDestinationUpdate
splunkDestinationUpdate =
  SplunkDestinationUpdate'
    { _sduS3BackupMode = Nothing
    , _sduHECToken = Nothing
    , _sduHECEndpointType = Nothing
    , _sduCloudWatchLoggingOptions = Nothing
    , _sduHECAcknowledgmentTimeoutInSeconds = Nothing
    , _sduS3Update = Nothing
    , _sduHECEndpoint = Nothing
    , _sduRetryOptions = Nothing
    , _sduProcessingConfiguration = Nothing
    }


-- | Defines how documents should be delivered to Amazon S3. When set to @FailedDocumentsOnly@ , Kinesis Data Firehose writes any data that could not be indexed to the configured Amazon S3 destination. When set to @AllDocuments@ , Kinesis Data Firehose delivers all incoming records to Amazon S3, and also writes failed documents to Amazon S3. Default value is @FailedDocumentsOnly@ .
sduS3BackupMode :: Lens' SplunkDestinationUpdate (Maybe SplunkS3BackupMode)
sduS3BackupMode = lens _sduS3BackupMode (\ s a -> s{_sduS3BackupMode = a})

-- | A GUID that you obtain from your Splunk cluster when you create a new HEC endpoint.
sduHECToken :: Lens' SplunkDestinationUpdate (Maybe Text)
sduHECToken = lens _sduHECToken (\ s a -> s{_sduHECToken = a})

-- | This type can be either "Raw" or "Event."
sduHECEndpointType :: Lens' SplunkDestinationUpdate (Maybe HECEndpointType)
sduHECEndpointType = lens _sduHECEndpointType (\ s a -> s{_sduHECEndpointType = a})

-- | The Amazon CloudWatch logging options for your delivery stream.
sduCloudWatchLoggingOptions :: Lens' SplunkDestinationUpdate (Maybe CloudWatchLoggingOptions)
sduCloudWatchLoggingOptions = lens _sduCloudWatchLoggingOptions (\ s a -> s{_sduCloudWatchLoggingOptions = a})

-- | The amount of time that Kinesis Data Firehose waits to receive an acknowledgment from Splunk after it sends data. At the end of the timeout period, Kinesis Data Firehose either tries to send the data again or considers it an error, based on your retry settings.
sduHECAcknowledgmentTimeoutInSeconds :: Lens' SplunkDestinationUpdate (Maybe Natural)
sduHECAcknowledgmentTimeoutInSeconds = lens _sduHECAcknowledgmentTimeoutInSeconds (\ s a -> s{_sduHECAcknowledgmentTimeoutInSeconds = a}) . mapping _Nat

-- | Your update to the configuration of the backup Amazon S3 location.
sduS3Update :: Lens' SplunkDestinationUpdate (Maybe S3DestinationUpdate)
sduS3Update = lens _sduS3Update (\ s a -> s{_sduS3Update = a})

-- | The HTTP Event Collector (HEC) endpoint to which Kinesis Data Firehose sends your data.
sduHECEndpoint :: Lens' SplunkDestinationUpdate (Maybe Text)
sduHECEndpoint = lens _sduHECEndpoint (\ s a -> s{_sduHECEndpoint = a})

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver data to Splunk or if it doesn't receive an acknowledgment of receipt from Splunk.
sduRetryOptions :: Lens' SplunkDestinationUpdate (Maybe SplunkRetryOptions)
sduRetryOptions = lens _sduRetryOptions (\ s a -> s{_sduRetryOptions = a})

-- | The data processing configuration.
sduProcessingConfiguration :: Lens' SplunkDestinationUpdate (Maybe ProcessingConfiguration)
sduProcessingConfiguration = lens _sduProcessingConfiguration (\ s a -> s{_sduProcessingConfiguration = a})

instance Hashable SplunkDestinationUpdate where

instance NFData SplunkDestinationUpdate where

instance ToJSON SplunkDestinationUpdate where
        toJSON SplunkDestinationUpdate'{..}
          = object
              (catMaybes
                 [("S3BackupMode" .=) <$> _sduS3BackupMode,
                  ("HECToken" .=) <$> _sduHECToken,
                  ("HECEndpointType" .=) <$> _sduHECEndpointType,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _sduCloudWatchLoggingOptions,
                  ("HECAcknowledgmentTimeoutInSeconds" .=) <$>
                    _sduHECAcknowledgmentTimeoutInSeconds,
                  ("S3Update" .=) <$> _sduS3Update,
                  ("HECEndpoint" .=) <$> _sduHECEndpoint,
                  ("RetryOptions" .=) <$> _sduRetryOptions,
                  ("ProcessingConfiguration" .=) <$>
                    _sduProcessingConfiguration])

-- | Configures retry behavior in case Kinesis Data Firehose is unable to deliver documents to Splunk, or if it doesn't receive an acknowledgment from Splunk.
--
--
--
-- /See:/ 'splunkRetryOptions' smart constructor.
newtype SplunkRetryOptions = SplunkRetryOptions'
  { _sroDurationInSeconds :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SplunkRetryOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sroDurationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
splunkRetryOptions
    :: SplunkRetryOptions
splunkRetryOptions = SplunkRetryOptions' {_sroDurationInSeconds = Nothing}


-- | The total amount of time that Kinesis Data Firehose spends on retries. This duration starts after the initial attempt to send data to Splunk fails. It doesn't include the periods during which Kinesis Data Firehose waits for acknowledgment from Splunk after each attempt.
sroDurationInSeconds :: Lens' SplunkRetryOptions (Maybe Natural)
sroDurationInSeconds = lens _sroDurationInSeconds (\ s a -> s{_sroDurationInSeconds = a}) . mapping _Nat

instance FromJSON SplunkRetryOptions where
        parseJSON
          = withObject "SplunkRetryOptions"
              (\ x ->
                 SplunkRetryOptions' <$> (x .:? "DurationInSeconds"))

instance Hashable SplunkRetryOptions where

instance NFData SplunkRetryOptions where

instance ToJSON SplunkRetryOptions where
        toJSON SplunkRetryOptions'{..}
          = object
              (catMaybes
                 [("DurationInSeconds" .=) <$> _sroDurationInSeconds])

-- | Metadata that you can assign to a delivery stream, consisting of a key-value pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - An optional string, which you can use to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
--
-- * 'tagKey' - A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | An optional string, which you can use to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue,
                  Just ("Key" .= _tagKey)])
