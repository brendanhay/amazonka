{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.Product where

import Network.AWS.KinesisAnalytics.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of the application, including the application Amazon Resource Name (ARN), status, latest version, and input and output configuration.
--
--
--
-- /See:/ 'applicationDetail' smart constructor.
data ApplicationDetail = ApplicationDetail'
  { _adApplicationDescription :: !(Maybe Text)
  , _adOutputDescriptions :: !(Maybe [OutputDescription])
  , _adCloudWatchLoggingOptionDescriptions :: !(Maybe [CloudWatchLoggingOptionDescription])
  , _adReferenceDataSourceDescriptions :: !(Maybe [ReferenceDataSourceDescription])
  , _adInputDescriptions :: !(Maybe [InputDescription])
  , _adApplicationCode :: !(Maybe Text)
  , _adCreateTimestamp :: !(Maybe POSIX)
  , _adLastUpdateTimestamp :: !(Maybe POSIX)
  , _adApplicationName :: !Text
  , _adApplicationARN :: !Text
  , _adApplicationStatus :: !ApplicationStatus
  , _adApplicationVersionId :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adApplicationDescription' - Description of the application.
--
-- * 'adOutputDescriptions' - Describes the application output configuration. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- * 'adCloudWatchLoggingOptionDescriptions' - Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- * 'adReferenceDataSourceDescriptions' - Describes reference data sources configured for the application. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'adInputDescriptions' - Describes the application input configuration. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'adApplicationCode' - Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
--
-- * 'adCreateTimestamp' - Time stamp when the application version was created.
--
-- * 'adLastUpdateTimestamp' - Time stamp when the application was last updated.
--
-- * 'adApplicationName' - Name of the application.
--
-- * 'adApplicationARN' - ARN of the application.
--
-- * 'adApplicationStatus' - Status of the application.
--
-- * 'adApplicationVersionId' - Provides the current application version.
applicationDetail
    :: Text -- ^ 'adApplicationName'
    -> Text -- ^ 'adApplicationARN'
    -> ApplicationStatus -- ^ 'adApplicationStatus'
    -> Natural -- ^ 'adApplicationVersionId'
    -> ApplicationDetail
applicationDetail pApplicationName_ pApplicationARN_ pApplicationStatus_ pApplicationVersionId_ =
  ApplicationDetail'
    { _adApplicationDescription = Nothing
    , _adOutputDescriptions = Nothing
    , _adCloudWatchLoggingOptionDescriptions = Nothing
    , _adReferenceDataSourceDescriptions = Nothing
    , _adInputDescriptions = Nothing
    , _adApplicationCode = Nothing
    , _adCreateTimestamp = Nothing
    , _adLastUpdateTimestamp = Nothing
    , _adApplicationName = pApplicationName_
    , _adApplicationARN = pApplicationARN_
    , _adApplicationStatus = pApplicationStatus_
    , _adApplicationVersionId = _Nat # pApplicationVersionId_
    }


-- | Description of the application.
adApplicationDescription :: Lens' ApplicationDetail (Maybe Text)
adApplicationDescription = lens _adApplicationDescription (\ s a -> s{_adApplicationDescription = a})

-- | Describes the application output configuration. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
adOutputDescriptions :: Lens' ApplicationDetail [OutputDescription]
adOutputDescriptions = lens _adOutputDescriptions (\ s a -> s{_adOutputDescriptions = a}) . _Default . _Coerce

-- | Describes the CloudWatch log streams that are configured to receive application messages. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
adCloudWatchLoggingOptionDescriptions :: Lens' ApplicationDetail [CloudWatchLoggingOptionDescription]
adCloudWatchLoggingOptionDescriptions = lens _adCloudWatchLoggingOptionDescriptions (\ s a -> s{_adCloudWatchLoggingOptionDescriptions = a}) . _Default . _Coerce

-- | Describes reference data sources configured for the application. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
adReferenceDataSourceDescriptions :: Lens' ApplicationDetail [ReferenceDataSourceDescription]
adReferenceDataSourceDescriptions = lens _adReferenceDataSourceDescriptions (\ s a -> s{_adReferenceDataSourceDescriptions = a}) . _Default . _Coerce

-- | Describes the application input configuration. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
adInputDescriptions :: Lens' ApplicationDetail [InputDescription]
adInputDescriptions = lens _adInputDescriptions (\ s a -> s{_adInputDescriptions = a}) . _Default . _Coerce

-- | Returns the application code that you provided to perform data analysis on any of the in-application streams in your application.
adApplicationCode :: Lens' ApplicationDetail (Maybe Text)
adApplicationCode = lens _adApplicationCode (\ s a -> s{_adApplicationCode = a})

-- | Time stamp when the application version was created.
adCreateTimestamp :: Lens' ApplicationDetail (Maybe UTCTime)
adCreateTimestamp = lens _adCreateTimestamp (\ s a -> s{_adCreateTimestamp = a}) . mapping _Time

-- | Time stamp when the application was last updated.
adLastUpdateTimestamp :: Lens' ApplicationDetail (Maybe UTCTime)
adLastUpdateTimestamp = lens _adLastUpdateTimestamp (\ s a -> s{_adLastUpdateTimestamp = a}) . mapping _Time

-- | Name of the application.
adApplicationName :: Lens' ApplicationDetail Text
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a})

-- | ARN of the application.
adApplicationARN :: Lens' ApplicationDetail Text
adApplicationARN = lens _adApplicationARN (\ s a -> s{_adApplicationARN = a})

-- | Status of the application.
adApplicationStatus :: Lens' ApplicationDetail ApplicationStatus
adApplicationStatus = lens _adApplicationStatus (\ s a -> s{_adApplicationStatus = a})

-- | Provides the current application version.
adApplicationVersionId :: Lens' ApplicationDetail Natural
adApplicationVersionId = lens _adApplicationVersionId (\ s a -> s{_adApplicationVersionId = a}) . _Nat

instance FromJSON ApplicationDetail where
        parseJSON
          = withObject "ApplicationDetail"
              (\ x ->
                 ApplicationDetail' <$>
                   (x .:? "ApplicationDescription") <*>
                     (x .:? "OutputDescriptions" .!= mempty)
                     <*>
                     (x .:? "CloudWatchLoggingOptionDescriptions" .!=
                        mempty)
                     <*>
                     (x .:? "ReferenceDataSourceDescriptions" .!= mempty)
                     <*> (x .:? "InputDescriptions" .!= mempty)
                     <*> (x .:? "ApplicationCode")
                     <*> (x .:? "CreateTimestamp")
                     <*> (x .:? "LastUpdateTimestamp")
                     <*> (x .: "ApplicationName")
                     <*> (x .: "ApplicationARN")
                     <*> (x .: "ApplicationStatus")
                     <*> (x .: "ApplicationVersionId"))

instance Hashable ApplicationDetail where

instance NFData ApplicationDetail where

-- | Provides application summary information, including the application Amazon Resource Name (ARN), name, and status.
--
--
--
-- /See:/ 'applicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { _asApplicationName   :: !Text
  , _asApplicationARN    :: !Text
  , _asApplicationStatus :: !ApplicationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asApplicationName' - Name of the application.
--
-- * 'asApplicationARN' - ARN of the application.
--
-- * 'asApplicationStatus' - Status of the application.
applicationSummary
    :: Text -- ^ 'asApplicationName'
    -> Text -- ^ 'asApplicationARN'
    -> ApplicationStatus -- ^ 'asApplicationStatus'
    -> ApplicationSummary
applicationSummary pApplicationName_ pApplicationARN_ pApplicationStatus_ =
  ApplicationSummary'
    { _asApplicationName = pApplicationName_
    , _asApplicationARN = pApplicationARN_
    , _asApplicationStatus = pApplicationStatus_
    }


-- | Name of the application.
asApplicationName :: Lens' ApplicationSummary Text
asApplicationName = lens _asApplicationName (\ s a -> s{_asApplicationName = a})

-- | ARN of the application.
asApplicationARN :: Lens' ApplicationSummary Text
asApplicationARN = lens _asApplicationARN (\ s a -> s{_asApplicationARN = a})

-- | Status of the application.
asApplicationStatus :: Lens' ApplicationSummary ApplicationStatus
asApplicationStatus = lens _asApplicationStatus (\ s a -> s{_asApplicationStatus = a})

instance FromJSON ApplicationSummary where
        parseJSON
          = withObject "ApplicationSummary"
              (\ x ->
                 ApplicationSummary' <$>
                   (x .: "ApplicationName") <*> (x .: "ApplicationARN")
                     <*> (x .: "ApplicationStatus"))

instance Hashable ApplicationSummary where

instance NFData ApplicationSummary where

-- | Describes updates to apply to an existing Amazon Kinesis Analytics application.
--
--
--
-- /See:/ 'applicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { _auReferenceDataSourceUpdates :: !(Maybe [ReferenceDataSourceUpdate])
  , _auInputUpdates :: !(Maybe [InputUpdate])
  , _auCloudWatchLoggingOptionUpdates :: !(Maybe [CloudWatchLoggingOptionUpdate])
  , _auOutputUpdates :: !(Maybe [OutputUpdate])
  , _auApplicationCodeUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auReferenceDataSourceUpdates' - Describes application reference data source updates.
--
-- * 'auInputUpdates' - Describes application input configuration updates.
--
-- * 'auCloudWatchLoggingOptionUpdates' - Describes application CloudWatch logging option updates.
--
-- * 'auOutputUpdates' - Describes application output configuration updates.
--
-- * 'auApplicationCodeUpdate' - Describes application code updates.
applicationUpdate
    :: ApplicationUpdate
applicationUpdate =
  ApplicationUpdate'
    { _auReferenceDataSourceUpdates = Nothing
    , _auInputUpdates = Nothing
    , _auCloudWatchLoggingOptionUpdates = Nothing
    , _auOutputUpdates = Nothing
    , _auApplicationCodeUpdate = Nothing
    }


-- | Describes application reference data source updates.
auReferenceDataSourceUpdates :: Lens' ApplicationUpdate [ReferenceDataSourceUpdate]
auReferenceDataSourceUpdates = lens _auReferenceDataSourceUpdates (\ s a -> s{_auReferenceDataSourceUpdates = a}) . _Default . _Coerce

-- | Describes application input configuration updates.
auInputUpdates :: Lens' ApplicationUpdate [InputUpdate]
auInputUpdates = lens _auInputUpdates (\ s a -> s{_auInputUpdates = a}) . _Default . _Coerce

-- | Describes application CloudWatch logging option updates.
auCloudWatchLoggingOptionUpdates :: Lens' ApplicationUpdate [CloudWatchLoggingOptionUpdate]
auCloudWatchLoggingOptionUpdates = lens _auCloudWatchLoggingOptionUpdates (\ s a -> s{_auCloudWatchLoggingOptionUpdates = a}) . _Default . _Coerce

-- | Describes application output configuration updates.
auOutputUpdates :: Lens' ApplicationUpdate [OutputUpdate]
auOutputUpdates = lens _auOutputUpdates (\ s a -> s{_auOutputUpdates = a}) . _Default . _Coerce

-- | Describes application code updates.
auApplicationCodeUpdate :: Lens' ApplicationUpdate (Maybe Text)
auApplicationCodeUpdate = lens _auApplicationCodeUpdate (\ s a -> s{_auApplicationCodeUpdate = a})

instance Hashable ApplicationUpdate where

instance NFData ApplicationUpdate where

instance ToJSON ApplicationUpdate where
        toJSON ApplicationUpdate'{..}
          = object
              (catMaybes
                 [("ReferenceDataSourceUpdates" .=) <$>
                    _auReferenceDataSourceUpdates,
                  ("InputUpdates" .=) <$> _auInputUpdates,
                  ("CloudWatchLoggingOptionUpdates" .=) <$>
                    _auCloudWatchLoggingOptionUpdates,
                  ("OutputUpdates" .=) <$> _auOutputUpdates,
                  ("ApplicationCodeUpdate" .=) <$>
                    _auApplicationCodeUpdate])

-- | Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the /'\n'/ as the row delimiter and a comma (",") as the column delimiter:
--
--
-- @"name1", "address1" @
--
-- @"name2, "address2"@
--
--
-- /See:/ 'csvMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { _cmpRecordRowDelimiter    :: !Text
  , _cmpRecordColumnDelimiter :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CSVMappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmpRecordRowDelimiter' - Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
--
-- * 'cmpRecordColumnDelimiter' - Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
csvMappingParameters
    :: Text -- ^ 'cmpRecordRowDelimiter'
    -> Text -- ^ 'cmpRecordColumnDelimiter'
    -> CSVMappingParameters
csvMappingParameters pRecordRowDelimiter_ pRecordColumnDelimiter_ =
  CSVMappingParameters'
    { _cmpRecordRowDelimiter = pRecordRowDelimiter_
    , _cmpRecordColumnDelimiter = pRecordColumnDelimiter_
    }


-- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
cmpRecordRowDelimiter :: Lens' CSVMappingParameters Text
cmpRecordRowDelimiter = lens _cmpRecordRowDelimiter (\ s a -> s{_cmpRecordRowDelimiter = a})

-- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
cmpRecordColumnDelimiter :: Lens' CSVMappingParameters Text
cmpRecordColumnDelimiter = lens _cmpRecordColumnDelimiter (\ s a -> s{_cmpRecordColumnDelimiter = a})

instance FromJSON CSVMappingParameters where
        parseJSON
          = withObject "CSVMappingParameters"
              (\ x ->
                 CSVMappingParameters' <$>
                   (x .: "RecordRowDelimiter") <*>
                     (x .: "RecordColumnDelimiter"))

instance Hashable CSVMappingParameters where

instance NFData CSVMappingParameters where

instance ToJSON CSVMappingParameters where
        toJSON CSVMappingParameters'{..}
          = object
              (catMaybes
                 [Just
                    ("RecordRowDelimiter" .= _cmpRecordRowDelimiter),
                  Just
                    ("RecordColumnDelimiter" .=
                       _cmpRecordColumnDelimiter)])

-- | Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.
--
--
--
-- /See:/ 'cloudWatchLoggingOption' smart constructor.
data CloudWatchLoggingOption = CloudWatchLoggingOption'
  { _cwloLogStreamARN :: !Text
  , _cwloRoleARN      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwloLogStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwloRoleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
cloudWatchLoggingOption
    :: Text -- ^ 'cwloLogStreamARN'
    -> Text -- ^ 'cwloRoleARN'
    -> CloudWatchLoggingOption
cloudWatchLoggingOption pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOption'
    {_cwloLogStreamARN = pLogStreamARN_, _cwloRoleARN = pRoleARN_}


-- | ARN of the CloudWatch log to receive application messages.
cwloLogStreamARN :: Lens' CloudWatchLoggingOption Text
cwloLogStreamARN = lens _cwloLogStreamARN (\ s a -> s{_cwloLogStreamARN = a})

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
cwloRoleARN :: Lens' CloudWatchLoggingOption Text
cwloRoleARN = lens _cwloRoleARN (\ s a -> s{_cwloRoleARN = a})

instance Hashable CloudWatchLoggingOption where

instance NFData CloudWatchLoggingOption where

instance ToJSON CloudWatchLoggingOption where
        toJSON CloudWatchLoggingOption'{..}
          = object
              (catMaybes
                 [Just ("LogStreamARN" .= _cwloLogStreamARN),
                  Just ("RoleARN" .= _cwloRoleARN)])

-- | Description of the CloudWatch logging option.
--
--
--
-- /See:/ 'cloudWatchLoggingOptionDescription' smart constructor.
data CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription'
  { _cwlodCloudWatchLoggingOptionId :: !(Maybe Text)
  , _cwlodLogStreamARN              :: !Text
  , _cwlodRoleARN                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLoggingOptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlodCloudWatchLoggingOptionId' - ID of the CloudWatch logging option description.
--
-- * 'cwlodLogStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwlodRoleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cloudWatchLoggingOptionDescription
    :: Text -- ^ 'cwlodLogStreamARN'
    -> Text -- ^ 'cwlodRoleARN'
    -> CloudWatchLoggingOptionDescription
cloudWatchLoggingOptionDescription pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOptionDescription'
    { _cwlodCloudWatchLoggingOptionId = Nothing
    , _cwlodLogStreamARN = pLogStreamARN_
    , _cwlodRoleARN = pRoleARN_
    }


-- | ID of the CloudWatch logging option description.
cwlodCloudWatchLoggingOptionId :: Lens' CloudWatchLoggingOptionDescription (Maybe Text)
cwlodCloudWatchLoggingOptionId = lens _cwlodCloudWatchLoggingOptionId (\ s a -> s{_cwlodCloudWatchLoggingOptionId = a})

-- | ARN of the CloudWatch log to receive application messages.
cwlodLogStreamARN :: Lens' CloudWatchLoggingOptionDescription Text
cwlodLogStreamARN = lens _cwlodLogStreamARN (\ s a -> s{_cwlodLogStreamARN = a})

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cwlodRoleARN :: Lens' CloudWatchLoggingOptionDescription Text
cwlodRoleARN = lens _cwlodRoleARN (\ s a -> s{_cwlodRoleARN = a})

instance FromJSON CloudWatchLoggingOptionDescription
         where
        parseJSON
          = withObject "CloudWatchLoggingOptionDescription"
              (\ x ->
                 CloudWatchLoggingOptionDescription' <$>
                   (x .:? "CloudWatchLoggingOptionId") <*>
                     (x .: "LogStreamARN")
                     <*> (x .: "RoleARN"))

instance Hashable CloudWatchLoggingOptionDescription
         where

instance NFData CloudWatchLoggingOptionDescription
         where

-- | Describes CloudWatch logging option updates.
--
--
--
-- /See:/ 'cloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { _cwlouRoleARNUpdate             :: !(Maybe Text)
  , _cwlouLogStreamARNUpdate        :: !(Maybe Text)
  , _cwlouCloudWatchLoggingOptionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLoggingOptionUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlouRoleARNUpdate' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- * 'cwlouLogStreamARNUpdate' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwlouCloudWatchLoggingOptionId' - ID of the CloudWatch logging option to update
cloudWatchLoggingOptionUpdate
    :: Text -- ^ 'cwlouCloudWatchLoggingOptionId'
    -> CloudWatchLoggingOptionUpdate
cloudWatchLoggingOptionUpdate pCloudWatchLoggingOptionId_ =
  CloudWatchLoggingOptionUpdate'
    { _cwlouRoleARNUpdate = Nothing
    , _cwlouLogStreamARNUpdate = Nothing
    , _cwlouCloudWatchLoggingOptionId = pCloudWatchLoggingOptionId_
    }


-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cwlouRoleARNUpdate :: Lens' CloudWatchLoggingOptionUpdate (Maybe Text)
cwlouRoleARNUpdate = lens _cwlouRoleARNUpdate (\ s a -> s{_cwlouRoleARNUpdate = a})

-- | ARN of the CloudWatch log to receive application messages.
cwlouLogStreamARNUpdate :: Lens' CloudWatchLoggingOptionUpdate (Maybe Text)
cwlouLogStreamARNUpdate = lens _cwlouLogStreamARNUpdate (\ s a -> s{_cwlouLogStreamARNUpdate = a})

-- | ID of the CloudWatch logging option to update
cwlouCloudWatchLoggingOptionId :: Lens' CloudWatchLoggingOptionUpdate Text
cwlouCloudWatchLoggingOptionId = lens _cwlouCloudWatchLoggingOptionId (\ s a -> s{_cwlouCloudWatchLoggingOptionId = a})

instance Hashable CloudWatchLoggingOptionUpdate where

instance NFData CloudWatchLoggingOptionUpdate where

instance ToJSON CloudWatchLoggingOptionUpdate where
        toJSON CloudWatchLoggingOptionUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _cwlouRoleARNUpdate,
                  ("LogStreamARNUpdate" .=) <$>
                    _cwlouLogStreamARNUpdate,
                  Just
                    ("CloudWatchLoggingOptionId" .=
                       _cwlouCloudWatchLoggingOptionId)])

-- | Describes the data format when records are written to the destination. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
--
--
-- /See:/ 'destinationSchema' smart constructor.
newtype DestinationSchema = DestinationSchema'
  { _dsRecordFormatType :: Maybe RecordFormatType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DestinationSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsRecordFormatType' - Specifies the format of the records on the output stream.
destinationSchema
    :: DestinationSchema
destinationSchema = DestinationSchema' {_dsRecordFormatType = Nothing}


-- | Specifies the format of the records on the output stream.
dsRecordFormatType :: Lens' DestinationSchema (Maybe RecordFormatType)
dsRecordFormatType = lens _dsRecordFormatType (\ s a -> s{_dsRecordFormatType = a})

instance FromJSON DestinationSchema where
        parseJSON
          = withObject "DestinationSchema"
              (\ x ->
                 DestinationSchema' <$> (x .:? "RecordFormatType"))

instance Hashable DestinationSchema where

instance NFData DestinationSchema where

instance ToJSON DestinationSchema where
        toJSON DestinationSchema'{..}
          = object
              (catMaybes
                 [("RecordFormatType" .=) <$> _dsRecordFormatType])

-- | When you configure the application input, you specify the streaming source, the in-application stream name that is created, and the mapping between the two. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'input' smart constructor.
data Input = Input'
  { _iInputParallelism             :: !(Maybe InputParallelism)
  , _iInputProcessingConfiguration :: !(Maybe InputProcessingConfiguration)
  , _iKinesisStreamsInput          :: !(Maybe KinesisStreamsInput)
  , _iKinesisFirehoseInput         :: !(Maybe KinesisFirehoseInput)
  , _iNamePrefix                   :: !Text
  , _iInputSchema                  :: !SourceSchema
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInputParallelism' - Describes the number of in-application streams to create.  Data from your source is routed to these in-application input streams. (see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
-- * 'iInputProcessingConfiguration' - The 'InputProcessingConfiguration' for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is 'InputLambdaProcessor' .
--
-- * 'iKinesisStreamsInput' - If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- * 'iKinesisFirehoseInput' - If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
--
-- * 'iNamePrefix' - Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
--
-- * 'iInputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. Also used to describe the format of the reference data source.
input
    :: Text -- ^ 'iNamePrefix'
    -> SourceSchema -- ^ 'iInputSchema'
    -> Input
input pNamePrefix_ pInputSchema_ =
  Input'
    { _iInputParallelism = Nothing
    , _iInputProcessingConfiguration = Nothing
    , _iKinesisStreamsInput = Nothing
    , _iKinesisFirehoseInput = Nothing
    , _iNamePrefix = pNamePrefix_
    , _iInputSchema = pInputSchema_
    }


-- | Describes the number of in-application streams to create.  Data from your source is routed to these in-application input streams. (see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
iInputParallelism :: Lens' Input (Maybe InputParallelism)
iInputParallelism = lens _iInputParallelism (\ s a -> s{_iInputParallelism = a})

-- | The 'InputProcessingConfiguration' for the input. An input processor transforms records as they are received from the stream, before the application's SQL code executes. Currently, the only input processing configuration available is 'InputLambdaProcessor' .
iInputProcessingConfiguration :: Lens' Input (Maybe InputProcessingConfiguration)
iInputProcessingConfiguration = lens _iInputProcessingConfiguration (\ s a -> s{_iInputProcessingConfiguration = a})

-- | If the streaming source is an Amazon Kinesis stream, identifies the stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
iKinesisStreamsInput :: Lens' Input (Maybe KinesisStreamsInput)
iKinesisStreamsInput = lens _iKinesisStreamsInput (\ s a -> s{_iKinesisStreamsInput = a})

-- | If the streaming source is an Amazon Kinesis Firehose delivery stream, identifies the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf. Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is required.
iKinesisFirehoseInput :: Lens' Input (Maybe KinesisFirehoseInput)
iKinesisFirehoseInput = lens _iKinesisFirehoseInput (\ s a -> s{_iKinesisFirehoseInput = a})

-- | Name prefix to use when creating an in-application stream. Suppose that you specify a prefix "MyInApplicationStream." Amazon Kinesis Analytics then creates one or more (as per the @InputParallelism@ count you specified) in-application streams with names "MyInApplicationStream_001," "MyInApplicationStream_002," and so on.
iNamePrefix :: Lens' Input Text
iNamePrefix = lens _iNamePrefix (\ s a -> s{_iNamePrefix = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created. Also used to describe the format of the reference data source.
iInputSchema :: Lens' Input SourceSchema
iInputSchema = lens _iInputSchema (\ s a -> s{_iInputSchema = a})

instance Hashable Input where

instance NFData Input where

instance ToJSON Input where
        toJSON Input'{..}
          = object
              (catMaybes
                 [("InputParallelism" .=) <$> _iInputParallelism,
                  ("InputProcessingConfiguration" .=) <$>
                    _iInputProcessingConfiguration,
                  ("KinesisStreamsInput" .=) <$> _iKinesisStreamsInput,
                  ("KinesisFirehoseInput" .=) <$>
                    _iKinesisFirehoseInput,
                  Just ("NamePrefix" .= _iNamePrefix),
                  Just ("InputSchema" .= _iInputSchema)])

-- | When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.
--
--
--
-- /See:/ 'inputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { _icId                                 :: !Text
  , _icInputStartingPositionConfiguration :: !InputStartingPositionConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icId' - Input source ID. You can get this ID by calling the 'DescribeApplication' operation.
--
-- * 'icInputStartingPositionConfiguration' - Point at which you want the application to start processing records from the streaming source.
inputConfiguration
    :: Text -- ^ 'icId'
    -> InputStartingPositionConfiguration -- ^ 'icInputStartingPositionConfiguration'
    -> InputConfiguration
inputConfiguration pId_ pInputStartingPositionConfiguration_ =
  InputConfiguration'
    { _icId = pId_
    , _icInputStartingPositionConfiguration =
        pInputStartingPositionConfiguration_
    }


-- | Input source ID. You can get this ID by calling the 'DescribeApplication' operation.
icId :: Lens' InputConfiguration Text
icId = lens _icId (\ s a -> s{_icId = a})

-- | Point at which you want the application to start processing records from the streaming source.
icInputStartingPositionConfiguration :: Lens' InputConfiguration InputStartingPositionConfiguration
icInputStartingPositionConfiguration = lens _icInputStartingPositionConfiguration (\ s a -> s{_icInputStartingPositionConfiguration = a})

instance Hashable InputConfiguration where

instance NFData InputConfiguration where

instance ToJSON InputConfiguration where
        toJSON InputConfiguration'{..}
          = object
              (catMaybes
                 [Just ("Id" .= _icId),
                  Just
                    ("InputStartingPositionConfiguration" .=
                       _icInputStartingPositionConfiguration)])

-- | Describes the application input configuration. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'inputDescription' smart constructor.
data InputDescription = InputDescription'
  { _idInputStartingPositionConfiguration :: !(Maybe InputStartingPositionConfiguration)
  , _idInputParallelism :: !(Maybe InputParallelism)
  , _idInputId :: !(Maybe Text)
  , _idInAppStreamNames :: !(Maybe [Text])
  , _idKinesisFirehoseInputDescription :: !(Maybe KinesisFirehoseInputDescription)
  , _idInputSchema :: !(Maybe SourceSchema)
  , _idKinesisStreamsInputDescription :: !(Maybe KinesisStreamsInputDescription)
  , _idNamePrefix :: !(Maybe Text)
  , _idInputProcessingConfigurationDescription :: !(Maybe InputProcessingConfigurationDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idInputStartingPositionConfiguration' - Point at which the application is configured to read from the input stream.
--
-- * 'idInputParallelism' - Describes the configured parallelism (number of in-application streams mapped to the streaming source).
--
-- * 'idInputId' - Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
--
-- * 'idInAppStreamNames' - Returns the in-application stream names that are mapped to the stream source.
--
-- * 'idKinesisFirehoseInputDescription' - If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- * 'idInputSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
--
-- * 'idKinesisStreamsInputDescription' - If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- * 'idNamePrefix' - In-application name prefix.
--
-- * 'idInputProcessingConfigurationDescription' - The description of the preprocessor that executes on records in this input before the application's code is run.
inputDescription
    :: InputDescription
inputDescription =
  InputDescription'
    { _idInputStartingPositionConfiguration = Nothing
    , _idInputParallelism = Nothing
    , _idInputId = Nothing
    , _idInAppStreamNames = Nothing
    , _idKinesisFirehoseInputDescription = Nothing
    , _idInputSchema = Nothing
    , _idKinesisStreamsInputDescription = Nothing
    , _idNamePrefix = Nothing
    , _idInputProcessingConfigurationDescription = Nothing
    }


-- | Point at which the application is configured to read from the input stream.
idInputStartingPositionConfiguration :: Lens' InputDescription (Maybe InputStartingPositionConfiguration)
idInputStartingPositionConfiguration = lens _idInputStartingPositionConfiguration (\ s a -> s{_idInputStartingPositionConfiguration = a})

-- | Describes the configured parallelism (number of in-application streams mapped to the streaming source).
idInputParallelism :: Lens' InputDescription (Maybe InputParallelism)
idInputParallelism = lens _idInputParallelism (\ s a -> s{_idInputParallelism = a})

-- | Input ID associated with the application input. This is the ID that Amazon Kinesis Analytics assigns to each input configuration you add to your application.
idInputId :: Lens' InputDescription (Maybe Text)
idInputId = lens _idInputId (\ s a -> s{_idInputId = a})

-- | Returns the in-application stream names that are mapped to the stream source.
idInAppStreamNames :: Lens' InputDescription [Text]
idInAppStreamNames = lens _idInAppStreamNames (\ s a -> s{_idInAppStreamNames = a}) . _Default . _Coerce

-- | If an Amazon Kinesis Firehose delivery stream is configured as a streaming source, provides the delivery stream's ARN and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
idKinesisFirehoseInputDescription :: Lens' InputDescription (Maybe KinesisFirehoseInputDescription)
idKinesisFirehoseInputDescription = lens _idKinesisFirehoseInputDescription (\ s a -> s{_idKinesisFirehoseInputDescription = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns in the in-application stream that is being created.
idInputSchema :: Lens' InputDescription (Maybe SourceSchema)
idInputSchema = lens _idInputSchema (\ s a -> s{_idInputSchema = a})

-- | If an Amazon Kinesis stream is configured as streaming source, provides Amazon Kinesis stream's Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to access the stream on your behalf.
idKinesisStreamsInputDescription :: Lens' InputDescription (Maybe KinesisStreamsInputDescription)
idKinesisStreamsInputDescription = lens _idKinesisStreamsInputDescription (\ s a -> s{_idKinesisStreamsInputDescription = a})

-- | In-application name prefix.
idNamePrefix :: Lens' InputDescription (Maybe Text)
idNamePrefix = lens _idNamePrefix (\ s a -> s{_idNamePrefix = a})

-- | The description of the preprocessor that executes on records in this input before the application's code is run.
idInputProcessingConfigurationDescription :: Lens' InputDescription (Maybe InputProcessingConfigurationDescription)
idInputProcessingConfigurationDescription = lens _idInputProcessingConfigurationDescription (\ s a -> s{_idInputProcessingConfigurationDescription = a})

instance FromJSON InputDescription where
        parseJSON
          = withObject "InputDescription"
              (\ x ->
                 InputDescription' <$>
                   (x .:? "InputStartingPositionConfiguration") <*>
                     (x .:? "InputParallelism")
                     <*> (x .:? "InputId")
                     <*> (x .:? "InAppStreamNames" .!= mempty)
                     <*> (x .:? "KinesisFirehoseInputDescription")
                     <*> (x .:? "InputSchema")
                     <*> (x .:? "KinesisStreamsInputDescription")
                     <*> (x .:? "NamePrefix")
                     <*>
                     (x .:? "InputProcessingConfigurationDescription"))

instance Hashable InputDescription where

instance NFData InputDescription where

-- | An object that contains the Amazon Resource Name (ARN) of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function.
--
--
--
-- /See:/ 'inputLambdaProcessor' smart constructor.
data InputLambdaProcessor = InputLambdaProcessor'
  { _ilpResourceARN :: !Text
  , _ilpRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputLambdaProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpResourceARN' - The ARN of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that operates on records in the stream.
--
-- * 'ilpRoleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessor
    :: Text -- ^ 'ilpResourceARN'
    -> Text -- ^ 'ilpRoleARN'
    -> InputLambdaProcessor
inputLambdaProcessor pResourceARN_ pRoleARN_ =
  InputLambdaProcessor'
    {_ilpResourceARN = pResourceARN_, _ilpRoleARN = pRoleARN_}


-- | The ARN of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that operates on records in the stream.
ilpResourceARN :: Lens' InputLambdaProcessor Text
ilpResourceARN = lens _ilpResourceARN (\ s a -> s{_ilpResourceARN = a})

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
ilpRoleARN :: Lens' InputLambdaProcessor Text
ilpRoleARN = lens _ilpRoleARN (\ s a -> s{_ilpRoleARN = a})

instance Hashable InputLambdaProcessor where

instance NFData InputLambdaProcessor where

instance ToJSON InputLambdaProcessor where
        toJSON InputLambdaProcessor'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _ilpResourceARN),
                  Just ("RoleARN" .= _ilpRoleARN)])

-- | An object that contains the Amazon Resource Name (ARN) of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.
--
--
--
-- /See:/ 'inputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { _ilpdResourceARN :: !(Maybe Text)
  , _ilpdRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputLambdaProcessorDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpdResourceARN' - The ARN of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- * 'ilpdRoleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessorDescription
    :: InputLambdaProcessorDescription
inputLambdaProcessorDescription =
  InputLambdaProcessorDescription'
    {_ilpdResourceARN = Nothing, _ilpdRoleARN = Nothing}


-- | The ARN of the <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
ilpdResourceARN :: Lens' InputLambdaProcessorDescription (Maybe Text)
ilpdResourceARN = lens _ilpdResourceARN (\ s a -> s{_ilpdResourceARN = a})

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
ilpdRoleARN :: Lens' InputLambdaProcessorDescription (Maybe Text)
ilpdRoleARN = lens _ilpdRoleARN (\ s a -> s{_ilpdRoleARN = a})

instance FromJSON InputLambdaProcessorDescription
         where
        parseJSON
          = withObject "InputLambdaProcessorDescription"
              (\ x ->
                 InputLambdaProcessorDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable InputLambdaProcessorDescription
         where

instance NFData InputLambdaProcessorDescription where

-- | Represents an update to the 'InputLambdaProcessor' that is used to preprocess the records in the stream.
--
--
--
-- /See:/ 'inputLambdaProcessorUpdate' smart constructor.
data InputLambdaProcessorUpdate = InputLambdaProcessorUpdate'
  { _ilpuRoleARNUpdate     :: !(Maybe Text)
  , _ilpuResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputLambdaProcessorUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpuRoleARNUpdate' - The ARN of the new IAM role that is used to access the AWS Lambda function.
--
-- * 'ilpuResourceARNUpdate' - The Amazon Resource Name (ARN) of the new <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
inputLambdaProcessorUpdate
    :: InputLambdaProcessorUpdate
inputLambdaProcessorUpdate =
  InputLambdaProcessorUpdate'
    {_ilpuRoleARNUpdate = Nothing, _ilpuResourceARNUpdate = Nothing}


-- | The ARN of the new IAM role that is used to access the AWS Lambda function.
ilpuRoleARNUpdate :: Lens' InputLambdaProcessorUpdate (Maybe Text)
ilpuRoleARNUpdate = lens _ilpuRoleARNUpdate (\ s a -> s{_ilpuRoleARNUpdate = a})

-- | The Amazon Resource Name (ARN) of the new <https://aws.amazon.com/documentation/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
ilpuResourceARNUpdate :: Lens' InputLambdaProcessorUpdate (Maybe Text)
ilpuResourceARNUpdate = lens _ilpuResourceARNUpdate (\ s a -> s{_ilpuResourceARNUpdate = a})

instance Hashable InputLambdaProcessorUpdate where

instance NFData InputLambdaProcessorUpdate where

instance ToJSON InputLambdaProcessorUpdate where
        toJSON InputLambdaProcessorUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _ilpuRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _ilpuResourceARNUpdate])

-- | Describes the number of in-application streams to create for a given streaming source. For information about parallelism, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
--
-- /See:/ 'inputParallelism' smart constructor.
newtype InputParallelism = InputParallelism'
  { _ipCount :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputParallelism' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipCount' - Number of in-application streams to create. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
inputParallelism
    :: InputParallelism
inputParallelism = InputParallelism' {_ipCount = Nothing}


-- | Number of in-application streams to create. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
ipCount :: Lens' InputParallelism (Maybe Natural)
ipCount = lens _ipCount (\ s a -> s{_ipCount = a}) . mapping _Nat

instance FromJSON InputParallelism where
        parseJSON
          = withObject "InputParallelism"
              (\ x -> InputParallelism' <$> (x .:? "Count"))

instance Hashable InputParallelism where

instance NFData InputParallelism where

instance ToJSON InputParallelism where
        toJSON InputParallelism'{..}
          = object (catMaybes [("Count" .=) <$> _ipCount])

-- | Provides updates to the parallelism count.
--
--
--
-- /See:/ 'inputParallelismUpdate' smart constructor.
newtype InputParallelismUpdate = InputParallelismUpdate'
  { _ipuCountUpdate :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputParallelismUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipuCountUpdate' - Number of in-application streams to create for the specified streaming source.
inputParallelismUpdate
    :: InputParallelismUpdate
inputParallelismUpdate = InputParallelismUpdate' {_ipuCountUpdate = Nothing}


-- | Number of in-application streams to create for the specified streaming source.
ipuCountUpdate :: Lens' InputParallelismUpdate (Maybe Natural)
ipuCountUpdate = lens _ipuCountUpdate (\ s a -> s{_ipuCountUpdate = a}) . mapping _Nat

instance Hashable InputParallelismUpdate where

instance NFData InputParallelismUpdate where

instance ToJSON InputParallelismUpdate where
        toJSON InputParallelismUpdate'{..}
          = object
              (catMaybes [("CountUpdate" .=) <$> _ipuCountUpdate])

-- | Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <https://aws.amazon.com/documentation/lambda/ AWS Lambda> .
--
--
--
-- /See:/ 'inputProcessingConfiguration' smart constructor.
newtype InputProcessingConfiguration = InputProcessingConfiguration'
  { _ipcInputLambdaProcessor :: InputLambdaProcessor
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcInputLambdaProcessor' - The 'InputLambdaProcessor' that is used to preprocess the records in the stream before being processed by your application code.
inputProcessingConfiguration
    :: InputLambdaProcessor -- ^ 'ipcInputLambdaProcessor'
    -> InputProcessingConfiguration
inputProcessingConfiguration pInputLambdaProcessor_ =
  InputProcessingConfiguration'
    {_ipcInputLambdaProcessor = pInputLambdaProcessor_}


-- | The 'InputLambdaProcessor' that is used to preprocess the records in the stream before being processed by your application code.
ipcInputLambdaProcessor :: Lens' InputProcessingConfiguration InputLambdaProcessor
ipcInputLambdaProcessor = lens _ipcInputLambdaProcessor (\ s a -> s{_ipcInputLambdaProcessor = a})

instance Hashable InputProcessingConfiguration where

instance NFData InputProcessingConfiguration where

instance ToJSON InputProcessingConfiguration where
        toJSON InputProcessingConfiguration'{..}
          = object
              (catMaybes
                 [Just
                    ("InputLambdaProcessor" .=
                       _ipcInputLambdaProcessor)])

-- | Provides configuration information about an input processor. Currently, the only input processor available is <https://aws.amazon.com/documentation/lambda/ AWS Lambda> .
--
--
--
-- /See:/ 'inputProcessingConfigurationDescription' smart constructor.
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription'
  { _ipcdInputLambdaProcessorDescription :: Maybe InputLambdaProcessorDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputProcessingConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcdInputLambdaProcessorDescription' - Provides configuration information about the associated 'InputLambdaProcessorDescription' .
inputProcessingConfigurationDescription
    :: InputProcessingConfigurationDescription
inputProcessingConfigurationDescription =
  InputProcessingConfigurationDescription'
    {_ipcdInputLambdaProcessorDescription = Nothing}


-- | Provides configuration information about the associated 'InputLambdaProcessorDescription' .
ipcdInputLambdaProcessorDescription :: Lens' InputProcessingConfigurationDescription (Maybe InputLambdaProcessorDescription)
ipcdInputLambdaProcessorDescription = lens _ipcdInputLambdaProcessorDescription (\ s a -> s{_ipcdInputLambdaProcessorDescription = a})

instance FromJSON
           InputProcessingConfigurationDescription
         where
        parseJSON
          = withObject
              "InputProcessingConfigurationDescription"
              (\ x ->
                 InputProcessingConfigurationDescription' <$>
                   (x .:? "InputLambdaProcessorDescription"))

instance Hashable
           InputProcessingConfigurationDescription
         where

instance NFData
           InputProcessingConfigurationDescription
         where

-- | Describes updates to an 'InputProcessingConfiguration' .
--
--
--
-- /See:/ 'inputProcessingConfigurationUpdate' smart constructor.
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate'
  { _ipcuInputLambdaProcessorUpdate :: InputLambdaProcessorUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputProcessingConfigurationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcuInputLambdaProcessorUpdate' - Provides update information for an 'InputLambdaProcessor' .
inputProcessingConfigurationUpdate
    :: InputLambdaProcessorUpdate -- ^ 'ipcuInputLambdaProcessorUpdate'
    -> InputProcessingConfigurationUpdate
inputProcessingConfigurationUpdate pInputLambdaProcessorUpdate_ =
  InputProcessingConfigurationUpdate'
    {_ipcuInputLambdaProcessorUpdate = pInputLambdaProcessorUpdate_}


-- | Provides update information for an 'InputLambdaProcessor' .
ipcuInputLambdaProcessorUpdate :: Lens' InputProcessingConfigurationUpdate InputLambdaProcessorUpdate
ipcuInputLambdaProcessorUpdate = lens _ipcuInputLambdaProcessorUpdate (\ s a -> s{_ipcuInputLambdaProcessorUpdate = a})

instance Hashable InputProcessingConfigurationUpdate
         where

instance NFData InputProcessingConfigurationUpdate
         where

instance ToJSON InputProcessingConfigurationUpdate
         where
        toJSON InputProcessingConfigurationUpdate'{..}
          = object
              (catMaybes
                 [Just
                    ("InputLambdaProcessorUpdate" .=
                       _ipcuInputLambdaProcessorUpdate)])

-- | Describes updates for the application's input schema.
--
--
--
-- /See:/ 'inputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { _isuRecordFormatUpdate   :: !(Maybe RecordFormat)
  , _isuRecordEncodingUpdate :: !(Maybe Text)
  , _isuRecordColumnUpdates  :: !(Maybe (List1 RecordColumn))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputSchemaUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isuRecordFormatUpdate' - Specifies the format of the records on the streaming source.
--
-- * 'isuRecordEncodingUpdate' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- * 'isuRecordColumnUpdates' - A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
inputSchemaUpdate
    :: InputSchemaUpdate
inputSchemaUpdate =
  InputSchemaUpdate'
    { _isuRecordFormatUpdate = Nothing
    , _isuRecordEncodingUpdate = Nothing
    , _isuRecordColumnUpdates = Nothing
    }


-- | Specifies the format of the records on the streaming source.
isuRecordFormatUpdate :: Lens' InputSchemaUpdate (Maybe RecordFormat)
isuRecordFormatUpdate = lens _isuRecordFormatUpdate (\ s a -> s{_isuRecordFormatUpdate = a})

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
isuRecordEncodingUpdate :: Lens' InputSchemaUpdate (Maybe Text)
isuRecordEncodingUpdate = lens _isuRecordEncodingUpdate (\ s a -> s{_isuRecordEncodingUpdate = a})

-- | A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
isuRecordColumnUpdates :: Lens' InputSchemaUpdate (Maybe (NonEmpty RecordColumn))
isuRecordColumnUpdates = lens _isuRecordColumnUpdates (\ s a -> s{_isuRecordColumnUpdates = a}) . mapping _List1

instance Hashable InputSchemaUpdate where

instance NFData InputSchemaUpdate where

instance ToJSON InputSchemaUpdate where
        toJSON InputSchemaUpdate'{..}
          = object
              (catMaybes
                 [("RecordFormatUpdate" .=) <$>
                    _isuRecordFormatUpdate,
                  ("RecordEncodingUpdate" .=) <$>
                    _isuRecordEncodingUpdate,
                  ("RecordColumnUpdates" .=) <$>
                    _isuRecordColumnUpdates])

-- | Describes the point at which the application reads from the streaming source.
--
--
--
-- /See:/ 'inputStartingPositionConfiguration' smart constructor.
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration'
  { _ispcInputStartingPosition :: Maybe InputStartingPosition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputStartingPositionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ispcInputStartingPosition' - The starting position on the stream.     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
inputStartingPositionConfiguration
    :: InputStartingPositionConfiguration
inputStartingPositionConfiguration =
  InputStartingPositionConfiguration' {_ispcInputStartingPosition = Nothing}


-- | The starting position on the stream.     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
ispcInputStartingPosition :: Lens' InputStartingPositionConfiguration (Maybe InputStartingPosition)
ispcInputStartingPosition = lens _ispcInputStartingPosition (\ s a -> s{_ispcInputStartingPosition = a})

instance FromJSON InputStartingPositionConfiguration
         where
        parseJSON
          = withObject "InputStartingPositionConfiguration"
              (\ x ->
                 InputStartingPositionConfiguration' <$>
                   (x .:? "InputStartingPosition"))

instance Hashable InputStartingPositionConfiguration
         where

instance NFData InputStartingPositionConfiguration
         where

instance ToJSON InputStartingPositionConfiguration
         where
        toJSON InputStartingPositionConfiguration'{..}
          = object
              (catMaybes
                 [("InputStartingPosition" .=) <$>
                    _ispcInputStartingPosition])

-- | Describes updates to a specific input configuration (identified by the @InputId@ of an application).
--
--
--
-- /See:/ 'inputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { _iuInputProcessingConfigurationUpdate :: !(Maybe InputProcessingConfigurationUpdate)
  , _iuKinesisStreamsInputUpdate :: !(Maybe KinesisStreamsInputUpdate)
  , _iuInputParallelismUpdate :: !(Maybe InputParallelismUpdate)
  , _iuNamePrefixUpdate :: !(Maybe Text)
  , _iuInputSchemaUpdate :: !(Maybe InputSchemaUpdate)
  , _iuKinesisFirehoseInputUpdate :: !(Maybe KinesisFirehoseInputUpdate)
  , _iuInputId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuInputProcessingConfigurationUpdate' - Describes updates for an input processing configuration.
--
-- * 'iuKinesisStreamsInputUpdate' - If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
--
-- * 'iuInputParallelismUpdate' - Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
--
-- * 'iuNamePrefixUpdate' - Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
--
-- * 'iuInputSchemaUpdate' - Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
--
-- * 'iuKinesisFirehoseInputUpdate' - If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
--
-- * 'iuInputId' - Input ID of the application input to be updated.
inputUpdate
    :: Text -- ^ 'iuInputId'
    -> InputUpdate
inputUpdate pInputId_ =
  InputUpdate'
    { _iuInputProcessingConfigurationUpdate = Nothing
    , _iuKinesisStreamsInputUpdate = Nothing
    , _iuInputParallelismUpdate = Nothing
    , _iuNamePrefixUpdate = Nothing
    , _iuInputSchemaUpdate = Nothing
    , _iuKinesisFirehoseInputUpdate = Nothing
    , _iuInputId = pInputId_
    }


-- | Describes updates for an input processing configuration.
iuInputProcessingConfigurationUpdate :: Lens' InputUpdate (Maybe InputProcessingConfigurationUpdate)
iuInputProcessingConfigurationUpdate = lens _iuInputProcessingConfigurationUpdate (\ s a -> s{_iuInputProcessingConfigurationUpdate = a})

-- | If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
iuKinesisStreamsInputUpdate :: Lens' InputUpdate (Maybe KinesisStreamsInputUpdate)
iuKinesisStreamsInputUpdate = lens _iuKinesisStreamsInputUpdate (\ s a -> s{_iuKinesisStreamsInputUpdate = a})

-- | Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
iuInputParallelismUpdate :: Lens' InputUpdate (Maybe InputParallelismUpdate)
iuInputParallelismUpdate = lens _iuInputParallelismUpdate (\ s a -> s{_iuInputParallelismUpdate = a})

-- | Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
iuNamePrefixUpdate :: Lens' InputUpdate (Maybe Text)
iuNamePrefixUpdate = lens _iuNamePrefixUpdate (\ s a -> s{_iuNamePrefixUpdate = a})

-- | Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
iuInputSchemaUpdate :: Lens' InputUpdate (Maybe InputSchemaUpdate)
iuInputSchemaUpdate = lens _iuInputSchemaUpdate (\ s a -> s{_iuInputSchemaUpdate = a})

-- | If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
iuKinesisFirehoseInputUpdate :: Lens' InputUpdate (Maybe KinesisFirehoseInputUpdate)
iuKinesisFirehoseInputUpdate = lens _iuKinesisFirehoseInputUpdate (\ s a -> s{_iuKinesisFirehoseInputUpdate = a})

-- | Input ID of the application input to be updated.
iuInputId :: Lens' InputUpdate Text
iuInputId = lens _iuInputId (\ s a -> s{_iuInputId = a})

instance Hashable InputUpdate where

instance NFData InputUpdate where

instance ToJSON InputUpdate where
        toJSON InputUpdate'{..}
          = object
              (catMaybes
                 [("InputProcessingConfigurationUpdate" .=) <$>
                    _iuInputProcessingConfigurationUpdate,
                  ("KinesisStreamsInputUpdate" .=) <$>
                    _iuKinesisStreamsInputUpdate,
                  ("InputParallelismUpdate" .=) <$>
                    _iuInputParallelismUpdate,
                  ("NamePrefixUpdate" .=) <$> _iuNamePrefixUpdate,
                  ("InputSchemaUpdate" .=) <$> _iuInputSchemaUpdate,
                  ("KinesisFirehoseInputUpdate" .=) <$>
                    _iuKinesisFirehoseInputUpdate,
                  Just ("InputId" .= _iuInputId)])

-- | Provides additional mapping information when JSON is the record format on the streaming source.
--
--
--
-- /See:/ 'jsonMappingParameters' smart constructor.
newtype JSONMappingParameters = JSONMappingParameters'
  { _jmpRecordRowPath :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONMappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jmpRecordRowPath' - Path to the top-level parent that contains the records.
jsonMappingParameters
    :: Text -- ^ 'jmpRecordRowPath'
    -> JSONMappingParameters
jsonMappingParameters pRecordRowPath_ =
  JSONMappingParameters' {_jmpRecordRowPath = pRecordRowPath_}


-- | Path to the top-level parent that contains the records.
jmpRecordRowPath :: Lens' JSONMappingParameters Text
jmpRecordRowPath = lens _jmpRecordRowPath (\ s a -> s{_jmpRecordRowPath = a})

instance FromJSON JSONMappingParameters where
        parseJSON
          = withObject "JSONMappingParameters"
              (\ x ->
                 JSONMappingParameters' <$> (x .: "RecordRowPath"))

instance Hashable JSONMappingParameters where

instance NFData JSONMappingParameters where

instance ToJSON JSONMappingParameters where
        toJSON JSONMappingParameters'{..}
          = object
              (catMaybes
                 [Just ("RecordRowPath" .= _jmpRecordRowPath)])

-- | Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
--
--
-- /See:/ 'kinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { _kfiResourceARN :: !Text
  , _kfiRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfiResourceARN' - ARN of the input delivery stream.
--
-- * 'kfiRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure the role has necessary permissions to access the stream.
kinesisFirehoseInput
    :: Text -- ^ 'kfiResourceARN'
    -> Text -- ^ 'kfiRoleARN'
    -> KinesisFirehoseInput
kinesisFirehoseInput pResourceARN_ pRoleARN_ =
  KinesisFirehoseInput'
    {_kfiResourceARN = pResourceARN_, _kfiRoleARN = pRoleARN_}


-- | ARN of the input delivery stream.
kfiResourceARN :: Lens' KinesisFirehoseInput Text
kfiResourceARN = lens _kfiResourceARN (\ s a -> s{_kfiResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure the role has necessary permissions to access the stream.
kfiRoleARN :: Lens' KinesisFirehoseInput Text
kfiRoleARN = lens _kfiRoleARN (\ s a -> s{_kfiRoleARN = a})

instance Hashable KinesisFirehoseInput where

instance NFData KinesisFirehoseInput where

instance ToJSON KinesisFirehoseInput where
        toJSON KinesisFirehoseInput'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _kfiResourceARN),
                  Just ("RoleARN" .= _kfiRoleARN)])

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration.
--
--
--
-- /See:/ 'kinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { _kfidResourceARN :: !(Maybe Text)
  , _kfidRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseInputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfidResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- * 'kfidRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
kinesisFirehoseInputDescription
    :: KinesisFirehoseInputDescription
kinesisFirehoseInputDescription =
  KinesisFirehoseInputDescription'
    {_kfidResourceARN = Nothing, _kfidRoleARN = Nothing}


-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
kfidResourceARN :: Lens' KinesisFirehoseInputDescription (Maybe Text)
kfidResourceARN = lens _kfidResourceARN (\ s a -> s{_kfidResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
kfidRoleARN :: Lens' KinesisFirehoseInputDescription (Maybe Text)
kfidRoleARN = lens _kfidRoleARN (\ s a -> s{_kfidRoleARN = a})

instance FromJSON KinesisFirehoseInputDescription
         where
        parseJSON
          = withObject "KinesisFirehoseInputDescription"
              (\ x ->
                 KinesisFirehoseInputDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable KinesisFirehoseInputDescription
         where

instance NFData KinesisFirehoseInputDescription where

-- | When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.
--
--
--
-- /See:/ 'kinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
  { _kfiuRoleARNUpdate     :: !(Maybe Text)
  , _kfiuResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseInputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfiuRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant necessary permissions to this role.
--
-- * 'kfiuResourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
kinesisFirehoseInputUpdate
    :: KinesisFirehoseInputUpdate
kinesisFirehoseInputUpdate =
  KinesisFirehoseInputUpdate'
    {_kfiuRoleARNUpdate = Nothing, _kfiuResourceARNUpdate = Nothing}


-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant necessary permissions to this role.
kfiuRoleARNUpdate :: Lens' KinesisFirehoseInputUpdate (Maybe Text)
kfiuRoleARNUpdate = lens _kfiuRoleARNUpdate (\ s a -> s{_kfiuRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
kfiuResourceARNUpdate :: Lens' KinesisFirehoseInputUpdate (Maybe Text)
kfiuResourceARNUpdate = lens _kfiuResourceARNUpdate (\ s a -> s{_kfiuResourceARNUpdate = a})

instance Hashable KinesisFirehoseInputUpdate where

instance NFData KinesisFirehoseInputUpdate where

instance ToJSON KinesisFirehoseInputUpdate where
        toJSON KinesisFirehoseInputUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _kfiuRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _kfiuResourceARNUpdate])

-- | When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.
--
--
--
-- /See:/ 'kinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { _kfoResourceARN :: !Text
  , _kfoRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfoResourceARN' - ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
--
-- * 'kfoRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kinesisFirehoseOutput
    :: Text -- ^ 'kfoResourceARN'
    -> Text -- ^ 'kfoRoleARN'
    -> KinesisFirehoseOutput
kinesisFirehoseOutput pResourceARN_ pRoleARN_ =
  KinesisFirehoseOutput'
    {_kfoResourceARN = pResourceARN_, _kfoRoleARN = pRoleARN_}


-- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
kfoResourceARN :: Lens' KinesisFirehoseOutput Text
kfoResourceARN = lens _kfoResourceARN (\ s a -> s{_kfoResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kfoRoleARN :: Lens' KinesisFirehoseOutput Text
kfoRoleARN = lens _kfoRoleARN (\ s a -> s{_kfoRoleARN = a})

instance Hashable KinesisFirehoseOutput where

instance NFData KinesisFirehoseOutput where

instance ToJSON KinesisFirehoseOutput where
        toJSON KinesisFirehoseOutput'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _kfoResourceARN),
                  Just ("RoleARN" .= _kfoRoleARN)])

-- | For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination.
--
--
--
-- /See:/ 'kinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { _kfodResourceARN :: !(Maybe Text)
  , _kfodRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfodResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- * 'kfodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisFirehoseOutputDescription
    :: KinesisFirehoseOutputDescription
kinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    {_kfodResourceARN = Nothing, _kfodRoleARN = Nothing}


-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
kfodResourceARN :: Lens' KinesisFirehoseOutputDescription (Maybe Text)
kfodResourceARN = lens _kfodResourceARN (\ s a -> s{_kfodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kfodRoleARN :: Lens' KinesisFirehoseOutputDescription (Maybe Text)
kfodRoleARN = lens _kfodRoleARN (\ s a -> s{_kfodRoleARN = a})

instance FromJSON KinesisFirehoseOutputDescription
         where
        parseJSON
          = withObject "KinesisFirehoseOutputDescription"
              (\ x ->
                 KinesisFirehoseOutputDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable KinesisFirehoseOutputDescription
         where

instance NFData KinesisFirehoseOutputDescription
         where

-- | When updating an output configuration using the 'UpdateApplication' operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination.
--
--
--
-- /See:/ 'kinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { _kfouRoleARNUpdate     :: !(Maybe Text)
  , _kfouResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisFirehoseOutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfouRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant necessary permissions to this role.
--
-- * 'kfouResourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
kinesisFirehoseOutputUpdate
    :: KinesisFirehoseOutputUpdate
kinesisFirehoseOutputUpdate =
  KinesisFirehoseOutputUpdate'
    {_kfouRoleARNUpdate = Nothing, _kfouResourceARNUpdate = Nothing}


-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant necessary permissions to this role.
kfouRoleARNUpdate :: Lens' KinesisFirehoseOutputUpdate (Maybe Text)
kfouRoleARNUpdate = lens _kfouRoleARNUpdate (\ s a -> s{_kfouRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
kfouResourceARNUpdate :: Lens' KinesisFirehoseOutputUpdate (Maybe Text)
kfouResourceARNUpdate = lens _kfouResourceARNUpdate (\ s a -> s{_kfouResourceARNUpdate = a})

instance Hashable KinesisFirehoseOutputUpdate where

instance NFData KinesisFirehoseOutputUpdate where

instance ToJSON KinesisFirehoseOutputUpdate where
        toJSON KinesisFirehoseOutputUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _kfouRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _kfouResourceARNUpdate])

-- | Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
--
--
-- /See:/ 'kinesisStreamsInput' smart constructor.
data KinesisStreamsInput = KinesisStreamsInput'
  { _ksiResourceARN :: !Text
  , _ksiRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksiResourceARN' - ARN of the input Amazon Kinesis stream to read.
--
-- * 'ksiRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
kinesisStreamsInput
    :: Text -- ^ 'ksiResourceARN'
    -> Text -- ^ 'ksiRoleARN'
    -> KinesisStreamsInput
kinesisStreamsInput pResourceARN_ pRoleARN_ =
  KinesisStreamsInput'
    {_ksiResourceARN = pResourceARN_, _ksiRoleARN = pRoleARN_}


-- | ARN of the input Amazon Kinesis stream to read.
ksiResourceARN :: Lens' KinesisStreamsInput Text
ksiResourceARN = lens _ksiResourceARN (\ s a -> s{_ksiResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
ksiRoleARN :: Lens' KinesisStreamsInput Text
ksiRoleARN = lens _ksiRoleARN (\ s a -> s{_ksiRoleARN = a})

instance Hashable KinesisStreamsInput where

instance NFData KinesisStreamsInput where

instance ToJSON KinesisStreamsInput where
        toJSON KinesisStreamsInput'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _ksiResourceARN),
                  Just ("RoleARN" .= _ksiRoleARN)])

-- | Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration.
--
--
--
-- /See:/ 'kinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
  { _ksidResourceARN :: !(Maybe Text)
  , _ksidRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsInputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksidResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- * 'ksidRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisStreamsInputDescription
    :: KinesisStreamsInputDescription
kinesisStreamsInputDescription =
  KinesisStreamsInputDescription'
    {_ksidResourceARN = Nothing, _ksidRoleARN = Nothing}


-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
ksidResourceARN :: Lens' KinesisStreamsInputDescription (Maybe Text)
ksidResourceARN = lens _ksidResourceARN (\ s a -> s{_ksidResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
ksidRoleARN :: Lens' KinesisStreamsInputDescription (Maybe Text)
ksidRoleARN = lens _ksidRoleARN (\ s a -> s{_ksidRoleARN = a})

instance FromJSON KinesisStreamsInputDescription
         where
        parseJSON
          = withObject "KinesisStreamsInputDescription"
              (\ x ->
                 KinesisStreamsInputDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable KinesisStreamsInputDescription
         where

instance NFData KinesisStreamsInputDescription where

-- | When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.
--
--
--
-- /See:/ 'kinesisStreamsInputUpdate' smart constructor.
data KinesisStreamsInputUpdate = KinesisStreamsInputUpdate'
  { _ksiuRoleARNUpdate     :: !(Maybe Text)
  , _ksiuResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsInputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksiuRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'ksiuResourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
kinesisStreamsInputUpdate
    :: KinesisStreamsInputUpdate
kinesisStreamsInputUpdate =
  KinesisStreamsInputUpdate'
    {_ksiuRoleARNUpdate = Nothing, _ksiuResourceARNUpdate = Nothing}


-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
ksiuRoleARNUpdate :: Lens' KinesisStreamsInputUpdate (Maybe Text)
ksiuRoleARNUpdate = lens _ksiuRoleARNUpdate (\ s a -> s{_ksiuRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
ksiuResourceARNUpdate :: Lens' KinesisStreamsInputUpdate (Maybe Text)
ksiuResourceARNUpdate = lens _ksiuResourceARNUpdate (\ s a -> s{_ksiuResourceARNUpdate = a})

instance Hashable KinesisStreamsInputUpdate where

instance NFData KinesisStreamsInputUpdate where

instance ToJSON KinesisStreamsInputUpdate where
        toJSON KinesisStreamsInputUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _ksiuRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _ksiuResourceARNUpdate])

-- | When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.
--
--
--
-- /See:/ 'kinesisStreamsOutput' smart constructor.
data KinesisStreamsOutput = KinesisStreamsOutput'
  { _ksoResourceARN :: !Text
  , _ksoRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksoResourceARN' - ARN of the destination Amazon Kinesis stream to write to.
--
-- * 'ksoRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kinesisStreamsOutput
    :: Text -- ^ 'ksoResourceARN'
    -> Text -- ^ 'ksoRoleARN'
    -> KinesisStreamsOutput
kinesisStreamsOutput pResourceARN_ pRoleARN_ =
  KinesisStreamsOutput'
    {_ksoResourceARN = pResourceARN_, _ksoRoleARN = pRoleARN_}


-- | ARN of the destination Amazon Kinesis stream to write to.
ksoResourceARN :: Lens' KinesisStreamsOutput Text
ksoResourceARN = lens _ksoResourceARN (\ s a -> s{_ksoResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
ksoRoleARN :: Lens' KinesisStreamsOutput Text
ksoRoleARN = lens _ksoRoleARN (\ s a -> s{_ksoRoleARN = a})

instance Hashable KinesisStreamsOutput where

instance NFData KinesisStreamsOutput where

instance ToJSON KinesisStreamsOutput where
        toJSON KinesisStreamsOutput'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _ksoResourceARN),
                  Just ("RoleARN" .= _ksoRoleARN)])

-- | For an application output, describes the Amazon Kinesis stream configured as its destination.
--
--
--
-- /See:/ 'kinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { _ksodResourceARN :: !(Maybe Text)
  , _ksodRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksodResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- * 'ksodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisStreamsOutputDescription
    :: KinesisStreamsOutputDescription
kinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    {_ksodResourceARN = Nothing, _ksodRoleARN = Nothing}


-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
ksodResourceARN :: Lens' KinesisStreamsOutputDescription (Maybe Text)
ksodResourceARN = lens _ksodResourceARN (\ s a -> s{_ksodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
ksodRoleARN :: Lens' KinesisStreamsOutputDescription (Maybe Text)
ksodRoleARN = lens _ksodRoleARN (\ s a -> s{_ksodRoleARN = a})

instance FromJSON KinesisStreamsOutputDescription
         where
        parseJSON
          = withObject "KinesisStreamsOutputDescription"
              (\ x ->
                 KinesisStreamsOutputDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable KinesisStreamsOutputDescription
         where

instance NFData KinesisStreamsOutputDescription where

-- | When updating an output configuration using the 'UpdateApplication' operation, provides information about an Amazon Kinesis stream configured as the destination.
--
--
--
-- /See:/ 'kinesisStreamsOutputUpdate' smart constructor.
data KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate'
  { _ksouRoleARNUpdate     :: !(Maybe Text)
  , _ksouResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KinesisStreamsOutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksouRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'ksouResourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want to write the output.
kinesisStreamsOutputUpdate
    :: KinesisStreamsOutputUpdate
kinesisStreamsOutputUpdate =
  KinesisStreamsOutputUpdate'
    {_ksouRoleARNUpdate = Nothing, _ksouResourceARNUpdate = Nothing}


-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
ksouRoleARNUpdate :: Lens' KinesisStreamsOutputUpdate (Maybe Text)
ksouRoleARNUpdate = lens _ksouRoleARNUpdate (\ s a -> s{_ksouRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want to write the output.
ksouResourceARNUpdate :: Lens' KinesisStreamsOutputUpdate (Maybe Text)
ksouResourceARNUpdate = lens _ksouResourceARNUpdate (\ s a -> s{_ksouResourceARNUpdate = a})

instance Hashable KinesisStreamsOutputUpdate where

instance NFData KinesisStreamsOutputUpdate where

instance ToJSON KinesisStreamsOutputUpdate where
        toJSON KinesisStreamsOutputUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _ksouRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _ksouResourceARNUpdate])

-- | When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf.
--
--
--
-- /See:/ 'lambdaOutput' smart constructor.
data LambdaOutput = LambdaOutput'
  { _loResourceARN :: !Text
  , _loRoleARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loResourceARN' - Amazon Resource Name (ARN) of the destination Lambda function to write to.
--
-- * 'loRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
lambdaOutput
    :: Text -- ^ 'loResourceARN'
    -> Text -- ^ 'loRoleARN'
    -> LambdaOutput
lambdaOutput pResourceARN_ pRoleARN_ =
  LambdaOutput' {_loResourceARN = pResourceARN_, _loRoleARN = pRoleARN_}


-- | Amazon Resource Name (ARN) of the destination Lambda function to write to.
loResourceARN :: Lens' LambdaOutput Text
loResourceARN = lens _loResourceARN (\ s a -> s{_loResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
loRoleARN :: Lens' LambdaOutput Text
loRoleARN = lens _loRoleARN (\ s a -> s{_loRoleARN = a})

instance Hashable LambdaOutput where

instance NFData LambdaOutput where

instance ToJSON LambdaOutput where
        toJSON LambdaOutput'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _loResourceARN),
                  Just ("RoleARN" .= _loRoleARN)])

-- | For an application output, describes the AWS Lambda function configured as its destination.
--
--
--
-- /See:/ 'lambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { _lodResourceARN :: !(Maybe Text)
  , _lodRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lodResourceARN' - Amazon Resource Name (ARN) of the destination Lambda function.
--
-- * 'lodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
lambdaOutputDescription
    :: LambdaOutputDescription
lambdaOutputDescription =
  LambdaOutputDescription' {_lodResourceARN = Nothing, _lodRoleARN = Nothing}


-- | Amazon Resource Name (ARN) of the destination Lambda function.
lodResourceARN :: Lens' LambdaOutputDescription (Maybe Text)
lodResourceARN = lens _lodResourceARN (\ s a -> s{_lodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
lodRoleARN :: Lens' LambdaOutputDescription (Maybe Text)
lodRoleARN = lens _lodRoleARN (\ s a -> s{_lodRoleARN = a})

instance FromJSON LambdaOutputDescription where
        parseJSON
          = withObject "LambdaOutputDescription"
              (\ x ->
                 LambdaOutputDescription' <$>
                   (x .:? "ResourceARN") <*> (x .:? "RoleARN"))

instance Hashable LambdaOutputDescription where

instance NFData LambdaOutputDescription where

-- | When updating an output configuration using the 'UpdateApplication' operation, provides information about an AWS Lambda function configured as the destination.
--
--
--
-- /See:/ 'lambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { _louRoleARNUpdate     :: !(Maybe Text)
  , _louResourceARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaOutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'louRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'louResourceARNUpdate' - Amazon Resource Name (ARN) of the destination Lambda function.
lambdaOutputUpdate
    :: LambdaOutputUpdate
lambdaOutputUpdate =
  LambdaOutputUpdate'
    {_louRoleARNUpdate = Nothing, _louResourceARNUpdate = Nothing}


-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
louRoleARNUpdate :: Lens' LambdaOutputUpdate (Maybe Text)
louRoleARNUpdate = lens _louRoleARNUpdate (\ s a -> s{_louRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the destination Lambda function.
louResourceARNUpdate :: Lens' LambdaOutputUpdate (Maybe Text)
louResourceARNUpdate = lens _louResourceARNUpdate (\ s a -> s{_louResourceARNUpdate = a})

instance Hashable LambdaOutputUpdate where

instance NFData LambdaOutputUpdate where

instance ToJSON LambdaOutputUpdate where
        toJSON LambdaOutputUpdate'{..}
          = object
              (catMaybes
                 [("RoleARNUpdate" .=) <$> _louRoleARNUpdate,
                  ("ResourceARNUpdate" .=) <$> _louResourceARNUpdate])

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
--
--
-- /See:/ 'mappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { _mpCSVMappingParameters  :: !(Maybe CSVMappingParameters)
  , _mpJSONMappingParameters :: !(Maybe JSONMappingParameters)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpCSVMappingParameters' - Provides additional mapping information when the record format uses delimiters (for example, CSV).
--
-- * 'mpJSONMappingParameters' - Provides additional mapping information when JSON is the record format on the streaming source.
mappingParameters
    :: MappingParameters
mappingParameters =
  MappingParameters'
    {_mpCSVMappingParameters = Nothing, _mpJSONMappingParameters = Nothing}


-- | Provides additional mapping information when the record format uses delimiters (for example, CSV).
mpCSVMappingParameters :: Lens' MappingParameters (Maybe CSVMappingParameters)
mpCSVMappingParameters = lens _mpCSVMappingParameters (\ s a -> s{_mpCSVMappingParameters = a})

-- | Provides additional mapping information when JSON is the record format on the streaming source.
mpJSONMappingParameters :: Lens' MappingParameters (Maybe JSONMappingParameters)
mpJSONMappingParameters = lens _mpJSONMappingParameters (\ s a -> s{_mpJSONMappingParameters = a})

instance FromJSON MappingParameters where
        parseJSON
          = withObject "MappingParameters"
              (\ x ->
                 MappingParameters' <$>
                   (x .:? "CSVMappingParameters") <*>
                     (x .:? "JSONMappingParameters"))

instance Hashable MappingParameters where

instance NFData MappingParameters where

instance ToJSON MappingParameters where
        toJSON MappingParameters'{..}
          = object
              (catMaybes
                 [("CSVMappingParameters" .=) <$>
                    _mpCSVMappingParameters,
                  ("JSONMappingParameters" .=) <$>
                    _mpJSONMappingParameters])

-- | Describes application output configuration in which you identify an in-application stream and a destination where you want the in-application stream data to be written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream.
--
--
--
--
-- For limits on how many destinations an application can write and other limitations, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
--
--
-- /See:/ 'output' smart constructor.
data Output = Output'
  { _oLambdaOutput          :: !(Maybe LambdaOutput)
  , _oKinesisStreamsOutput  :: !(Maybe KinesisStreamsOutput)
  , _oKinesisFirehoseOutput :: !(Maybe KinesisFirehoseOutput)
  , _oName                  :: !Text
  , _oDestinationSchema     :: !DestinationSchema
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oLambdaOutput' - Identifies an AWS Lambda function as the destination.
--
-- * 'oKinesisStreamsOutput' - Identifies an Amazon Kinesis stream as the destination.
--
-- * 'oKinesisFirehoseOutput' - Identifies an Amazon Kinesis Firehose delivery stream as the destination.
--
-- * 'oName' - Name of the in-application stream.
--
-- * 'oDestinationSchema' - Describes the data format when records are written to the destination. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
output
    :: Text -- ^ 'oName'
    -> DestinationSchema -- ^ 'oDestinationSchema'
    -> Output
output pName_ pDestinationSchema_ =
  Output'
    { _oLambdaOutput = Nothing
    , _oKinesisStreamsOutput = Nothing
    , _oKinesisFirehoseOutput = Nothing
    , _oName = pName_
    , _oDestinationSchema = pDestinationSchema_
    }


-- | Identifies an AWS Lambda function as the destination.
oLambdaOutput :: Lens' Output (Maybe LambdaOutput)
oLambdaOutput = lens _oLambdaOutput (\ s a -> s{_oLambdaOutput = a})

-- | Identifies an Amazon Kinesis stream as the destination.
oKinesisStreamsOutput :: Lens' Output (Maybe KinesisStreamsOutput)
oKinesisStreamsOutput = lens _oKinesisStreamsOutput (\ s a -> s{_oKinesisStreamsOutput = a})

-- | Identifies an Amazon Kinesis Firehose delivery stream as the destination.
oKinesisFirehoseOutput :: Lens' Output (Maybe KinesisFirehoseOutput)
oKinesisFirehoseOutput = lens _oKinesisFirehoseOutput (\ s a -> s{_oKinesisFirehoseOutput = a})

-- | Name of the in-application stream.
oName :: Lens' Output Text
oName = lens _oName (\ s a -> s{_oName = a})

-- | Describes the data format when records are written to the destination. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
oDestinationSchema :: Lens' Output DestinationSchema
oDestinationSchema = lens _oDestinationSchema (\ s a -> s{_oDestinationSchema = a})

instance Hashable Output where

instance NFData Output where

instance ToJSON Output where
        toJSON Output'{..}
          = object
              (catMaybes
                 [("LambdaOutput" .=) <$> _oLambdaOutput,
                  ("KinesisStreamsOutput" .=) <$>
                    _oKinesisStreamsOutput,
                  ("KinesisFirehoseOutput" .=) <$>
                    _oKinesisFirehoseOutput,
                  Just ("Name" .= _oName),
                  Just ("DestinationSchema" .= _oDestinationSchema)])

-- | Describes the application output configuration, which includes the in-application stream name and the destination where the stream data is written. The destination can be an Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream.
--
--
--
-- /See:/ 'outputDescription' smart constructor.
data OutputDescription = OutputDescription'
  { _odOutputId :: !(Maybe Text)
  , _odDestinationSchema :: !(Maybe DestinationSchema)
  , _odKinesisFirehoseOutputDescription :: !(Maybe KinesisFirehoseOutputDescription)
  , _odKinesisStreamsOutputDescription :: !(Maybe KinesisStreamsOutputDescription)
  , _odName :: !(Maybe Text)
  , _odLambdaOutputDescription :: !(Maybe LambdaOutputDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odOutputId' - A unique identifier for the output configuration.
--
-- * 'odDestinationSchema' - Data format used for writing data to the destination.
--
-- * 'odKinesisFirehoseOutputDescription' - Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
--
-- * 'odKinesisStreamsOutputDescription' - Describes Amazon Kinesis stream configured as the destination where output is written.
--
-- * 'odName' - Name of the in-application stream configured as output.
--
-- * 'odLambdaOutputDescription' - Describes the AWS Lambda function configured as the destination where output is written.
outputDescription
    :: OutputDescription
outputDescription =
  OutputDescription'
    { _odOutputId = Nothing
    , _odDestinationSchema = Nothing
    , _odKinesisFirehoseOutputDescription = Nothing
    , _odKinesisStreamsOutputDescription = Nothing
    , _odName = Nothing
    , _odLambdaOutputDescription = Nothing
    }


-- | A unique identifier for the output configuration.
odOutputId :: Lens' OutputDescription (Maybe Text)
odOutputId = lens _odOutputId (\ s a -> s{_odOutputId = a})

-- | Data format used for writing data to the destination.
odDestinationSchema :: Lens' OutputDescription (Maybe DestinationSchema)
odDestinationSchema = lens _odDestinationSchema (\ s a -> s{_odDestinationSchema = a})

-- | Describes the Amazon Kinesis Firehose delivery stream configured as the destination where output is written.
odKinesisFirehoseOutputDescription :: Lens' OutputDescription (Maybe KinesisFirehoseOutputDescription)
odKinesisFirehoseOutputDescription = lens _odKinesisFirehoseOutputDescription (\ s a -> s{_odKinesisFirehoseOutputDescription = a})

-- | Describes Amazon Kinesis stream configured as the destination where output is written.
odKinesisStreamsOutputDescription :: Lens' OutputDescription (Maybe KinesisStreamsOutputDescription)
odKinesisStreamsOutputDescription = lens _odKinesisStreamsOutputDescription (\ s a -> s{_odKinesisStreamsOutputDescription = a})

-- | Name of the in-application stream configured as output.
odName :: Lens' OutputDescription (Maybe Text)
odName = lens _odName (\ s a -> s{_odName = a})

-- | Describes the AWS Lambda function configured as the destination where output is written.
odLambdaOutputDescription :: Lens' OutputDescription (Maybe LambdaOutputDescription)
odLambdaOutputDescription = lens _odLambdaOutputDescription (\ s a -> s{_odLambdaOutputDescription = a})

instance FromJSON OutputDescription where
        parseJSON
          = withObject "OutputDescription"
              (\ x ->
                 OutputDescription' <$>
                   (x .:? "OutputId") <*> (x .:? "DestinationSchema")
                     <*> (x .:? "KinesisFirehoseOutputDescription")
                     <*> (x .:? "KinesisStreamsOutputDescription")
                     <*> (x .:? "Name")
                     <*> (x .:? "LambdaOutputDescription"))

instance Hashable OutputDescription where

instance NFData OutputDescription where

-- | Describes updates to the output configuration identified by the @OutputId@ .
--
--
--
-- /See:/ 'outputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { _ouKinesisStreamsOutputUpdate  :: !(Maybe KinesisStreamsOutputUpdate)
  , _ouDestinationSchemaUpdate     :: !(Maybe DestinationSchema)
  , _ouKinesisFirehoseOutputUpdate :: !(Maybe KinesisFirehoseOutputUpdate)
  , _ouNameUpdate                  :: !(Maybe Text)
  , _ouLambdaOutputUpdate          :: !(Maybe LambdaOutputUpdate)
  , _ouOutputId                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ouKinesisStreamsOutputUpdate' - Describes an Amazon Kinesis stream as the destination for the output.
--
-- * 'ouDestinationSchemaUpdate' - Describes the data format when records are written to the destination. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
--
-- * 'ouKinesisFirehoseOutputUpdate' - Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
--
-- * 'ouNameUpdate' - If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
--
-- * 'ouLambdaOutputUpdate' - Describes an AWS Lambda function as the destination for the output.
--
-- * 'ouOutputId' - Identifies the specific output configuration that you want to update.
outputUpdate
    :: Text -- ^ 'ouOutputId'
    -> OutputUpdate
outputUpdate pOutputId_ =
  OutputUpdate'
    { _ouKinesisStreamsOutputUpdate = Nothing
    , _ouDestinationSchemaUpdate = Nothing
    , _ouKinesisFirehoseOutputUpdate = Nothing
    , _ouNameUpdate = Nothing
    , _ouLambdaOutputUpdate = Nothing
    , _ouOutputId = pOutputId_
    }


-- | Describes an Amazon Kinesis stream as the destination for the output.
ouKinesisStreamsOutputUpdate :: Lens' OutputUpdate (Maybe KinesisStreamsOutputUpdate)
ouKinesisStreamsOutputUpdate = lens _ouKinesisStreamsOutputUpdate (\ s a -> s{_ouKinesisStreamsOutputUpdate = a})

-- | Describes the data format when records are written to the destination. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output> .
ouDestinationSchemaUpdate :: Lens' OutputUpdate (Maybe DestinationSchema)
ouDestinationSchemaUpdate = lens _ouDestinationSchemaUpdate (\ s a -> s{_ouDestinationSchemaUpdate = a})

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination for the output.
ouKinesisFirehoseOutputUpdate :: Lens' OutputUpdate (Maybe KinesisFirehoseOutputUpdate)
ouKinesisFirehoseOutputUpdate = lens _ouKinesisFirehoseOutputUpdate (\ s a -> s{_ouKinesisFirehoseOutputUpdate = a})

-- | If you want to specify a different in-application stream for this output configuration, use this field to specify the new in-application stream name.
ouNameUpdate :: Lens' OutputUpdate (Maybe Text)
ouNameUpdate = lens _ouNameUpdate (\ s a -> s{_ouNameUpdate = a})

-- | Describes an AWS Lambda function as the destination for the output.
ouLambdaOutputUpdate :: Lens' OutputUpdate (Maybe LambdaOutputUpdate)
ouLambdaOutputUpdate = lens _ouLambdaOutputUpdate (\ s a -> s{_ouLambdaOutputUpdate = a})

-- | Identifies the specific output configuration that you want to update.
ouOutputId :: Lens' OutputUpdate Text
ouOutputId = lens _ouOutputId (\ s a -> s{_ouOutputId = a})

instance Hashable OutputUpdate where

instance NFData OutputUpdate where

instance ToJSON OutputUpdate where
        toJSON OutputUpdate'{..}
          = object
              (catMaybes
                 [("KinesisStreamsOutputUpdate" .=) <$>
                    _ouKinesisStreamsOutputUpdate,
                  ("DestinationSchemaUpdate" .=) <$>
                    _ouDestinationSchemaUpdate,
                  ("KinesisFirehoseOutputUpdate" .=) <$>
                    _ouKinesisFirehoseOutputUpdate,
                  ("NameUpdate" .=) <$> _ouNameUpdate,
                  ("LambdaOutputUpdate" .=) <$> _ouLambdaOutputUpdate,
                  Just ("OutputId" .= _ouOutputId)])

-- | Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.
--
--
-- Also used to describe the format of the reference data source.
--
--
-- /See:/ 'recordColumn' smart constructor.
data RecordColumn = RecordColumn'
  { _rcMapping :: !(Maybe Text)
  , _rcName    :: !Text
  , _rcSqlType :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordColumn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcMapping' - Reference to the data element in the streaming input of the reference data source.
--
-- * 'rcName' - Name of the column created in the in-application input stream or reference table.
--
-- * 'rcSqlType' - Type of column created in the in-application input stream or reference table.
recordColumn
    :: Text -- ^ 'rcName'
    -> Text -- ^ 'rcSqlType'
    -> RecordColumn
recordColumn pName_ pSqlType_ =
  RecordColumn' {_rcMapping = Nothing, _rcName = pName_, _rcSqlType = pSqlType_}


-- | Reference to the data element in the streaming input of the reference data source.
rcMapping :: Lens' RecordColumn (Maybe Text)
rcMapping = lens _rcMapping (\ s a -> s{_rcMapping = a})

-- | Name of the column created in the in-application input stream or reference table.
rcName :: Lens' RecordColumn Text
rcName = lens _rcName (\ s a -> s{_rcName = a})

-- | Type of column created in the in-application input stream or reference table.
rcSqlType :: Lens' RecordColumn Text
rcSqlType = lens _rcSqlType (\ s a -> s{_rcSqlType = a})

instance FromJSON RecordColumn where
        parseJSON
          = withObject "RecordColumn"
              (\ x ->
                 RecordColumn' <$>
                   (x .:? "Mapping") <*> (x .: "Name") <*>
                     (x .: "SqlType"))

instance Hashable RecordColumn where

instance NFData RecordColumn where

instance ToJSON RecordColumn where
        toJSON RecordColumn'{..}
          = object
              (catMaybes
                 [("Mapping" .=) <$> _rcMapping,
                  Just ("Name" .= _rcName),
                  Just ("SqlType" .= _rcSqlType)])

-- | Describes the record format and relevant mapping information that should be applied to schematize the records on the stream.
--
--
--
-- /See:/ 'recordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { _rfMappingParameters :: !(Maybe MappingParameters)
  , _rfRecordFormatType  :: !RecordFormatType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfMappingParameters' - When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- * 'rfRecordFormatType' - The type of record format.
recordFormat
    :: RecordFormatType -- ^ 'rfRecordFormatType'
    -> RecordFormat
recordFormat pRecordFormatType_ =
  RecordFormat'
    {_rfMappingParameters = Nothing, _rfRecordFormatType = pRecordFormatType_}


-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
rfMappingParameters :: Lens' RecordFormat (Maybe MappingParameters)
rfMappingParameters = lens _rfMappingParameters (\ s a -> s{_rfMappingParameters = a})

-- | The type of record format.
rfRecordFormatType :: Lens' RecordFormat RecordFormatType
rfRecordFormatType = lens _rfRecordFormatType (\ s a -> s{_rfRecordFormatType = a})

instance FromJSON RecordFormat where
        parseJSON
          = withObject "RecordFormat"
              (\ x ->
                 RecordFormat' <$>
                   (x .:? "MappingParameters") <*>
                     (x .: "RecordFormatType"))

instance Hashable RecordFormat where

instance NFData RecordFormat where

instance ToJSON RecordFormat where
        toJSON RecordFormat'{..}
          = object
              (catMaybes
                 [("MappingParameters" .=) <$> _rfMappingParameters,
                  Just ("RecordFormatType" .= _rfRecordFormatType)])

-- | Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.
--
--
--
-- /See:/ 'referenceDataSource' smart constructor.
data ReferenceDataSource = ReferenceDataSource'
  { _rdsS3ReferenceDataSource :: !(Maybe S3ReferenceDataSource)
  , _rdsTableName             :: !Text
  , _rdsReferenceSchema       :: !SourceSchema
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReferenceDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsS3ReferenceDataSource' - Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the 'UpdateApplication' operation to trigger reloading of data into your application.
--
-- * 'rdsTableName' - Name of the in-application table to create.
--
-- * 'rdsReferenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
referenceDataSource
    :: Text -- ^ 'rdsTableName'
    -> SourceSchema -- ^ 'rdsReferenceSchema'
    -> ReferenceDataSource
referenceDataSource pTableName_ pReferenceSchema_ =
  ReferenceDataSource'
    { _rdsS3ReferenceDataSource = Nothing
    , _rdsTableName = pTableName_
    , _rdsReferenceSchema = pReferenceSchema_
    }


-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the 'UpdateApplication' operation to trigger reloading of data into your application.
rdsS3ReferenceDataSource :: Lens' ReferenceDataSource (Maybe S3ReferenceDataSource)
rdsS3ReferenceDataSource = lens _rdsS3ReferenceDataSource (\ s a -> s{_rdsS3ReferenceDataSource = a})

-- | Name of the in-application table to create.
rdsTableName :: Lens' ReferenceDataSource Text
rdsTableName = lens _rdsTableName (\ s a -> s{_rdsTableName = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsReferenceSchema :: Lens' ReferenceDataSource SourceSchema
rdsReferenceSchema = lens _rdsReferenceSchema (\ s a -> s{_rdsReferenceSchema = a})

instance Hashable ReferenceDataSource where

instance NFData ReferenceDataSource where

instance ToJSON ReferenceDataSource where
        toJSON ReferenceDataSource'{..}
          = object
              (catMaybes
                 [("S3ReferenceDataSource" .=) <$>
                    _rdsS3ReferenceDataSource,
                  Just ("TableName" .= _rdsTableName),
                  Just ("ReferenceSchema" .= _rdsReferenceSchema)])

-- | Describes the reference data source configured for an application.
--
--
--
-- /See:/ 'referenceDataSourceDescription' smart constructor.
data ReferenceDataSourceDescription = ReferenceDataSourceDescription'
  { _rdsdReferenceSchema                  :: !(Maybe SourceSchema)
  , _rdsdReferenceId                      :: !Text
  , _rdsdTableName                        :: !Text
  , _rdsdS3ReferenceDataSourceDescription :: !S3ReferenceDataSourceDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdReferenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- * 'rdsdReferenceId' - ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the 'AddApplicationReferenceDataSource' operation.
--
-- * 'rdsdTableName' - The in-application table name created by the specific reference data source configuration.
--
-- * 'rdsdS3ReferenceDataSourceDescription' - Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
referenceDataSourceDescription
    :: Text -- ^ 'rdsdReferenceId'
    -> Text -- ^ 'rdsdTableName'
    -> S3ReferenceDataSourceDescription -- ^ 'rdsdS3ReferenceDataSourceDescription'
    -> ReferenceDataSourceDescription
referenceDataSourceDescription pReferenceId_ pTableName_ pS3ReferenceDataSourceDescription_ =
  ReferenceDataSourceDescription'
    { _rdsdReferenceSchema = Nothing
    , _rdsdReferenceId = pReferenceId_
    , _rdsdTableName = pTableName_
    , _rdsdS3ReferenceDataSourceDescription = pS3ReferenceDataSourceDescription_
    }


-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsdReferenceSchema :: Lens' ReferenceDataSourceDescription (Maybe SourceSchema)
rdsdReferenceSchema = lens _rdsdReferenceSchema (\ s a -> s{_rdsdReferenceSchema = a})

-- | ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the 'AddApplicationReferenceDataSource' operation.
rdsdReferenceId :: Lens' ReferenceDataSourceDescription Text
rdsdReferenceId = lens _rdsdReferenceId (\ s a -> s{_rdsdReferenceId = a})

-- | The in-application table name created by the specific reference data source configuration.
rdsdTableName :: Lens' ReferenceDataSourceDescription Text
rdsdTableName = lens _rdsdTableName (\ s a -> s{_rdsdTableName = a})

-- | Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
rdsdS3ReferenceDataSourceDescription :: Lens' ReferenceDataSourceDescription S3ReferenceDataSourceDescription
rdsdS3ReferenceDataSourceDescription = lens _rdsdS3ReferenceDataSourceDescription (\ s a -> s{_rdsdS3ReferenceDataSourceDescription = a})

instance FromJSON ReferenceDataSourceDescription
         where
        parseJSON
          = withObject "ReferenceDataSourceDescription"
              (\ x ->
                 ReferenceDataSourceDescription' <$>
                   (x .:? "ReferenceSchema") <*> (x .: "ReferenceId")
                     <*> (x .: "TableName")
                     <*> (x .: "S3ReferenceDataSourceDescription"))

instance Hashable ReferenceDataSourceDescription
         where

instance NFData ReferenceDataSourceDescription where

-- | When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.
--
--
--
-- /See:/ 'referenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { _rdsuTableNameUpdate             :: !(Maybe Text)
  , _rdsuS3ReferenceDataSourceUpdate :: !(Maybe S3ReferenceDataSourceUpdate)
  , _rdsuReferenceSchemaUpdate       :: !(Maybe SourceSchema)
  , _rdsuReferenceId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReferenceDataSourceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsuTableNameUpdate' - In-application table name that is created by this update.
--
-- * 'rdsuS3ReferenceDataSourceUpdate' - Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- * 'rdsuReferenceSchemaUpdate' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- * 'rdsuReferenceId' - ID of the reference data source being updated. You can use the 'DescribeApplication' operation to get this value.
referenceDataSourceUpdate
    :: Text -- ^ 'rdsuReferenceId'
    -> ReferenceDataSourceUpdate
referenceDataSourceUpdate pReferenceId_ =
  ReferenceDataSourceUpdate'
    { _rdsuTableNameUpdate = Nothing
    , _rdsuS3ReferenceDataSourceUpdate = Nothing
    , _rdsuReferenceSchemaUpdate = Nothing
    , _rdsuReferenceId = pReferenceId_
    }


-- | In-application table name that is created by this update.
rdsuTableNameUpdate :: Lens' ReferenceDataSourceUpdate (Maybe Text)
rdsuTableNameUpdate = lens _rdsuTableNameUpdate (\ s a -> s{_rdsuTableNameUpdate = a})

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
rdsuS3ReferenceDataSourceUpdate :: Lens' ReferenceDataSourceUpdate (Maybe S3ReferenceDataSourceUpdate)
rdsuS3ReferenceDataSourceUpdate = lens _rdsuS3ReferenceDataSourceUpdate (\ s a -> s{_rdsuS3ReferenceDataSourceUpdate = a})

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
rdsuReferenceSchemaUpdate :: Lens' ReferenceDataSourceUpdate (Maybe SourceSchema)
rdsuReferenceSchemaUpdate = lens _rdsuReferenceSchemaUpdate (\ s a -> s{_rdsuReferenceSchemaUpdate = a})

-- | ID of the reference data source being updated. You can use the 'DescribeApplication' operation to get this value.
rdsuReferenceId :: Lens' ReferenceDataSourceUpdate Text
rdsuReferenceId = lens _rdsuReferenceId (\ s a -> s{_rdsuReferenceId = a})

instance Hashable ReferenceDataSourceUpdate where

instance NFData ReferenceDataSourceUpdate where

instance ToJSON ReferenceDataSourceUpdate where
        toJSON ReferenceDataSourceUpdate'{..}
          = object
              (catMaybes
                 [("TableNameUpdate" .=) <$> _rdsuTableNameUpdate,
                  ("S3ReferenceDataSourceUpdate" .=) <$>
                    _rdsuS3ReferenceDataSourceUpdate,
                  ("ReferenceSchemaUpdate" .=) <$>
                    _rdsuReferenceSchemaUpdate,
                  Just ("ReferenceId" .= _rdsuReferenceId)])

-- | Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the S3 object that contains the data.
--
--
--
-- /See:/ 's3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { _scRoleARN   :: !Text
  , _scBucketARN :: !Text
  , _scFileKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scRoleARN' - IAM ARN of the role used to access the data.
--
-- * 'scBucketARN' - ARN of the S3 bucket that contains the data.
--
-- * 'scFileKey' - The name of the object that contains the data.
s3Configuration
    :: Text -- ^ 'scRoleARN'
    -> Text -- ^ 'scBucketARN'
    -> Text -- ^ 'scFileKey'
    -> S3Configuration
s3Configuration pRoleARN_ pBucketARN_ pFileKey_ =
  S3Configuration'
    {_scRoleARN = pRoleARN_, _scBucketARN = pBucketARN_, _scFileKey = pFileKey_}


-- | IAM ARN of the role used to access the data.
scRoleARN :: Lens' S3Configuration Text
scRoleARN = lens _scRoleARN (\ s a -> s{_scRoleARN = a})

-- | ARN of the S3 bucket that contains the data.
scBucketARN :: Lens' S3Configuration Text
scBucketARN = lens _scBucketARN (\ s a -> s{_scBucketARN = a})

-- | The name of the object that contains the data.
scFileKey :: Lens' S3Configuration Text
scFileKey = lens _scFileKey (\ s a -> s{_scFileKey = a})

instance Hashable S3Configuration where

instance NFData S3Configuration where

instance ToJSON S3Configuration where
        toJSON S3Configuration'{..}
          = object
              (catMaybes
                 [Just ("RoleARN" .= _scRoleARN),
                  Just ("BucketARN" .= _scBucketARN),
                  Just ("FileKey" .= _scFileKey)])

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.
--
--
-- An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the 'UpdateApplication' operation to trigger reloading of data into your application.
--
--
-- /See:/ 's3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { _srdsBucketARN        :: !Text
  , _srdsFileKey          :: !Text
  , _srdsReferenceRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ReferenceDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdsBucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- * 'srdsFileKey' - Object key name containing reference data.
--
-- * 'srdsReferenceRoleARN' - ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
s3ReferenceDataSource
    :: Text -- ^ 'srdsBucketARN'
    -> Text -- ^ 'srdsFileKey'
    -> Text -- ^ 'srdsReferenceRoleARN'
    -> S3ReferenceDataSource
s3ReferenceDataSource pBucketARN_ pFileKey_ pReferenceRoleARN_ =
  S3ReferenceDataSource'
    { _srdsBucketARN = pBucketARN_
    , _srdsFileKey = pFileKey_
    , _srdsReferenceRoleARN = pReferenceRoleARN_
    }


-- | Amazon Resource Name (ARN) of the S3 bucket.
srdsBucketARN :: Lens' S3ReferenceDataSource Text
srdsBucketARN = lens _srdsBucketARN (\ s a -> s{_srdsBucketARN = a})

-- | Object key name containing reference data.
srdsFileKey :: Lens' S3ReferenceDataSource Text
srdsFileKey = lens _srdsFileKey (\ s a -> s{_srdsFileKey = a})

-- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
srdsReferenceRoleARN :: Lens' S3ReferenceDataSource Text
srdsReferenceRoleARN = lens _srdsReferenceRoleARN (\ s a -> s{_srdsReferenceRoleARN = a})

instance Hashable S3ReferenceDataSource where

instance NFData S3ReferenceDataSource where

instance ToJSON S3ReferenceDataSource where
        toJSON S3ReferenceDataSource'{..}
          = object
              (catMaybes
                 [Just ("BucketARN" .= _srdsBucketARN),
                  Just ("FileKey" .= _srdsFileKey),
                  Just ("ReferenceRoleARN" .= _srdsReferenceRoleARN)])

-- | Provides the bucket name and object key name that stores the reference data.
--
--
--
-- /See:/ 's3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { _srdsdBucketARN        :: !Text
  , _srdsdFileKey          :: !Text
  , _srdsdReferenceRoleARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdsdBucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- * 'srdsdFileKey' - Amazon S3 object key name.
--
-- * 'srdsdReferenceRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
s3ReferenceDataSourceDescription
    :: Text -- ^ 'srdsdBucketARN'
    -> Text -- ^ 'srdsdFileKey'
    -> Text -- ^ 'srdsdReferenceRoleARN'
    -> S3ReferenceDataSourceDescription
s3ReferenceDataSourceDescription pBucketARN_ pFileKey_ pReferenceRoleARN_ =
  S3ReferenceDataSourceDescription'
    { _srdsdBucketARN = pBucketARN_
    , _srdsdFileKey = pFileKey_
    , _srdsdReferenceRoleARN = pReferenceRoleARN_
    }


-- | Amazon Resource Name (ARN) of the S3 bucket.
srdsdBucketARN :: Lens' S3ReferenceDataSourceDescription Text
srdsdBucketARN = lens _srdsdBucketARN (\ s a -> s{_srdsdBucketARN = a})

-- | Amazon S3 object key name.
srdsdFileKey :: Lens' S3ReferenceDataSourceDescription Text
srdsdFileKey = lens _srdsdFileKey (\ s a -> s{_srdsdFileKey = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
srdsdReferenceRoleARN :: Lens' S3ReferenceDataSourceDescription Text
srdsdReferenceRoleARN = lens _srdsdReferenceRoleARN (\ s a -> s{_srdsdReferenceRoleARN = a})

instance FromJSON S3ReferenceDataSourceDescription
         where
        parseJSON
          = withObject "S3ReferenceDataSourceDescription"
              (\ x ->
                 S3ReferenceDataSourceDescription' <$>
                   (x .: "BucketARN") <*> (x .: "FileKey") <*>
                     (x .: "ReferenceRoleARN"))

instance Hashable S3ReferenceDataSourceDescription
         where

instance NFData S3ReferenceDataSourceDescription
         where

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
--
--
-- /See:/ 's3ReferenceDataSourceUpdate' smart constructor.
data S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate'
  { _srdsuBucketARNUpdate        :: !(Maybe Text)
  , _srdsuFileKeyUpdate          :: !(Maybe Text)
  , _srdsuReferenceRoleARNUpdate :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3ReferenceDataSourceUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdsuBucketARNUpdate' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- * 'srdsuFileKeyUpdate' - Object key name.
--
-- * 'srdsuReferenceRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
s3ReferenceDataSourceUpdate
    :: S3ReferenceDataSourceUpdate
s3ReferenceDataSourceUpdate =
  S3ReferenceDataSourceUpdate'
    { _srdsuBucketARNUpdate = Nothing
    , _srdsuFileKeyUpdate = Nothing
    , _srdsuReferenceRoleARNUpdate = Nothing
    }


-- | Amazon Resource Name (ARN) of the S3 bucket.
srdsuBucketARNUpdate :: Lens' S3ReferenceDataSourceUpdate (Maybe Text)
srdsuBucketARNUpdate = lens _srdsuBucketARNUpdate (\ s a -> s{_srdsuBucketARNUpdate = a})

-- | Object key name.
srdsuFileKeyUpdate :: Lens' S3ReferenceDataSourceUpdate (Maybe Text)
srdsuFileKeyUpdate = lens _srdsuFileKeyUpdate (\ s a -> s{_srdsuFileKeyUpdate = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
srdsuReferenceRoleARNUpdate :: Lens' S3ReferenceDataSourceUpdate (Maybe Text)
srdsuReferenceRoleARNUpdate = lens _srdsuReferenceRoleARNUpdate (\ s a -> s{_srdsuReferenceRoleARNUpdate = a})

instance Hashable S3ReferenceDataSourceUpdate where

instance NFData S3ReferenceDataSourceUpdate where

instance ToJSON S3ReferenceDataSourceUpdate where
        toJSON S3ReferenceDataSourceUpdate'{..}
          = object
              (catMaybes
                 [("BucketARNUpdate" .=) <$> _srdsuBucketARNUpdate,
                  ("FileKeyUpdate" .=) <$> _srdsuFileKeyUpdate,
                  ("ReferenceRoleARNUpdate" .=) <$>
                    _srdsuReferenceRoleARNUpdate])

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
--
--
-- /See:/ 'sourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { _ssRecordEncoding :: !(Maybe Text)
  , _ssRecordFormat   :: !RecordFormat
  , _ssRecordColumns  :: !(List1 RecordColumn)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssRecordEncoding' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- * 'ssRecordFormat' - Specifies the format of the records on the streaming source.
--
-- * 'ssRecordColumns' - A list of @RecordColumn@ objects.
sourceSchema
    :: RecordFormat -- ^ 'ssRecordFormat'
    -> NonEmpty RecordColumn -- ^ 'ssRecordColumns'
    -> SourceSchema
sourceSchema pRecordFormat_ pRecordColumns_ =
  SourceSchema'
    { _ssRecordEncoding = Nothing
    , _ssRecordFormat = pRecordFormat_
    , _ssRecordColumns = _List1 # pRecordColumns_
    }


-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
ssRecordEncoding :: Lens' SourceSchema (Maybe Text)
ssRecordEncoding = lens _ssRecordEncoding (\ s a -> s{_ssRecordEncoding = a})

-- | Specifies the format of the records on the streaming source.
ssRecordFormat :: Lens' SourceSchema RecordFormat
ssRecordFormat = lens _ssRecordFormat (\ s a -> s{_ssRecordFormat = a})

-- | A list of @RecordColumn@ objects.
ssRecordColumns :: Lens' SourceSchema (NonEmpty RecordColumn)
ssRecordColumns = lens _ssRecordColumns (\ s a -> s{_ssRecordColumns = a}) . _List1

instance FromJSON SourceSchema where
        parseJSON
          = withObject "SourceSchema"
              (\ x ->
                 SourceSchema' <$>
                   (x .:? "RecordEncoding") <*> (x .: "RecordFormat")
                     <*> (x .: "RecordColumns"))

instance Hashable SourceSchema where

instance NFData SourceSchema where

instance ToJSON SourceSchema where
        toJSON SourceSchema'{..}
          = object
              (catMaybes
                 [("RecordEncoding" .=) <$> _ssRecordEncoding,
                  Just ("RecordFormat" .= _ssRecordFormat),
                  Just ("RecordColumns" .= _ssRecordColumns)])
