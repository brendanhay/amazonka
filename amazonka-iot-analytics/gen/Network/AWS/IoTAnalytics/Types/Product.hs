{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Product where

import Network.AWS.IoTAnalytics.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that adds other attributes based on existing attributes in the message.
--
--
--
-- /See:/ 'addAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { _aaaNext       :: !(Maybe Text)
  , _aaaName       :: !Text
  , _aaaAttributes :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaaNext' - The next activity in the pipeline.
--
-- * 'aaaName' - The name of the 'addAttributes' activity.
--
-- * 'aaaAttributes' - A list of 1-50 "AttributeNameMapping" objects that map an existing attribute to a new attribute.
addAttributesActivity
    :: Text -- ^ 'aaaName'
    -> AddAttributesActivity
addAttributesActivity pName_ =
  AddAttributesActivity'
    {_aaaNext = Nothing, _aaaName = pName_, _aaaAttributes = mempty}


-- | The next activity in the pipeline.
aaaNext :: Lens' AddAttributesActivity (Maybe Text)
aaaNext = lens _aaaNext (\ s a -> s{_aaaNext = a})

-- | The name of the 'addAttributes' activity.
aaaName :: Lens' AddAttributesActivity Text
aaaName = lens _aaaName (\ s a -> s{_aaaName = a})

-- | A list of 1-50 "AttributeNameMapping" objects that map an existing attribute to a new attribute.
aaaAttributes :: Lens' AddAttributesActivity (HashMap Text Text)
aaaAttributes = lens _aaaAttributes (\ s a -> s{_aaaAttributes = a}) . _Map

instance FromJSON AddAttributesActivity where
        parseJSON
          = withObject "AddAttributesActivity"
              (\ x ->
                 AddAttributesActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .:? "attributes" .!= mempty))

instance Hashable AddAttributesActivity where

instance NFData AddAttributesActivity where

instance ToJSON AddAttributesActivity where
        toJSON AddAttributesActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _aaaNext, Just ("name" .= _aaaName),
                  Just ("attributes" .= _aaaAttributes)])

-- | Contains informations about errors.
--
--
--
-- /See:/ 'batchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { _bpmeeErrorCode    :: !(Maybe Text)
  , _bpmeeErrorMessage :: !(Maybe Text)
  , _bpmeeMessageId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchPutMessageErrorEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmeeErrorCode' - The code associated with the error.
--
-- * 'bpmeeErrorMessage' - The message associated with the error.
--
-- * 'bpmeeMessageId' - The ID of the message that caused the error. (See the value corresponding to the "messageId" key in the message object.)
batchPutMessageErrorEntry
    :: BatchPutMessageErrorEntry
batchPutMessageErrorEntry =
  BatchPutMessageErrorEntry'
    { _bpmeeErrorCode = Nothing
    , _bpmeeErrorMessage = Nothing
    , _bpmeeMessageId = Nothing
    }


-- | The code associated with the error.
bpmeeErrorCode :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeErrorCode = lens _bpmeeErrorCode (\ s a -> s{_bpmeeErrorCode = a})

-- | The message associated with the error.
bpmeeErrorMessage :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeErrorMessage = lens _bpmeeErrorMessage (\ s a -> s{_bpmeeErrorMessage = a})

-- | The ID of the message that caused the error. (See the value corresponding to the "messageId" key in the message object.)
bpmeeMessageId :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeMessageId = lens _bpmeeMessageId (\ s a -> s{_bpmeeMessageId = a})

instance FromJSON BatchPutMessageErrorEntry where
        parseJSON
          = withObject "BatchPutMessageErrorEntry"
              (\ x ->
                 BatchPutMessageErrorEntry' <$>
                   (x .:? "errorCode") <*> (x .:? "errorMessage") <*>
                     (x .:? "messageId"))

instance Hashable BatchPutMessageErrorEntry where

instance NFData BatchPutMessageErrorEntry where

-- | A collection of data from an MQTT topic. Channels archive the raw, unprocessed messages before publishing the data to a pipeline.
--
--
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cCreationTime    :: !(Maybe POSIX)
  , _cStatus          :: !(Maybe ChannelStatus)
  , _cArn             :: !(Maybe Text)
  , _cRetentionPeriod :: !(Maybe RetentionPeriod)
  , _cName            :: !(Maybe Text)
  , _cLastUpdateTime  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - When the channel was created.
--
-- * 'cStatus' - The status of the channel.
--
-- * 'cArn' - The ARN of the channel.
--
-- * 'cRetentionPeriod' - How long, in days, message data is kept for the channel.
--
-- * 'cName' - The name of the channel.
--
-- * 'cLastUpdateTime' - When the channel was last updated.
channel
    :: Channel
channel =
  Channel'
    { _cCreationTime = Nothing
    , _cStatus = Nothing
    , _cArn = Nothing
    , _cRetentionPeriod = Nothing
    , _cName = Nothing
    , _cLastUpdateTime = Nothing
    }


-- | When the channel was created.
cCreationTime :: Lens' Channel (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\ s a -> s{_cCreationTime = a}) . mapping _Time

-- | The status of the channel.
cStatus :: Lens' Channel (Maybe ChannelStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The ARN of the channel.
cArn :: Lens' Channel (Maybe Text)
cArn = lens _cArn (\ s a -> s{_cArn = a})

-- | How long, in days, message data is kept for the channel.
cRetentionPeriod :: Lens' Channel (Maybe RetentionPeriod)
cRetentionPeriod = lens _cRetentionPeriod (\ s a -> s{_cRetentionPeriod = a})

-- | The name of the channel.
cName :: Lens' Channel (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | When the channel was last updated.
cLastUpdateTime :: Lens' Channel (Maybe UTCTime)
cLastUpdateTime = lens _cLastUpdateTime (\ s a -> s{_cLastUpdateTime = a}) . mapping _Time

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ x ->
                 Channel' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "arn")
                     <*> (x .:? "retentionPeriod")
                     <*> (x .:? "name")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable Channel where

instance NFData Channel where

-- | The activity that determines the source of the messages to be processed.
--
--
--
-- /See:/ 'channelActivity' smart constructor.
data ChannelActivity = ChannelActivity'
  { _caNext        :: !(Maybe Text)
  , _caName        :: !Text
  , _caChannelName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChannelActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caNext' - The next activity in the pipeline.
--
-- * 'caName' - The name of the 'channel' activity.
--
-- * 'caChannelName' - The name of the channel from which the messages are processed.
channelActivity
    :: Text -- ^ 'caName'
    -> Text -- ^ 'caChannelName'
    -> ChannelActivity
channelActivity pName_ pChannelName_ =
  ChannelActivity'
    {_caNext = Nothing, _caName = pName_, _caChannelName = pChannelName_}


-- | The next activity in the pipeline.
caNext :: Lens' ChannelActivity (Maybe Text)
caNext = lens _caNext (\ s a -> s{_caNext = a})

-- | The name of the 'channel' activity.
caName :: Lens' ChannelActivity Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | The name of the channel from which the messages are processed.
caChannelName :: Lens' ChannelActivity Text
caChannelName = lens _caChannelName (\ s a -> s{_caChannelName = a})

instance FromJSON ChannelActivity where
        parseJSON
          = withObject "ChannelActivity"
              (\ x ->
                 ChannelActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "channelName"))

instance Hashable ChannelActivity where

instance NFData ChannelActivity where

instance ToJSON ChannelActivity where
        toJSON ChannelActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _caNext, Just ("name" .= _caName),
                  Just ("channelName" .= _caChannelName)])

-- | Statistics information about the channel.
--
--
--
-- /See:/ 'channelStatistics' smart constructor.
newtype ChannelStatistics = ChannelStatistics'
  { _csSize :: Maybe EstimatedResourceSize
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChannelStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSize' - The estimated size of the channel.
channelStatistics
    :: ChannelStatistics
channelStatistics = ChannelStatistics' {_csSize = Nothing}


-- | The estimated size of the channel.
csSize :: Lens' ChannelStatistics (Maybe EstimatedResourceSize)
csSize = lens _csSize (\ s a -> s{_csSize = a})

instance FromJSON ChannelStatistics where
        parseJSON
          = withObject "ChannelStatistics"
              (\ x -> ChannelStatistics' <$> (x .:? "size"))

instance Hashable ChannelStatistics where

instance NFData ChannelStatistics where

-- | A summary of information about a channel.
--
--
--
-- /See:/ 'channelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { _csCreationTime   :: !(Maybe POSIX)
  , _csStatus         :: !(Maybe ChannelStatus)
  , _csChannelName    :: !(Maybe Text)
  , _csLastUpdateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCreationTime' - When the channel was created.
--
-- * 'csStatus' - The status of the channel.
--
-- * 'csChannelName' - The name of the channel.
--
-- * 'csLastUpdateTime' - The last time the channel was updated.
channelSummary
    :: ChannelSummary
channelSummary =
  ChannelSummary'
    { _csCreationTime = Nothing
    , _csStatus = Nothing
    , _csChannelName = Nothing
    , _csLastUpdateTime = Nothing
    }


-- | When the channel was created.
csCreationTime :: Lens' ChannelSummary (Maybe UTCTime)
csCreationTime = lens _csCreationTime (\ s a -> s{_csCreationTime = a}) . mapping _Time

-- | The status of the channel.
csStatus :: Lens' ChannelSummary (Maybe ChannelStatus)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | The name of the channel.
csChannelName :: Lens' ChannelSummary (Maybe Text)
csChannelName = lens _csChannelName (\ s a -> s{_csChannelName = a})

-- | The last time the channel was updated.
csLastUpdateTime :: Lens' ChannelSummary (Maybe UTCTime)
csLastUpdateTime = lens _csLastUpdateTime (\ s a -> s{_csLastUpdateTime = a}) . mapping _Time

instance FromJSON ChannelSummary where
        parseJSON
          = withObject "ChannelSummary"
              (\ x ->
                 ChannelSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "channelName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable ChannelSummary where

instance NFData ChannelSummary where

-- | Information needed to run the "containerAction" to produce data set contents.
--
--
--
-- /See:/ 'containerDatasetAction' smart constructor.
data ContainerDatasetAction = ContainerDatasetAction'
  { _cdaVariables             :: !(Maybe [Variable])
  , _cdaImage                 :: !Text
  , _cdaExecutionRoleARN      :: !Text
  , _cdaResourceConfiguration :: !ResourceConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerDatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdaVariables' - The values of variables used within the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of "stringValue", "datasetContentVersionValue", or "outputFileUriValue".
--
-- * 'cdaImage' - The ARN of the Docker container stored in your account. The Docker container contains an application and needed support libraries and is used to generate data set contents.
--
-- * 'cdaExecutionRoleARN' - The ARN of the role which gives permission to the system to access needed resources in order to run the "containerAction". This includes, at minimum, permission to retrieve the data set contents which are the input to the containerized application.
--
-- * 'cdaResourceConfiguration' - Configuration of the resource which executes the "containerAction".
containerDatasetAction
    :: Text -- ^ 'cdaImage'
    -> Text -- ^ 'cdaExecutionRoleARN'
    -> ResourceConfiguration -- ^ 'cdaResourceConfiguration'
    -> ContainerDatasetAction
containerDatasetAction pImage_ pExecutionRoleARN_ pResourceConfiguration_ =
  ContainerDatasetAction'
    { _cdaVariables = Nothing
    , _cdaImage = pImage_
    , _cdaExecutionRoleARN = pExecutionRoleARN_
    , _cdaResourceConfiguration = pResourceConfiguration_
    }


-- | The values of variables used within the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of "stringValue", "datasetContentVersionValue", or "outputFileUriValue".
cdaVariables :: Lens' ContainerDatasetAction [Variable]
cdaVariables = lens _cdaVariables (\ s a -> s{_cdaVariables = a}) . _Default . _Coerce

-- | The ARN of the Docker container stored in your account. The Docker container contains an application and needed support libraries and is used to generate data set contents.
cdaImage :: Lens' ContainerDatasetAction Text
cdaImage = lens _cdaImage (\ s a -> s{_cdaImage = a})

-- | The ARN of the role which gives permission to the system to access needed resources in order to run the "containerAction". This includes, at minimum, permission to retrieve the data set contents which are the input to the containerized application.
cdaExecutionRoleARN :: Lens' ContainerDatasetAction Text
cdaExecutionRoleARN = lens _cdaExecutionRoleARN (\ s a -> s{_cdaExecutionRoleARN = a})

-- | Configuration of the resource which executes the "containerAction".
cdaResourceConfiguration :: Lens' ContainerDatasetAction ResourceConfiguration
cdaResourceConfiguration = lens _cdaResourceConfiguration (\ s a -> s{_cdaResourceConfiguration = a})

instance FromJSON ContainerDatasetAction where
        parseJSON
          = withObject "ContainerDatasetAction"
              (\ x ->
                 ContainerDatasetAction' <$>
                   (x .:? "variables" .!= mempty) <*> (x .: "image") <*>
                     (x .: "executionRoleArn")
                     <*> (x .: "resourceConfiguration"))

instance Hashable ContainerDatasetAction where

instance NFData ContainerDatasetAction where

instance ToJSON ContainerDatasetAction where
        toJSON ContainerDatasetAction'{..}
          = object
              (catMaybes
                 [("variables" .=) <$> _cdaVariables,
                  Just ("image" .= _cdaImage),
                  Just ("executionRoleArn" .= _cdaExecutionRoleARN),
                  Just
                    ("resourceConfiguration" .=
                       _cdaResourceConfiguration)])

-- | Information about a data set.
--
--
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'
  { _dCreationTime         :: !(Maybe POSIX)
  , _dStatus               :: !(Maybe DatasetStatus)
  , _dArn                  :: !(Maybe Text)
  , _dActions              :: !(Maybe (List1 DatasetAction))
  , _dTriggers             :: !(Maybe [DatasetTrigger])
  , _dRetentionPeriod      :: !(Maybe RetentionPeriod)
  , _dName                 :: !(Maybe Text)
  , _dContentDeliveryRules :: !(Maybe [DatasetContentDeliveryRule])
  , _dLastUpdateTime       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCreationTime' - When the data set was created.
--
-- * 'dStatus' - The status of the data set.
--
-- * 'dArn' - The ARN of the data set.
--
-- * 'dActions' - The "DatasetAction" objects that automatically create the data set contents.
--
-- * 'dTriggers' - The "DatasetTrigger" objects that specify when the data set is automatically updated.
--
-- * 'dRetentionPeriod' - [Optional] How long, in days, message data is kept for the data set.
--
-- * 'dName' - The name of the data set.
--
-- * 'dContentDeliveryRules' - When data set contents are created they are delivered to destinations specified here.
--
-- * 'dLastUpdateTime' - The last time the data set was updated.
dataset
    :: Dataset
dataset =
  Dataset'
    { _dCreationTime = Nothing
    , _dStatus = Nothing
    , _dArn = Nothing
    , _dActions = Nothing
    , _dTriggers = Nothing
    , _dRetentionPeriod = Nothing
    , _dName = Nothing
    , _dContentDeliveryRules = Nothing
    , _dLastUpdateTime = Nothing
    }


-- | When the data set was created.
dCreationTime :: Lens' Dataset (Maybe UTCTime)
dCreationTime = lens _dCreationTime (\ s a -> s{_dCreationTime = a}) . mapping _Time

-- | The status of the data set.
dStatus :: Lens' Dataset (Maybe DatasetStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The ARN of the data set.
dArn :: Lens' Dataset (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a})

-- | The "DatasetAction" objects that automatically create the data set contents.
dActions :: Lens' Dataset (Maybe (NonEmpty DatasetAction))
dActions = lens _dActions (\ s a -> s{_dActions = a}) . mapping _List1

-- | The "DatasetTrigger" objects that specify when the data set is automatically updated.
dTriggers :: Lens' Dataset [DatasetTrigger]
dTriggers = lens _dTriggers (\ s a -> s{_dTriggers = a}) . _Default . _Coerce

-- | [Optional] How long, in days, message data is kept for the data set.
dRetentionPeriod :: Lens' Dataset (Maybe RetentionPeriod)
dRetentionPeriod = lens _dRetentionPeriod (\ s a -> s{_dRetentionPeriod = a})

-- | The name of the data set.
dName :: Lens' Dataset (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | When data set contents are created they are delivered to destinations specified here.
dContentDeliveryRules :: Lens' Dataset [DatasetContentDeliveryRule]
dContentDeliveryRules = lens _dContentDeliveryRules (\ s a -> s{_dContentDeliveryRules = a}) . _Default . _Coerce

-- | The last time the data set was updated.
dLastUpdateTime :: Lens' Dataset (Maybe UTCTime)
dLastUpdateTime = lens _dLastUpdateTime (\ s a -> s{_dLastUpdateTime = a}) . mapping _Time

instance FromJSON Dataset where
        parseJSON
          = withObject "Dataset"
              (\ x ->
                 Dataset' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "arn")
                     <*> (x .:? "actions")
                     <*> (x .:? "triggers" .!= mempty)
                     <*> (x .:? "retentionPeriod")
                     <*> (x .:? "name")
                     <*> (x .:? "contentDeliveryRules" .!= mempty)
                     <*> (x .:? "lastUpdateTime"))

instance Hashable Dataset where

instance NFData Dataset where

-- | A "DatasetAction" object that specifies how data set contents are automatically created.
--
--
--
-- /See:/ 'datasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { _daQueryAction     :: !(Maybe SqlQueryDatasetAction)
  , _daActionName      :: !(Maybe Text)
  , _daContainerAction :: !(Maybe ContainerDatasetAction)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daQueryAction' - An "SqlQueryDatasetAction" object that uses an SQL query to automatically create data set contents.
--
-- * 'daActionName' - The name of the data set action by which data set contents are automatically created.
--
-- * 'daContainerAction' - Information which allows the system to run a containerized application in order to create the data set contents. The application must be in a Docker container along with any needed support libraries.
datasetAction
    :: DatasetAction
datasetAction =
  DatasetAction'
    { _daQueryAction = Nothing
    , _daActionName = Nothing
    , _daContainerAction = Nothing
    }


-- | An "SqlQueryDatasetAction" object that uses an SQL query to automatically create data set contents.
daQueryAction :: Lens' DatasetAction (Maybe SqlQueryDatasetAction)
daQueryAction = lens _daQueryAction (\ s a -> s{_daQueryAction = a})

-- | The name of the data set action by which data set contents are automatically created.
daActionName :: Lens' DatasetAction (Maybe Text)
daActionName = lens _daActionName (\ s a -> s{_daActionName = a})

-- | Information which allows the system to run a containerized application in order to create the data set contents. The application must be in a Docker container along with any needed support libraries.
daContainerAction :: Lens' DatasetAction (Maybe ContainerDatasetAction)
daContainerAction = lens _daContainerAction (\ s a -> s{_daContainerAction = a})

instance FromJSON DatasetAction where
        parseJSON
          = withObject "DatasetAction"
              (\ x ->
                 DatasetAction' <$>
                   (x .:? "queryAction") <*> (x .:? "actionName") <*>
                     (x .:? "containerAction"))

instance Hashable DatasetAction where

instance NFData DatasetAction where

instance ToJSON DatasetAction where
        toJSON DatasetAction'{..}
          = object
              (catMaybes
                 [("queryAction" .=) <$> _daQueryAction,
                  ("actionName" .=) <$> _daActionName,
                  ("containerAction" .=) <$> _daContainerAction])

-- |
--
--
--
-- /See:/ 'datasetActionSummary' smart constructor.
data DatasetActionSummary = DatasetActionSummary'
  { _dasActionName :: !(Maybe Text)
  , _dasActionType :: !(Maybe DatasetActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetActionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasActionName' - The name of the action which automatically creates the data set's contents.
--
-- * 'dasActionType' - The type of action by which the data set's contents are automatically created.
datasetActionSummary
    :: DatasetActionSummary
datasetActionSummary =
  DatasetActionSummary' {_dasActionName = Nothing, _dasActionType = Nothing}


-- | The name of the action which automatically creates the data set's contents.
dasActionName :: Lens' DatasetActionSummary (Maybe Text)
dasActionName = lens _dasActionName (\ s a -> s{_dasActionName = a})

-- | The type of action by which the data set's contents are automatically created.
dasActionType :: Lens' DatasetActionSummary (Maybe DatasetActionType)
dasActionType = lens _dasActionType (\ s a -> s{_dasActionType = a})

instance FromJSON DatasetActionSummary where
        parseJSON
          = withObject "DatasetActionSummary"
              (\ x ->
                 DatasetActionSummary' <$>
                   (x .:? "actionName") <*> (x .:? "actionType"))

instance Hashable DatasetActionSummary where

instance NFData DatasetActionSummary where

-- | The destination to which data set contents are delivered.
--
--
--
-- /See:/ 'datasetContentDeliveryDestination' smart constructor.
newtype DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { _dcddIotEventsDestinationConfiguration :: Maybe IotEventsDestinationConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetContentDeliveryDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcddIotEventsDestinationConfiguration' - Configuration information for delivery of data set contents to AWS IoT Events.
datasetContentDeliveryDestination
    :: DatasetContentDeliveryDestination
datasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    {_dcddIotEventsDestinationConfiguration = Nothing}


-- | Configuration information for delivery of data set contents to AWS IoT Events.
dcddIotEventsDestinationConfiguration :: Lens' DatasetContentDeliveryDestination (Maybe IotEventsDestinationConfiguration)
dcddIotEventsDestinationConfiguration = lens _dcddIotEventsDestinationConfiguration (\ s a -> s{_dcddIotEventsDestinationConfiguration = a})

instance FromJSON DatasetContentDeliveryDestination
         where
        parseJSON
          = withObject "DatasetContentDeliveryDestination"
              (\ x ->
                 DatasetContentDeliveryDestination' <$>
                   (x .:? "iotEventsDestinationConfiguration"))

instance Hashable DatasetContentDeliveryDestination
         where

instance NFData DatasetContentDeliveryDestination
         where

instance ToJSON DatasetContentDeliveryDestination
         where
        toJSON DatasetContentDeliveryDestination'{..}
          = object
              (catMaybes
                 [("iotEventsDestinationConfiguration" .=) <$>
                    _dcddIotEventsDestinationConfiguration])

-- | When data set contents are created they are delivered to destination specified here.
--
--
--
-- /See:/ 'datasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { _dcdrEntryName   :: !(Maybe Text)
  , _dcdrDestination :: !DatasetContentDeliveryDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetContentDeliveryRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrEntryName' - The name of the data set content delivery rules entry.
--
-- * 'dcdrDestination' - The destination to which data set contents are delivered.
datasetContentDeliveryRule
    :: DatasetContentDeliveryDestination -- ^ 'dcdrDestination'
    -> DatasetContentDeliveryRule
datasetContentDeliveryRule pDestination_ =
  DatasetContentDeliveryRule'
    {_dcdrEntryName = Nothing, _dcdrDestination = pDestination_}


-- | The name of the data set content delivery rules entry.
dcdrEntryName :: Lens' DatasetContentDeliveryRule (Maybe Text)
dcdrEntryName = lens _dcdrEntryName (\ s a -> s{_dcdrEntryName = a})

-- | The destination to which data set contents are delivered.
dcdrDestination :: Lens' DatasetContentDeliveryRule DatasetContentDeliveryDestination
dcdrDestination = lens _dcdrDestination (\ s a -> s{_dcdrDestination = a})

instance FromJSON DatasetContentDeliveryRule where
        parseJSON
          = withObject "DatasetContentDeliveryRule"
              (\ x ->
                 DatasetContentDeliveryRule' <$>
                   (x .:? "entryName") <*> (x .: "destination"))

instance Hashable DatasetContentDeliveryRule where

instance NFData DatasetContentDeliveryRule where

instance ToJSON DatasetContentDeliveryRule where
        toJSON DatasetContentDeliveryRule'{..}
          = object
              (catMaybes
                 [("entryName" .=) <$> _dcdrEntryName,
                  Just ("destination" .= _dcdrDestination)])

-- | The state of the data set contents and the reason they are in this state.
--
--
--
-- /See:/ 'datasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { _dcsState  :: !(Maybe DatasetContentState)
  , _dcsReason :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetContentStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsState' - The state of the data set contents. Can be one of "READY", "CREATING", "SUCCEEDED" or "FAILED".
--
-- * 'dcsReason' - The reason the data set contents are in this state.
datasetContentStatus
    :: DatasetContentStatus
datasetContentStatus =
  DatasetContentStatus' {_dcsState = Nothing, _dcsReason = Nothing}


-- | The state of the data set contents. Can be one of "READY", "CREATING", "SUCCEEDED" or "FAILED".
dcsState :: Lens' DatasetContentStatus (Maybe DatasetContentState)
dcsState = lens _dcsState (\ s a -> s{_dcsState = a})

-- | The reason the data set contents are in this state.
dcsReason :: Lens' DatasetContentStatus (Maybe Text)
dcsReason = lens _dcsReason (\ s a -> s{_dcsReason = a})

instance FromJSON DatasetContentStatus where
        parseJSON
          = withObject "DatasetContentStatus"
              (\ x ->
                 DatasetContentStatus' <$>
                   (x .:? "state") <*> (x .:? "reason"))

instance Hashable DatasetContentStatus where

instance NFData DatasetContentStatus where

-- | Summary information about data set contents.
--
--
--
-- /See:/ 'datasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { _dcsCreationTime :: !(Maybe POSIX)
  , _dcsStatus       :: !(Maybe DatasetContentStatus)
  , _dcsScheduleTime :: !(Maybe POSIX)
  , _dcsVersion      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetContentSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsCreationTime' - The actual time the creation of the data set contents was started.
--
-- * 'dcsStatus' - The status of the data set contents.
--
-- * 'dcsScheduleTime' - The time the creation of the data set contents was scheduled to start.
--
-- * 'dcsVersion' - The version of the data set contents.
datasetContentSummary
    :: DatasetContentSummary
datasetContentSummary =
  DatasetContentSummary'
    { _dcsCreationTime = Nothing
    , _dcsStatus = Nothing
    , _dcsScheduleTime = Nothing
    , _dcsVersion = Nothing
    }


-- | The actual time the creation of the data set contents was started.
dcsCreationTime :: Lens' DatasetContentSummary (Maybe UTCTime)
dcsCreationTime = lens _dcsCreationTime (\ s a -> s{_dcsCreationTime = a}) . mapping _Time

-- | The status of the data set contents.
dcsStatus :: Lens' DatasetContentSummary (Maybe DatasetContentStatus)
dcsStatus = lens _dcsStatus (\ s a -> s{_dcsStatus = a})

-- | The time the creation of the data set contents was scheduled to start.
dcsScheduleTime :: Lens' DatasetContentSummary (Maybe UTCTime)
dcsScheduleTime = lens _dcsScheduleTime (\ s a -> s{_dcsScheduleTime = a}) . mapping _Time

-- | The version of the data set contents.
dcsVersion :: Lens' DatasetContentSummary (Maybe Text)
dcsVersion = lens _dcsVersion (\ s a -> s{_dcsVersion = a})

instance FromJSON DatasetContentSummary where
        parseJSON
          = withObject "DatasetContentSummary"
              (\ x ->
                 DatasetContentSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "scheduleTime")
                     <*> (x .:? "version"))

instance Hashable DatasetContentSummary where

instance NFData DatasetContentSummary where

-- | The data set whose latest contents are used as input to the notebook or application.
--
--
--
-- /See:/ 'datasetContentVersionValue' smart constructor.
newtype DatasetContentVersionValue = DatasetContentVersionValue'
  { _dcvvDatasetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetContentVersionValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvvDatasetName' - The name of the data set whose latest contents are used as input to the notebook or application.
datasetContentVersionValue
    :: Text -- ^ 'dcvvDatasetName'
    -> DatasetContentVersionValue
datasetContentVersionValue pDatasetName_ =
  DatasetContentVersionValue' {_dcvvDatasetName = pDatasetName_}


-- | The name of the data set whose latest contents are used as input to the notebook or application.
dcvvDatasetName :: Lens' DatasetContentVersionValue Text
dcvvDatasetName = lens _dcvvDatasetName (\ s a -> s{_dcvvDatasetName = a})

instance FromJSON DatasetContentVersionValue where
        parseJSON
          = withObject "DatasetContentVersionValue"
              (\ x ->
                 DatasetContentVersionValue' <$> (x .: "datasetName"))

instance Hashable DatasetContentVersionValue where

instance NFData DatasetContentVersionValue where

instance ToJSON DatasetContentVersionValue where
        toJSON DatasetContentVersionValue'{..}
          = object
              (catMaybes
                 [Just ("datasetName" .= _dcvvDatasetName)])

-- | The reference to a data set entry.
--
--
--
-- /See:/ 'datasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { _deEntryName :: !(Maybe Text)
  , _deDataURI   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEntryName' - The name of the data set item.
--
-- * 'deDataURI' - The pre-signed URI of the data set item.
datasetEntry
    :: DatasetEntry
datasetEntry = DatasetEntry' {_deEntryName = Nothing, _deDataURI = Nothing}


-- | The name of the data set item.
deEntryName :: Lens' DatasetEntry (Maybe Text)
deEntryName = lens _deEntryName (\ s a -> s{_deEntryName = a})

-- | The pre-signed URI of the data set item.
deDataURI :: Lens' DatasetEntry (Maybe Text)
deDataURI = lens _deDataURI (\ s a -> s{_deDataURI = a})

instance FromJSON DatasetEntry where
        parseJSON
          = withObject "DatasetEntry"
              (\ x ->
                 DatasetEntry' <$>
                   (x .:? "entryName") <*> (x .:? "dataURI"))

instance Hashable DatasetEntry where

instance NFData DatasetEntry where

-- | A summary of information about a data set.
--
--
--
-- /See:/ 'datasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { _dssCreationTime   :: !(Maybe POSIX)
  , _dssStatus         :: !(Maybe DatasetStatus)
  , _dssActions        :: !(Maybe (List1 DatasetActionSummary))
  , _dssTriggers       :: !(Maybe [DatasetTrigger])
  , _dssDatasetName    :: !(Maybe Text)
  , _dssLastUpdateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssCreationTime' - The time the data set was created.
--
-- * 'dssStatus' - The status of the data set.
--
-- * 'dssActions' - A list of "DataActionSummary" objects.
--
-- * 'dssTriggers' - A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five DataSetTrigger objects
--
-- * 'dssDatasetName' - The name of the data set.
--
-- * 'dssLastUpdateTime' - The last time the data set was updated.
datasetSummary
    :: DatasetSummary
datasetSummary =
  DatasetSummary'
    { _dssCreationTime = Nothing
    , _dssStatus = Nothing
    , _dssActions = Nothing
    , _dssTriggers = Nothing
    , _dssDatasetName = Nothing
    , _dssLastUpdateTime = Nothing
    }


-- | The time the data set was created.
dssCreationTime :: Lens' DatasetSummary (Maybe UTCTime)
dssCreationTime = lens _dssCreationTime (\ s a -> s{_dssCreationTime = a}) . mapping _Time

-- | The status of the data set.
dssStatus :: Lens' DatasetSummary (Maybe DatasetStatus)
dssStatus = lens _dssStatus (\ s a -> s{_dssStatus = a})

-- | A list of "DataActionSummary" objects.
dssActions :: Lens' DatasetSummary (Maybe (NonEmpty DatasetActionSummary))
dssActions = lens _dssActions (\ s a -> s{_dssActions = a}) . mapping _List1

-- | A list of triggers. A trigger causes data set content to be populated at a specified time interval or when another data set is populated. The list of triggers can be empty or contain up to five DataSetTrigger objects
dssTriggers :: Lens' DatasetSummary [DatasetTrigger]
dssTriggers = lens _dssTriggers (\ s a -> s{_dssTriggers = a}) . _Default . _Coerce

-- | The name of the data set.
dssDatasetName :: Lens' DatasetSummary (Maybe Text)
dssDatasetName = lens _dssDatasetName (\ s a -> s{_dssDatasetName = a})

-- | The last time the data set was updated.
dssLastUpdateTime :: Lens' DatasetSummary (Maybe UTCTime)
dssLastUpdateTime = lens _dssLastUpdateTime (\ s a -> s{_dssLastUpdateTime = a}) . mapping _Time

instance FromJSON DatasetSummary where
        parseJSON
          = withObject "DatasetSummary"
              (\ x ->
                 DatasetSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "actions")
                     <*> (x .:? "triggers" .!= mempty)
                     <*> (x .:? "datasetName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatasetSummary where

instance NFData DatasetSummary where

-- | The "DatasetTrigger" that specifies when the data set is automatically updated.
--
--
--
-- /See:/ 'datasetTrigger' smart constructor.
data DatasetTrigger = DatasetTrigger'
  { _dtDataset  :: !(Maybe TriggeringDataset)
  , _dtSchedule :: !(Maybe Schedule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDataset' - The data set whose content creation triggers the creation of this data set's contents.
--
-- * 'dtSchedule' - The "Schedule" when the trigger is initiated.
datasetTrigger
    :: DatasetTrigger
datasetTrigger = DatasetTrigger' {_dtDataset = Nothing, _dtSchedule = Nothing}


-- | The data set whose content creation triggers the creation of this data set's contents.
dtDataset :: Lens' DatasetTrigger (Maybe TriggeringDataset)
dtDataset = lens _dtDataset (\ s a -> s{_dtDataset = a})

-- | The "Schedule" when the trigger is initiated.
dtSchedule :: Lens' DatasetTrigger (Maybe Schedule)
dtSchedule = lens _dtSchedule (\ s a -> s{_dtSchedule = a})

instance FromJSON DatasetTrigger where
        parseJSON
          = withObject "DatasetTrigger"
              (\ x ->
                 DatasetTrigger' <$>
                   (x .:? "dataset") <*> (x .:? "schedule"))

instance Hashable DatasetTrigger where

instance NFData DatasetTrigger where

instance ToJSON DatasetTrigger where
        toJSON DatasetTrigger'{..}
          = object
              (catMaybes
                 [("dataset" .=) <$> _dtDataset,
                  ("schedule" .=) <$> _dtSchedule])

-- | Information about a data store.
--
--
--
-- /See:/ 'datastore' smart constructor.
data Datastore = Datastore'
  { _datCreationTime    :: !(Maybe POSIX)
  , _datStatus          :: !(Maybe DatastoreStatus)
  , _datArn             :: !(Maybe Text)
  , _datRetentionPeriod :: !(Maybe RetentionPeriod)
  , _datName            :: !(Maybe Text)
  , _datLastUpdateTime  :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Datastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datCreationTime' - When the data store was created.
--
-- * 'datStatus' - The status of a data store:     * CREATING    * The data store is being created.     * ACTIVE    * The data store has been created and can be used.     * DELETING    * The data store is being deleted.
--
-- * 'datArn' - The ARN of the data store.
--
-- * 'datRetentionPeriod' - How long, in days, message data is kept for the data store.
--
-- * 'datName' - The name of the data store.
--
-- * 'datLastUpdateTime' - The last time the data store was updated.
datastore
    :: Datastore
datastore =
  Datastore'
    { _datCreationTime = Nothing
    , _datStatus = Nothing
    , _datArn = Nothing
    , _datRetentionPeriod = Nothing
    , _datName = Nothing
    , _datLastUpdateTime = Nothing
    }


-- | When the data store was created.
datCreationTime :: Lens' Datastore (Maybe UTCTime)
datCreationTime = lens _datCreationTime (\ s a -> s{_datCreationTime = a}) . mapping _Time

-- | The status of a data store:     * CREATING    * The data store is being created.     * ACTIVE    * The data store has been created and can be used.     * DELETING    * The data store is being deleted.
datStatus :: Lens' Datastore (Maybe DatastoreStatus)
datStatus = lens _datStatus (\ s a -> s{_datStatus = a})

-- | The ARN of the data store.
datArn :: Lens' Datastore (Maybe Text)
datArn = lens _datArn (\ s a -> s{_datArn = a})

-- | How long, in days, message data is kept for the data store.
datRetentionPeriod :: Lens' Datastore (Maybe RetentionPeriod)
datRetentionPeriod = lens _datRetentionPeriod (\ s a -> s{_datRetentionPeriod = a})

-- | The name of the data store.
datName :: Lens' Datastore (Maybe Text)
datName = lens _datName (\ s a -> s{_datName = a})

-- | The last time the data store was updated.
datLastUpdateTime :: Lens' Datastore (Maybe UTCTime)
datLastUpdateTime = lens _datLastUpdateTime (\ s a -> s{_datLastUpdateTime = a}) . mapping _Time

instance FromJSON Datastore where
        parseJSON
          = withObject "Datastore"
              (\ x ->
                 Datastore' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "arn")
                     <*> (x .:? "retentionPeriod")
                     <*> (x .:? "name")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable Datastore where

instance NFData Datastore where

-- | The 'datastore' activity that specifies where to store the processed data.
--
--
--
-- /See:/ 'datastoreActivity' smart constructor.
data DatastoreActivity = DatastoreActivity'
  { _daName          :: !Text
  , _daDatastoreName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatastoreActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daName' - The name of the 'datastore' activity.
--
-- * 'daDatastoreName' - The name of the data store where processed messages are stored.
datastoreActivity
    :: Text -- ^ 'daName'
    -> Text -- ^ 'daDatastoreName'
    -> DatastoreActivity
datastoreActivity pName_ pDatastoreName_ =
  DatastoreActivity' {_daName = pName_, _daDatastoreName = pDatastoreName_}


-- | The name of the 'datastore' activity.
daName :: Lens' DatastoreActivity Text
daName = lens _daName (\ s a -> s{_daName = a})

-- | The name of the data store where processed messages are stored.
daDatastoreName :: Lens' DatastoreActivity Text
daDatastoreName = lens _daDatastoreName (\ s a -> s{_daDatastoreName = a})

instance FromJSON DatastoreActivity where
        parseJSON
          = withObject "DatastoreActivity"
              (\ x ->
                 DatastoreActivity' <$>
                   (x .: "name") <*> (x .: "datastoreName"))

instance Hashable DatastoreActivity where

instance NFData DatastoreActivity where

instance ToJSON DatastoreActivity where
        toJSON DatastoreActivity'{..}
          = object
              (catMaybes
                 [Just ("name" .= _daName),
                  Just ("datastoreName" .= _daDatastoreName)])

-- | Statistical information about the data store.
--
--
--
-- /See:/ 'datastoreStatistics' smart constructor.
newtype DatastoreStatistics = DatastoreStatistics'
  { _dsSize :: Maybe EstimatedResourceSize
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatastoreStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSize' - The estimated size of the data store.
datastoreStatistics
    :: DatastoreStatistics
datastoreStatistics = DatastoreStatistics' {_dsSize = Nothing}


-- | The estimated size of the data store.
dsSize :: Lens' DatastoreStatistics (Maybe EstimatedResourceSize)
dsSize = lens _dsSize (\ s a -> s{_dsSize = a})

instance FromJSON DatastoreStatistics where
        parseJSON
          = withObject "DatastoreStatistics"
              (\ x -> DatastoreStatistics' <$> (x .:? "size"))

instance Hashable DatastoreStatistics where

instance NFData DatastoreStatistics where

-- | A summary of information about a data store.
--
--
--
-- /See:/ 'datastoreSummary' smart constructor.
data DatastoreSummary = DatastoreSummary'
  { _dsCreationTime   :: !(Maybe POSIX)
  , _dsStatus         :: !(Maybe DatastoreStatus)
  , _dsDatastoreName  :: !(Maybe Text)
  , _dsLastUpdateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatastoreSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsCreationTime' - When the data store was created.
--
-- * 'dsStatus' - The status of the data store.
--
-- * 'dsDatastoreName' - The name of the data store.
--
-- * 'dsLastUpdateTime' - The last time the data store was updated.
datastoreSummary
    :: DatastoreSummary
datastoreSummary =
  DatastoreSummary'
    { _dsCreationTime = Nothing
    , _dsStatus = Nothing
    , _dsDatastoreName = Nothing
    , _dsLastUpdateTime = Nothing
    }


-- | When the data store was created.
dsCreationTime :: Lens' DatastoreSummary (Maybe UTCTime)
dsCreationTime = lens _dsCreationTime (\ s a -> s{_dsCreationTime = a}) . mapping _Time

-- | The status of the data store.
dsStatus :: Lens' DatastoreSummary (Maybe DatastoreStatus)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a})

-- | The name of the data store.
dsDatastoreName :: Lens' DatastoreSummary (Maybe Text)
dsDatastoreName = lens _dsDatastoreName (\ s a -> s{_dsDatastoreName = a})

-- | The last time the data store was updated.
dsLastUpdateTime :: Lens' DatastoreSummary (Maybe UTCTime)
dsLastUpdateTime = lens _dsLastUpdateTime (\ s a -> s{_dsLastUpdateTime = a}) . mapping _Time

instance FromJSON DatastoreSummary where
        parseJSON
          = withObject "DatastoreSummary"
              (\ x ->
                 DatastoreSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "datastoreName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatastoreSummary where

instance NFData DatastoreSummary where

-- | Used to limit data to that which has arrived since the last execution of the action.
--
--
--
-- /See:/ 'deltaTime' smart constructor.
data DeltaTime = DeltaTime'
  { _dtOffsetSeconds  :: !Int
  , _dtTimeExpression :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeltaTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtOffsetSeconds' - The number of seconds of estimated "in flight" lag time of message data. When you create data set contents using message data from a specified time frame, some message data may still be "in flight" when processing begins, and so will not arrive in time to be processed. Use this field to make allowances for the "in flight" time of your message data, so that data not processed from a previous time frame will be included with the next time frame. Without this, missed message data would be excluded from processing during the next time frame as well, because its timestamp places it within the previous time frame.
--
-- * 'dtTimeExpression' - An expression by which the time of the message data may be determined. This may be the name of a timestamp field, or a SQL expression which is used to derive the time the message data was generated.
deltaTime
    :: Int -- ^ 'dtOffsetSeconds'
    -> Text -- ^ 'dtTimeExpression'
    -> DeltaTime
deltaTime pOffsetSeconds_ pTimeExpression_ =
  DeltaTime'
    {_dtOffsetSeconds = pOffsetSeconds_, _dtTimeExpression = pTimeExpression_}


-- | The number of seconds of estimated "in flight" lag time of message data. When you create data set contents using message data from a specified time frame, some message data may still be "in flight" when processing begins, and so will not arrive in time to be processed. Use this field to make allowances for the "in flight" time of your message data, so that data not processed from a previous time frame will be included with the next time frame. Without this, missed message data would be excluded from processing during the next time frame as well, because its timestamp places it within the previous time frame.
dtOffsetSeconds :: Lens' DeltaTime Int
dtOffsetSeconds = lens _dtOffsetSeconds (\ s a -> s{_dtOffsetSeconds = a})

-- | An expression by which the time of the message data may be determined. This may be the name of a timestamp field, or a SQL expression which is used to derive the time the message data was generated.
dtTimeExpression :: Lens' DeltaTime Text
dtTimeExpression = lens _dtTimeExpression (\ s a -> s{_dtTimeExpression = a})

instance FromJSON DeltaTime where
        parseJSON
          = withObject "DeltaTime"
              (\ x ->
                 DeltaTime' <$>
                   (x .: "offsetSeconds") <*> (x .: "timeExpression"))

instance Hashable DeltaTime where

instance NFData DeltaTime where

instance ToJSON DeltaTime where
        toJSON DeltaTime'{..}
          = object
              (catMaybes
                 [Just ("offsetSeconds" .= _dtOffsetSeconds),
                  Just ("timeExpression" .= _dtTimeExpression)])

-- | An activity that adds data from the AWS IoT device registry to your message.
--
--
--
-- /See:/ 'deviceRegistryEnrichActivity' smart constructor.
data DeviceRegistryEnrichActivity = DeviceRegistryEnrichActivity'
  { _dreaNext      :: !(Maybe Text)
  , _dreaName      :: !Text
  , _dreaAttribute :: !Text
  , _dreaThingName :: !Text
  , _dreaRoleARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceRegistryEnrichActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreaNext' - The next activity in the pipeline.
--
-- * 'dreaName' - The name of the 'deviceRegistryEnrich' activity.
--
-- * 'dreaAttribute' - The name of the attribute that is added to the message.
--
-- * 'dreaThingName' - The name of the IoT device whose registry information is added to the message.
--
-- * 'dreaRoleARN' - The ARN of the role that allows access to the device's registry information.
deviceRegistryEnrichActivity
    :: Text -- ^ 'dreaName'
    -> Text -- ^ 'dreaAttribute'
    -> Text -- ^ 'dreaThingName'
    -> Text -- ^ 'dreaRoleARN'
    -> DeviceRegistryEnrichActivity
deviceRegistryEnrichActivity pName_ pAttribute_ pThingName_ pRoleARN_ =
  DeviceRegistryEnrichActivity'
    { _dreaNext = Nothing
    , _dreaName = pName_
    , _dreaAttribute = pAttribute_
    , _dreaThingName = pThingName_
    , _dreaRoleARN = pRoleARN_
    }


-- | The next activity in the pipeline.
dreaNext :: Lens' DeviceRegistryEnrichActivity (Maybe Text)
dreaNext = lens _dreaNext (\ s a -> s{_dreaNext = a})

-- | The name of the 'deviceRegistryEnrich' activity.
dreaName :: Lens' DeviceRegistryEnrichActivity Text
dreaName = lens _dreaName (\ s a -> s{_dreaName = a})

-- | The name of the attribute that is added to the message.
dreaAttribute :: Lens' DeviceRegistryEnrichActivity Text
dreaAttribute = lens _dreaAttribute (\ s a -> s{_dreaAttribute = a})

-- | The name of the IoT device whose registry information is added to the message.
dreaThingName :: Lens' DeviceRegistryEnrichActivity Text
dreaThingName = lens _dreaThingName (\ s a -> s{_dreaThingName = a})

-- | The ARN of the role that allows access to the device's registry information.
dreaRoleARN :: Lens' DeviceRegistryEnrichActivity Text
dreaRoleARN = lens _dreaRoleARN (\ s a -> s{_dreaRoleARN = a})

instance FromJSON DeviceRegistryEnrichActivity where
        parseJSON
          = withObject "DeviceRegistryEnrichActivity"
              (\ x ->
                 DeviceRegistryEnrichActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "attribute")
                     <*> (x .: "thingName")
                     <*> (x .: "roleArn"))

instance Hashable DeviceRegistryEnrichActivity where

instance NFData DeviceRegistryEnrichActivity where

instance ToJSON DeviceRegistryEnrichActivity where
        toJSON DeviceRegistryEnrichActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _dreaNext,
                  Just ("name" .= _dreaName),
                  Just ("attribute" .= _dreaAttribute),
                  Just ("thingName" .= _dreaThingName),
                  Just ("roleArn" .= _dreaRoleARN)])

-- | An activity that adds information from the AWS IoT Device Shadows service to a message.
--
--
--
-- /See:/ 'deviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { _dseaNext      :: !(Maybe Text)
  , _dseaName      :: !Text
  , _dseaAttribute :: !Text
  , _dseaThingName :: !Text
  , _dseaRoleARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceShadowEnrichActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseaNext' - The next activity in the pipeline.
--
-- * 'dseaName' - The name of the 'deviceShadowEnrich' activity.
--
-- * 'dseaAttribute' - The name of the attribute that is added to the message.
--
-- * 'dseaThingName' - The name of the IoT device whose shadow information is added to the message.
--
-- * 'dseaRoleARN' - The ARN of the role that allows access to the device's shadow.
deviceShadowEnrichActivity
    :: Text -- ^ 'dseaName'
    -> Text -- ^ 'dseaAttribute'
    -> Text -- ^ 'dseaThingName'
    -> Text -- ^ 'dseaRoleARN'
    -> DeviceShadowEnrichActivity
deviceShadowEnrichActivity pName_ pAttribute_ pThingName_ pRoleARN_ =
  DeviceShadowEnrichActivity'
    { _dseaNext = Nothing
    , _dseaName = pName_
    , _dseaAttribute = pAttribute_
    , _dseaThingName = pThingName_
    , _dseaRoleARN = pRoleARN_
    }


-- | The next activity in the pipeline.
dseaNext :: Lens' DeviceShadowEnrichActivity (Maybe Text)
dseaNext = lens _dseaNext (\ s a -> s{_dseaNext = a})

-- | The name of the 'deviceShadowEnrich' activity.
dseaName :: Lens' DeviceShadowEnrichActivity Text
dseaName = lens _dseaName (\ s a -> s{_dseaName = a})

-- | The name of the attribute that is added to the message.
dseaAttribute :: Lens' DeviceShadowEnrichActivity Text
dseaAttribute = lens _dseaAttribute (\ s a -> s{_dseaAttribute = a})

-- | The name of the IoT device whose shadow information is added to the message.
dseaThingName :: Lens' DeviceShadowEnrichActivity Text
dseaThingName = lens _dseaThingName (\ s a -> s{_dseaThingName = a})

-- | The ARN of the role that allows access to the device's shadow.
dseaRoleARN :: Lens' DeviceShadowEnrichActivity Text
dseaRoleARN = lens _dseaRoleARN (\ s a -> s{_dseaRoleARN = a})

instance FromJSON DeviceShadowEnrichActivity where
        parseJSON
          = withObject "DeviceShadowEnrichActivity"
              (\ x ->
                 DeviceShadowEnrichActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "attribute")
                     <*> (x .: "thingName")
                     <*> (x .: "roleArn"))

instance Hashable DeviceShadowEnrichActivity where

instance NFData DeviceShadowEnrichActivity where

instance ToJSON DeviceShadowEnrichActivity where
        toJSON DeviceShadowEnrichActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _dseaNext,
                  Just ("name" .= _dseaName),
                  Just ("attribute" .= _dseaAttribute),
                  Just ("thingName" .= _dseaThingName),
                  Just ("roleArn" .= _dseaRoleARN)])

-- | The estimated size of the resource.
--
--
--
-- /See:/ 'estimatedResourceSize' smart constructor.
data EstimatedResourceSize = EstimatedResourceSize'
  { _ersEstimatedOn          :: !(Maybe POSIX)
  , _ersEstimatedSizeInBytes :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EstimatedResourceSize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ersEstimatedOn' - The time when the estimate of the size of the resource was made.
--
-- * 'ersEstimatedSizeInBytes' - The estimated size of the resource in bytes.
estimatedResourceSize
    :: EstimatedResourceSize
estimatedResourceSize =
  EstimatedResourceSize'
    {_ersEstimatedOn = Nothing, _ersEstimatedSizeInBytes = Nothing}


-- | The time when the estimate of the size of the resource was made.
ersEstimatedOn :: Lens' EstimatedResourceSize (Maybe UTCTime)
ersEstimatedOn = lens _ersEstimatedOn (\ s a -> s{_ersEstimatedOn = a}) . mapping _Time

-- | The estimated size of the resource in bytes.
ersEstimatedSizeInBytes :: Lens' EstimatedResourceSize (Maybe Double)
ersEstimatedSizeInBytes = lens _ersEstimatedSizeInBytes (\ s a -> s{_ersEstimatedSizeInBytes = a})

instance FromJSON EstimatedResourceSize where
        parseJSON
          = withObject "EstimatedResourceSize"
              (\ x ->
                 EstimatedResourceSize' <$>
                   (x .:? "estimatedOn") <*>
                     (x .:? "estimatedSizeInBytes"))

instance Hashable EstimatedResourceSize where

instance NFData EstimatedResourceSize where

-- | An activity that filters a message based on its attributes.
--
--
--
-- /See:/ 'filterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { _faNext   :: !(Maybe Text)
  , _faName   :: !Text
  , _faFilter :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FilterActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faNext' - The next activity in the pipeline.
--
-- * 'faName' - The name of the 'filter' activity.
--
-- * 'faFilter' - An expression that looks like a SQL WHERE clause that must return a Boolean value.
filterActivity
    :: Text -- ^ 'faName'
    -> Text -- ^ 'faFilter'
    -> FilterActivity
filterActivity pName_ pFilter_ =
  FilterActivity' {_faNext = Nothing, _faName = pName_, _faFilter = pFilter_}


-- | The next activity in the pipeline.
faNext :: Lens' FilterActivity (Maybe Text)
faNext = lens _faNext (\ s a -> s{_faNext = a})

-- | The name of the 'filter' activity.
faName :: Lens' FilterActivity Text
faName = lens _faName (\ s a -> s{_faName = a})

-- | An expression that looks like a SQL WHERE clause that must return a Boolean value.
faFilter :: Lens' FilterActivity Text
faFilter = lens _faFilter (\ s a -> s{_faFilter = a})

instance FromJSON FilterActivity where
        parseJSON
          = withObject "FilterActivity"
              (\ x ->
                 FilterActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*> (x .: "filter"))

instance Hashable FilterActivity where

instance NFData FilterActivity where

instance ToJSON FilterActivity where
        toJSON FilterActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _faNext, Just ("name" .= _faName),
                  Just ("filter" .= _faFilter)])

-- | Configuration information for delivery of data set contents to AWS IoT Events.
--
--
--
-- /See:/ 'iotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { _iedcInputName :: !Text
  , _iedcRoleARN   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IotEventsDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iedcInputName' - The name of the AWS IoT Events input to which data set contents are delivered.
--
-- * 'iedcRoleARN' - The ARN of the role which grants AWS IoT Analytics permission to deliver data set contents to an AWS IoT Events input.
iotEventsDestinationConfiguration
    :: Text -- ^ 'iedcInputName'
    -> Text -- ^ 'iedcRoleARN'
    -> IotEventsDestinationConfiguration
iotEventsDestinationConfiguration pInputName_ pRoleARN_ =
  IotEventsDestinationConfiguration'
    {_iedcInputName = pInputName_, _iedcRoleARN = pRoleARN_}


-- | The name of the AWS IoT Events input to which data set contents are delivered.
iedcInputName :: Lens' IotEventsDestinationConfiguration Text
iedcInputName = lens _iedcInputName (\ s a -> s{_iedcInputName = a})

-- | The ARN of the role which grants AWS IoT Analytics permission to deliver data set contents to an AWS IoT Events input.
iedcRoleARN :: Lens' IotEventsDestinationConfiguration Text
iedcRoleARN = lens _iedcRoleARN (\ s a -> s{_iedcRoleARN = a})

instance FromJSON IotEventsDestinationConfiguration
         where
        parseJSON
          = withObject "IotEventsDestinationConfiguration"
              (\ x ->
                 IotEventsDestinationConfiguration' <$>
                   (x .: "inputName") <*> (x .: "roleArn"))

instance Hashable IotEventsDestinationConfiguration
         where

instance NFData IotEventsDestinationConfiguration
         where

instance ToJSON IotEventsDestinationConfiguration
         where
        toJSON IotEventsDestinationConfiguration'{..}
          = object
              (catMaybes
                 [Just ("inputName" .= _iedcInputName),
                  Just ("roleArn" .= _iedcRoleARN)])

-- | An activity that runs a Lambda function to modify the message.
--
--
--
-- /See:/ 'lambdaActivity' smart constructor.
data LambdaActivity = LambdaActivity'
  { _laNext       :: !(Maybe Text)
  , _laName       :: !Text
  , _laLambdaName :: !Text
  , _laBatchSize  :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNext' - The next activity in the pipeline.
--
-- * 'laName' - The name of the 'lambda' activity.
--
-- * 'laLambdaName' - The name of the Lambda function that is run on the message.
--
-- * 'laBatchSize' - The number of messages passed to the Lambda function for processing. The AWS Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
lambdaActivity
    :: Text -- ^ 'laName'
    -> Text -- ^ 'laLambdaName'
    -> Natural -- ^ 'laBatchSize'
    -> LambdaActivity
lambdaActivity pName_ pLambdaName_ pBatchSize_ =
  LambdaActivity'
    { _laNext = Nothing
    , _laName = pName_
    , _laLambdaName = pLambdaName_
    , _laBatchSize = _Nat # pBatchSize_
    }


-- | The next activity in the pipeline.
laNext :: Lens' LambdaActivity (Maybe Text)
laNext = lens _laNext (\ s a -> s{_laNext = a})

-- | The name of the 'lambda' activity.
laName :: Lens' LambdaActivity Text
laName = lens _laName (\ s a -> s{_laName = a})

-- | The name of the Lambda function that is run on the message.
laLambdaName :: Lens' LambdaActivity Text
laLambdaName = lens _laLambdaName (\ s a -> s{_laLambdaName = a})

-- | The number of messages passed to the Lambda function for processing. The AWS Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
laBatchSize :: Lens' LambdaActivity Natural
laBatchSize = lens _laBatchSize (\ s a -> s{_laBatchSize = a}) . _Nat

instance FromJSON LambdaActivity where
        parseJSON
          = withObject "LambdaActivity"
              (\ x ->
                 LambdaActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "lambdaName")
                     <*> (x .: "batchSize"))

instance Hashable LambdaActivity where

instance NFData LambdaActivity where

instance ToJSON LambdaActivity where
        toJSON LambdaActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _laNext, Just ("name" .= _laName),
                  Just ("lambdaName" .= _laLambdaName),
                  Just ("batchSize" .= _laBatchSize)])

-- | Information about logging options.
--
--
--
-- /See:/ 'loggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { _loRoleARN :: !Text
  , _loLevel   :: !LoggingLevel
  , _loEnabled :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loRoleARN' - The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
--
-- * 'loLevel' - The logging level. Currently, only "ERROR" is supported.
--
-- * 'loEnabled' - If true, logging is enabled for AWS IoT Analytics.
loggingOptions
    :: Text -- ^ 'loRoleARN'
    -> LoggingLevel -- ^ 'loLevel'
    -> Bool -- ^ 'loEnabled'
    -> LoggingOptions
loggingOptions pRoleARN_ pLevel_ pEnabled_ =
  LoggingOptions'
    {_loRoleARN = pRoleARN_, _loLevel = pLevel_, _loEnabled = pEnabled_}


-- | The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
loRoleARN :: Lens' LoggingOptions Text
loRoleARN = lens _loRoleARN (\ s a -> s{_loRoleARN = a})

-- | The logging level. Currently, only "ERROR" is supported.
loLevel :: Lens' LoggingOptions LoggingLevel
loLevel = lens _loLevel (\ s a -> s{_loLevel = a})

-- | If true, logging is enabled for AWS IoT Analytics.
loEnabled :: Lens' LoggingOptions Bool
loEnabled = lens _loEnabled (\ s a -> s{_loEnabled = a})

instance FromJSON LoggingOptions where
        parseJSON
          = withObject "LoggingOptions"
              (\ x ->
                 LoggingOptions' <$>
                   (x .: "roleArn") <*> (x .: "level") <*>
                     (x .: "enabled"))

instance Hashable LoggingOptions where

instance NFData LoggingOptions where

instance ToJSON LoggingOptions where
        toJSON LoggingOptions'{..}
          = object
              (catMaybes
                 [Just ("roleArn" .= _loRoleARN),
                  Just ("level" .= _loLevel),
                  Just ("enabled" .= _loEnabled)])

-- | An activity that computes an arithmetic expression using the message's attributes.
--
--
--
-- /See:/ 'mathActivity' smart constructor.
data MathActivity = MathActivity'
  { _maNext      :: !(Maybe Text)
  , _maName      :: !Text
  , _maAttribute :: !Text
  , _maMath      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MathActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maNext' - The next activity in the pipeline.
--
-- * 'maName' - The name of the 'math' activity.
--
-- * 'maAttribute' - The name of the attribute that contains the result of the math operation.
--
-- * 'maMath' - An expression that uses one or more existing attributes and must return an integer value.
mathActivity
    :: Text -- ^ 'maName'
    -> Text -- ^ 'maAttribute'
    -> Text -- ^ 'maMath'
    -> MathActivity
mathActivity pName_ pAttribute_ pMath_ =
  MathActivity'
    { _maNext = Nothing
    , _maName = pName_
    , _maAttribute = pAttribute_
    , _maMath = pMath_
    }


-- | The next activity in the pipeline.
maNext :: Lens' MathActivity (Maybe Text)
maNext = lens _maNext (\ s a -> s{_maNext = a})

-- | The name of the 'math' activity.
maName :: Lens' MathActivity Text
maName = lens _maName (\ s a -> s{_maName = a})

-- | The name of the attribute that contains the result of the math operation.
maAttribute :: Lens' MathActivity Text
maAttribute = lens _maAttribute (\ s a -> s{_maAttribute = a})

-- | An expression that uses one or more existing attributes and must return an integer value.
maMath :: Lens' MathActivity Text
maMath = lens _maMath (\ s a -> s{_maMath = a})

instance FromJSON MathActivity where
        parseJSON
          = withObject "MathActivity"
              (\ x ->
                 MathActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "attribute")
                     <*> (x .: "math"))

instance Hashable MathActivity where

instance NFData MathActivity where

instance ToJSON MathActivity where
        toJSON MathActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _maNext, Just ("name" .= _maName),
                  Just ("attribute" .= _maAttribute),
                  Just ("math" .= _maMath)])

-- | Information about a message.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mMessageId :: !Text
  , _mPayload   :: !Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMessageId' - The ID you wish to assign to the message. Each "messageId" must be unique within each batch sent.
--
-- * 'mPayload' - The payload of the message. This may be a JSON string or a Base64-encoded string representing binary data (in which case you must decode it by means of a pipeline activity).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
message
    :: Text -- ^ 'mMessageId'
    -> ByteString -- ^ 'mPayload'
    -> Message
message pMessageId_ pPayload_ =
  Message' {_mMessageId = pMessageId_, _mPayload = _Base64 # pPayload_}


-- | The ID you wish to assign to the message. Each "messageId" must be unique within each batch sent.
mMessageId :: Lens' Message Text
mMessageId = lens _mMessageId (\ s a -> s{_mMessageId = a})

-- | The payload of the message. This may be a JSON string or a Base64-encoded string representing binary data (in which case you must decode it by means of a pipeline activity).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mPayload :: Lens' Message ByteString
mPayload = lens _mPayload (\ s a -> s{_mPayload = a}) . _Base64

instance Hashable Message where

instance NFData Message where

instance ToJSON Message where
        toJSON Message'{..}
          = object
              (catMaybes
                 [Just ("messageId" .= _mMessageId),
                  Just ("payload" .= _mPayload)])

-- | The value of the variable as a structure that specifies an output file URI.
--
--
--
-- /See:/ 'outputFileURIValue' smart constructor.
newtype OutputFileURIValue = OutputFileURIValue'
  { _ofuvFileName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputFileURIValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofuvFileName' - The URI of the location where data set contents are stored, usually the URI of a file in an S3 bucket.
outputFileURIValue
    :: Text -- ^ 'ofuvFileName'
    -> OutputFileURIValue
outputFileURIValue pFileName_ = OutputFileURIValue' {_ofuvFileName = pFileName_}


-- | The URI of the location where data set contents are stored, usually the URI of a file in an S3 bucket.
ofuvFileName :: Lens' OutputFileURIValue Text
ofuvFileName = lens _ofuvFileName (\ s a -> s{_ofuvFileName = a})

instance FromJSON OutputFileURIValue where
        parseJSON
          = withObject "OutputFileURIValue"
              (\ x -> OutputFileURIValue' <$> (x .: "fileName"))

instance Hashable OutputFileURIValue where

instance NFData OutputFileURIValue where

instance ToJSON OutputFileURIValue where
        toJSON OutputFileURIValue'{..}
          = object
              (catMaybes [Just ("fileName" .= _ofuvFileName)])

-- | Contains information about a pipeline.
--
--
--
-- /See:/ 'pipeline' smart constructor.
data Pipeline = Pipeline'
  { _pCreationTime          :: !(Maybe POSIX)
  , _pArn                   :: !(Maybe Text)
  , _pActivities            :: !(Maybe (List1 PipelineActivity))
  , _pName                  :: !(Maybe Text)
  , _pReprocessingSummaries :: !(Maybe [ReprocessingSummary])
  , _pLastUpdateTime        :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Pipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pCreationTime' - When the pipeline was created.
--
-- * 'pArn' - The ARN of the pipeline.
--
-- * 'pActivities' - The activities that perform transformations on the messages.
--
-- * 'pName' - The name of the pipeline.
--
-- * 'pReprocessingSummaries' - A summary of information about the pipeline reprocessing.
--
-- * 'pLastUpdateTime' - The last time the pipeline was updated.
pipeline
    :: Pipeline
pipeline =
  Pipeline'
    { _pCreationTime = Nothing
    , _pArn = Nothing
    , _pActivities = Nothing
    , _pName = Nothing
    , _pReprocessingSummaries = Nothing
    , _pLastUpdateTime = Nothing
    }


-- | When the pipeline was created.
pCreationTime :: Lens' Pipeline (Maybe UTCTime)
pCreationTime = lens _pCreationTime (\ s a -> s{_pCreationTime = a}) . mapping _Time

-- | The ARN of the pipeline.
pArn :: Lens' Pipeline (Maybe Text)
pArn = lens _pArn (\ s a -> s{_pArn = a})

-- | The activities that perform transformations on the messages.
pActivities :: Lens' Pipeline (Maybe (NonEmpty PipelineActivity))
pActivities = lens _pActivities (\ s a -> s{_pActivities = a}) . mapping _List1

-- | The name of the pipeline.
pName :: Lens' Pipeline (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | A summary of information about the pipeline reprocessing.
pReprocessingSummaries :: Lens' Pipeline [ReprocessingSummary]
pReprocessingSummaries = lens _pReprocessingSummaries (\ s a -> s{_pReprocessingSummaries = a}) . _Default . _Coerce

-- | The last time the pipeline was updated.
pLastUpdateTime :: Lens' Pipeline (Maybe UTCTime)
pLastUpdateTime = lens _pLastUpdateTime (\ s a -> s{_pLastUpdateTime = a}) . mapping _Time

instance FromJSON Pipeline where
        parseJSON
          = withObject "Pipeline"
              (\ x ->
                 Pipeline' <$>
                   (x .:? "creationTime") <*> (x .:? "arn") <*>
                     (x .:? "activities")
                     <*> (x .:? "name")
                     <*> (x .:? "reprocessingSummaries" .!= mempty)
                     <*> (x .:? "lastUpdateTime"))

instance Hashable Pipeline where

instance NFData Pipeline where

-- | An activity that performs a transformation on a message.
--
--
--
-- /See:/ 'pipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { _paSelectAttributes     :: !(Maybe SelectAttributesActivity)
  , _paChannel              :: !(Maybe ChannelActivity)
  , _paAddAttributes        :: !(Maybe AddAttributesActivity)
  , _paDeviceRegistryEnrich :: !(Maybe DeviceRegistryEnrichActivity)
  , _paRemoveAttributes     :: !(Maybe RemoveAttributesActivity)
  , _paLambda               :: !(Maybe LambdaActivity)
  , _paDatastore            :: !(Maybe DatastoreActivity)
  , _paDeviceShadowEnrich   :: !(Maybe DeviceShadowEnrichActivity)
  , _paFilter               :: !(Maybe FilterActivity)
  , _paMath                 :: !(Maybe MathActivity)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paSelectAttributes' - Creates a new message using only the specified attributes from the original message.
--
-- * 'paChannel' - Determines the source of the messages to be processed.
--
-- * 'paAddAttributes' - Adds other attributes based on existing attributes in the message.
--
-- * 'paDeviceRegistryEnrich' - Adds data from the AWS IoT device registry to your message.
--
-- * 'paRemoveAttributes' - Removes attributes from a message.
--
-- * 'paLambda' - Runs a Lambda function to modify the message.
--
-- * 'paDatastore' - Specifies where to store the processed message data.
--
-- * 'paDeviceShadowEnrich' - Adds information from the AWS IoT Device Shadows service to a message.
--
-- * 'paFilter' - Filters a message based on its attributes.
--
-- * 'paMath' - Computes an arithmetic expression using the message's attributes and adds it to the message.
pipelineActivity
    :: PipelineActivity
pipelineActivity =
  PipelineActivity'
    { _paSelectAttributes = Nothing
    , _paChannel = Nothing
    , _paAddAttributes = Nothing
    , _paDeviceRegistryEnrich = Nothing
    , _paRemoveAttributes = Nothing
    , _paLambda = Nothing
    , _paDatastore = Nothing
    , _paDeviceShadowEnrich = Nothing
    , _paFilter = Nothing
    , _paMath = Nothing
    }


-- | Creates a new message using only the specified attributes from the original message.
paSelectAttributes :: Lens' PipelineActivity (Maybe SelectAttributesActivity)
paSelectAttributes = lens _paSelectAttributes (\ s a -> s{_paSelectAttributes = a})

-- | Determines the source of the messages to be processed.
paChannel :: Lens' PipelineActivity (Maybe ChannelActivity)
paChannel = lens _paChannel (\ s a -> s{_paChannel = a})

-- | Adds other attributes based on existing attributes in the message.
paAddAttributes :: Lens' PipelineActivity (Maybe AddAttributesActivity)
paAddAttributes = lens _paAddAttributes (\ s a -> s{_paAddAttributes = a})

-- | Adds data from the AWS IoT device registry to your message.
paDeviceRegistryEnrich :: Lens' PipelineActivity (Maybe DeviceRegistryEnrichActivity)
paDeviceRegistryEnrich = lens _paDeviceRegistryEnrich (\ s a -> s{_paDeviceRegistryEnrich = a})

-- | Removes attributes from a message.
paRemoveAttributes :: Lens' PipelineActivity (Maybe RemoveAttributesActivity)
paRemoveAttributes = lens _paRemoveAttributes (\ s a -> s{_paRemoveAttributes = a})

-- | Runs a Lambda function to modify the message.
paLambda :: Lens' PipelineActivity (Maybe LambdaActivity)
paLambda = lens _paLambda (\ s a -> s{_paLambda = a})

-- | Specifies where to store the processed message data.
paDatastore :: Lens' PipelineActivity (Maybe DatastoreActivity)
paDatastore = lens _paDatastore (\ s a -> s{_paDatastore = a})

-- | Adds information from the AWS IoT Device Shadows service to a message.
paDeviceShadowEnrich :: Lens' PipelineActivity (Maybe DeviceShadowEnrichActivity)
paDeviceShadowEnrich = lens _paDeviceShadowEnrich (\ s a -> s{_paDeviceShadowEnrich = a})

-- | Filters a message based on its attributes.
paFilter :: Lens' PipelineActivity (Maybe FilterActivity)
paFilter = lens _paFilter (\ s a -> s{_paFilter = a})

-- | Computes an arithmetic expression using the message's attributes and adds it to the message.
paMath :: Lens' PipelineActivity (Maybe MathActivity)
paMath = lens _paMath (\ s a -> s{_paMath = a})

instance FromJSON PipelineActivity where
        parseJSON
          = withObject "PipelineActivity"
              (\ x ->
                 PipelineActivity' <$>
                   (x .:? "selectAttributes") <*> (x .:? "channel") <*>
                     (x .:? "addAttributes")
                     <*> (x .:? "deviceRegistryEnrich")
                     <*> (x .:? "removeAttributes")
                     <*> (x .:? "lambda")
                     <*> (x .:? "datastore")
                     <*> (x .:? "deviceShadowEnrich")
                     <*> (x .:? "filter")
                     <*> (x .:? "math"))

instance Hashable PipelineActivity where

instance NFData PipelineActivity where

instance ToJSON PipelineActivity where
        toJSON PipelineActivity'{..}
          = object
              (catMaybes
                 [("selectAttributes" .=) <$> _paSelectAttributes,
                  ("channel" .=) <$> _paChannel,
                  ("addAttributes" .=) <$> _paAddAttributes,
                  ("deviceRegistryEnrich" .=) <$>
                    _paDeviceRegistryEnrich,
                  ("removeAttributes" .=) <$> _paRemoveAttributes,
                  ("lambda" .=) <$> _paLambda,
                  ("datastore" .=) <$> _paDatastore,
                  ("deviceShadowEnrich" .=) <$> _paDeviceShadowEnrich,
                  ("filter" .=) <$> _paFilter,
                  ("math" .=) <$> _paMath])

-- | A summary of information about a pipeline.
--
--
--
-- /See:/ 'pipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { _psCreationTime          :: !(Maybe POSIX)
  , _psPipelineName          :: !(Maybe Text)
  , _psReprocessingSummaries :: !(Maybe [ReprocessingSummary])
  , _psLastUpdateTime        :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCreationTime' - When the pipeline was created.
--
-- * 'psPipelineName' - The name of the pipeline.
--
-- * 'psReprocessingSummaries' - A summary of information about the pipeline reprocessing.
--
-- * 'psLastUpdateTime' - When the pipeline was last updated.
pipelineSummary
    :: PipelineSummary
pipelineSummary =
  PipelineSummary'
    { _psCreationTime = Nothing
    , _psPipelineName = Nothing
    , _psReprocessingSummaries = Nothing
    , _psLastUpdateTime = Nothing
    }


-- | When the pipeline was created.
psCreationTime :: Lens' PipelineSummary (Maybe UTCTime)
psCreationTime = lens _psCreationTime (\ s a -> s{_psCreationTime = a}) . mapping _Time

-- | The name of the pipeline.
psPipelineName :: Lens' PipelineSummary (Maybe Text)
psPipelineName = lens _psPipelineName (\ s a -> s{_psPipelineName = a})

-- | A summary of information about the pipeline reprocessing.
psReprocessingSummaries :: Lens' PipelineSummary [ReprocessingSummary]
psReprocessingSummaries = lens _psReprocessingSummaries (\ s a -> s{_psReprocessingSummaries = a}) . _Default . _Coerce

-- | When the pipeline was last updated.
psLastUpdateTime :: Lens' PipelineSummary (Maybe UTCTime)
psLastUpdateTime = lens _psLastUpdateTime (\ s a -> s{_psLastUpdateTime = a}) . mapping _Time

instance FromJSON PipelineSummary where
        parseJSON
          = withObject "PipelineSummary"
              (\ x ->
                 PipelineSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "pipelineName") <*>
                     (x .:? "reprocessingSummaries" .!= mempty)
                     <*> (x .:? "lastUpdateTime"))

instance Hashable PipelineSummary where

instance NFData PipelineSummary where

-- | Information which is used to filter message data, to segregate it according to the time frame in which it arrives.
--
--
--
-- /See:/ 'queryFilter' smart constructor.
newtype QueryFilter = QueryFilter'
  { _qfDeltaTime :: Maybe DeltaTime
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qfDeltaTime' - Used to limit data to that which has arrived since the last execution of the action.
queryFilter
    :: QueryFilter
queryFilter = QueryFilter' {_qfDeltaTime = Nothing}


-- | Used to limit data to that which has arrived since the last execution of the action.
qfDeltaTime :: Lens' QueryFilter (Maybe DeltaTime)
qfDeltaTime = lens _qfDeltaTime (\ s a -> s{_qfDeltaTime = a})

instance FromJSON QueryFilter where
        parseJSON
          = withObject "QueryFilter"
              (\ x -> QueryFilter' <$> (x .:? "deltaTime"))

instance Hashable QueryFilter where

instance NFData QueryFilter where

instance ToJSON QueryFilter where
        toJSON QueryFilter'{..}
          = object
              (catMaybes [("deltaTime" .=) <$> _qfDeltaTime])

-- | An activity that removes attributes from a message.
--
--
--
-- /See:/ 'removeAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { _raaNext       :: !(Maybe Text)
  , _raaName       :: !Text
  , _raaAttributes :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raaNext' - The next activity in the pipeline.
--
-- * 'raaName' - The name of the 'removeAttributes' activity.
--
-- * 'raaAttributes' - A list of 1-50 attributes to remove from the message.
removeAttributesActivity
    :: Text -- ^ 'raaName'
    -> NonEmpty Text -- ^ 'raaAttributes'
    -> RemoveAttributesActivity
removeAttributesActivity pName_ pAttributes_ =
  RemoveAttributesActivity'
    { _raaNext = Nothing
    , _raaName = pName_
    , _raaAttributes = _List1 # pAttributes_
    }


-- | The next activity in the pipeline.
raaNext :: Lens' RemoveAttributesActivity (Maybe Text)
raaNext = lens _raaNext (\ s a -> s{_raaNext = a})

-- | The name of the 'removeAttributes' activity.
raaName :: Lens' RemoveAttributesActivity Text
raaName = lens _raaName (\ s a -> s{_raaName = a})

-- | A list of 1-50 attributes to remove from the message.
raaAttributes :: Lens' RemoveAttributesActivity (NonEmpty Text)
raaAttributes = lens _raaAttributes (\ s a -> s{_raaAttributes = a}) . _List1

instance FromJSON RemoveAttributesActivity where
        parseJSON
          = withObject "RemoveAttributesActivity"
              (\ x ->
                 RemoveAttributesActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "attributes"))

instance Hashable RemoveAttributesActivity where

instance NFData RemoveAttributesActivity where

instance ToJSON RemoveAttributesActivity where
        toJSON RemoveAttributesActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _raaNext, Just ("name" .= _raaName),
                  Just ("attributes" .= _raaAttributes)])

-- | Information about pipeline reprocessing.
--
--
--
-- /See:/ 'reprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { _rsCreationTime :: !(Maybe POSIX)
  , _rsStatus       :: !(Maybe ReprocessingStatus)
  , _rsId           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReprocessingSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsCreationTime' - The time the pipeline reprocessing was created.
--
-- * 'rsStatus' - The status of the pipeline reprocessing.
--
-- * 'rsId' - The 'reprocessingId' returned by "StartPipelineReprocessing".
reprocessingSummary
    :: ReprocessingSummary
reprocessingSummary =
  ReprocessingSummary'
    {_rsCreationTime = Nothing, _rsStatus = Nothing, _rsId = Nothing}


-- | The time the pipeline reprocessing was created.
rsCreationTime :: Lens' ReprocessingSummary (Maybe UTCTime)
rsCreationTime = lens _rsCreationTime (\ s a -> s{_rsCreationTime = a}) . mapping _Time

-- | The status of the pipeline reprocessing.
rsStatus :: Lens' ReprocessingSummary (Maybe ReprocessingStatus)
rsStatus = lens _rsStatus (\ s a -> s{_rsStatus = a})

-- | The 'reprocessingId' returned by "StartPipelineReprocessing".
rsId :: Lens' ReprocessingSummary (Maybe Text)
rsId = lens _rsId (\ s a -> s{_rsId = a})

instance FromJSON ReprocessingSummary where
        parseJSON
          = withObject "ReprocessingSummary"
              (\ x ->
                 ReprocessingSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "id"))

instance Hashable ReprocessingSummary where

instance NFData ReprocessingSummary where

-- | The configuration of the resource used to execute the "containerAction".
--
--
--
-- /See:/ 'resourceConfiguration' smart constructor.
data ResourceConfiguration = ResourceConfiguration'
  { _rcComputeType    :: !ComputeType
  , _rcVolumeSizeInGB :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcComputeType' - The type of the compute resource used to execute the "containerAction". Possible values are: ACU_1 (vCPU=4, memory=16GiB) or ACU_2 (vCPU=8, memory=32GiB).
--
-- * 'rcVolumeSizeInGB' - The size (in GB) of the persistent storage available to the resource instance used to execute the "containerAction" (min: 1, max: 50).
resourceConfiguration
    :: ComputeType -- ^ 'rcComputeType'
    -> Natural -- ^ 'rcVolumeSizeInGB'
    -> ResourceConfiguration
resourceConfiguration pComputeType_ pVolumeSizeInGB_ =
  ResourceConfiguration'
    { _rcComputeType = pComputeType_
    , _rcVolumeSizeInGB = _Nat # pVolumeSizeInGB_
    }


-- | The type of the compute resource used to execute the "containerAction". Possible values are: ACU_1 (vCPU=4, memory=16GiB) or ACU_2 (vCPU=8, memory=32GiB).
rcComputeType :: Lens' ResourceConfiguration ComputeType
rcComputeType = lens _rcComputeType (\ s a -> s{_rcComputeType = a})

-- | The size (in GB) of the persistent storage available to the resource instance used to execute the "containerAction" (min: 1, max: 50).
rcVolumeSizeInGB :: Lens' ResourceConfiguration Natural
rcVolumeSizeInGB = lens _rcVolumeSizeInGB (\ s a -> s{_rcVolumeSizeInGB = a}) . _Nat

instance FromJSON ResourceConfiguration where
        parseJSON
          = withObject "ResourceConfiguration"
              (\ x ->
                 ResourceConfiguration' <$>
                   (x .: "computeType") <*> (x .: "volumeSizeInGB"))

instance Hashable ResourceConfiguration where

instance NFData ResourceConfiguration where

instance ToJSON ResourceConfiguration where
        toJSON ResourceConfiguration'{..}
          = object
              (catMaybes
                 [Just ("computeType" .= _rcComputeType),
                  Just ("volumeSizeInGB" .= _rcVolumeSizeInGB)])

-- | How long, in days, message data is kept.
--
--
--
-- /See:/ 'retentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { _rpUnlimited    :: !(Maybe Bool)
  , _rpNumberOfDays :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetentionPeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpUnlimited' - If true, message data is kept indefinitely.
--
-- * 'rpNumberOfDays' - The number of days that message data is kept. The "unlimited" parameter must be false.
retentionPeriod
    :: RetentionPeriod
retentionPeriod =
  RetentionPeriod' {_rpUnlimited = Nothing, _rpNumberOfDays = Nothing}


-- | If true, message data is kept indefinitely.
rpUnlimited :: Lens' RetentionPeriod (Maybe Bool)
rpUnlimited = lens _rpUnlimited (\ s a -> s{_rpUnlimited = a})

-- | The number of days that message data is kept. The "unlimited" parameter must be false.
rpNumberOfDays :: Lens' RetentionPeriod (Maybe Natural)
rpNumberOfDays = lens _rpNumberOfDays (\ s a -> s{_rpNumberOfDays = a}) . mapping _Nat

instance FromJSON RetentionPeriod where
        parseJSON
          = withObject "RetentionPeriod"
              (\ x ->
                 RetentionPeriod' <$>
                   (x .:? "unlimited") <*> (x .:? "numberOfDays"))

instance Hashable RetentionPeriod where

instance NFData RetentionPeriod where

instance ToJSON RetentionPeriod where
        toJSON RetentionPeriod'{..}
          = object
              (catMaybes
                 [("unlimited" .=) <$> _rpUnlimited,
                  ("numberOfDays" .=) <$> _rpNumberOfDays])

-- | The schedule for when to trigger an update.
--
--
--
-- /See:/ 'schedule' smart constructor.
newtype Schedule = Schedule'
  { _sExpression :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sExpression' - The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the Amazon CloudWatch documentation.
schedule
    :: Schedule
schedule = Schedule' {_sExpression = Nothing}


-- | The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the Amazon CloudWatch documentation.
sExpression :: Lens' Schedule (Maybe Text)
sExpression = lens _sExpression (\ s a -> s{_sExpression = a})

instance FromJSON Schedule where
        parseJSON
          = withObject "Schedule"
              (\ x -> Schedule' <$> (x .:? "expression"))

instance Hashable Schedule where

instance NFData Schedule where

instance ToJSON Schedule where
        toJSON Schedule'{..}
          = object
              (catMaybes [("expression" .=) <$> _sExpression])

-- | Creates a new message using only the specified attributes from the original message.
--
--
--
-- /See:/ 'selectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
  { _saaNext       :: !(Maybe Text)
  , _saaName       :: !Text
  , _saaAttributes :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectAttributesActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saaNext' - The next activity in the pipeline.
--
-- * 'saaName' - The name of the 'selectAttributes' activity.
--
-- * 'saaAttributes' - A list of the attributes to select from the message.
selectAttributesActivity
    :: Text -- ^ 'saaName'
    -> NonEmpty Text -- ^ 'saaAttributes'
    -> SelectAttributesActivity
selectAttributesActivity pName_ pAttributes_ =
  SelectAttributesActivity'
    { _saaNext = Nothing
    , _saaName = pName_
    , _saaAttributes = _List1 # pAttributes_
    }


-- | The next activity in the pipeline.
saaNext :: Lens' SelectAttributesActivity (Maybe Text)
saaNext = lens _saaNext (\ s a -> s{_saaNext = a})

-- | The name of the 'selectAttributes' activity.
saaName :: Lens' SelectAttributesActivity Text
saaName = lens _saaName (\ s a -> s{_saaName = a})

-- | A list of the attributes to select from the message.
saaAttributes :: Lens' SelectAttributesActivity (NonEmpty Text)
saaAttributes = lens _saaAttributes (\ s a -> s{_saaAttributes = a}) . _List1

instance FromJSON SelectAttributesActivity where
        parseJSON
          = withObject "SelectAttributesActivity"
              (\ x ->
                 SelectAttributesActivity' <$>
                   (x .:? "next") <*> (x .: "name") <*>
                     (x .: "attributes"))

instance Hashable SelectAttributesActivity where

instance NFData SelectAttributesActivity where

instance ToJSON SelectAttributesActivity where
        toJSON SelectAttributesActivity'{..}
          = object
              (catMaybes
                 [("next" .=) <$> _saaNext, Just ("name" .= _saaName),
                  Just ("attributes" .= _saaAttributes)])

-- | The SQL query to modify the message.
--
--
--
-- /See:/ 'sqlQueryDatasetAction' smart constructor.
data SqlQueryDatasetAction = SqlQueryDatasetAction'
  { _sqdaFilters  :: !(Maybe [QueryFilter])
  , _sqdaSqlQuery :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SqlQueryDatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqdaFilters' - Pre-filters applied to message data.
--
-- * 'sqdaSqlQuery' - A SQL query string.
sqlQueryDatasetAction
    :: Text -- ^ 'sqdaSqlQuery'
    -> SqlQueryDatasetAction
sqlQueryDatasetAction pSqlQuery_ =
  SqlQueryDatasetAction' {_sqdaFilters = Nothing, _sqdaSqlQuery = pSqlQuery_}


-- | Pre-filters applied to message data.
sqdaFilters :: Lens' SqlQueryDatasetAction [QueryFilter]
sqdaFilters = lens _sqdaFilters (\ s a -> s{_sqdaFilters = a}) . _Default . _Coerce

-- | A SQL query string.
sqdaSqlQuery :: Lens' SqlQueryDatasetAction Text
sqdaSqlQuery = lens _sqdaSqlQuery (\ s a -> s{_sqdaSqlQuery = a})

instance FromJSON SqlQueryDatasetAction where
        parseJSON
          = withObject "SqlQueryDatasetAction"
              (\ x ->
                 SqlQueryDatasetAction' <$>
                   (x .:? "filters" .!= mempty) <*> (x .: "sqlQuery"))

instance Hashable SqlQueryDatasetAction where

instance NFData SqlQueryDatasetAction where

instance ToJSON SqlQueryDatasetAction where
        toJSON SqlQueryDatasetAction'{..}
          = object
              (catMaybes
                 [("filters" .=) <$> _sqdaFilters,
                  Just ("sqlQuery" .= _sqdaSqlQuery)])

-- | A set of key/value pairs which are used to manage the resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag's key.
--
-- * 'tagValue' - The tag's value.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The tag's key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The tag's value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "key") <*> (x .: "value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("key" .= _tagKey),
                  Just ("value" .= _tagValue)])

-- | Information about the data set whose content generation triggers the new data set content generation.
--
--
--
-- /See:/ 'triggeringDataset' smart constructor.
newtype TriggeringDataset = TriggeringDataset'
  { _tdName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TriggeringDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdName' - The name of the data set whose content generation triggers the new data set content generation.
triggeringDataset
    :: Text -- ^ 'tdName'
    -> TriggeringDataset
triggeringDataset pName_ = TriggeringDataset' {_tdName = pName_}


-- | The name of the data set whose content generation triggers the new data set content generation.
tdName :: Lens' TriggeringDataset Text
tdName = lens _tdName (\ s a -> s{_tdName = a})

instance FromJSON TriggeringDataset where
        parseJSON
          = withObject "TriggeringDataset"
              (\ x -> TriggeringDataset' <$> (x .: "name"))

instance Hashable TriggeringDataset where

instance NFData TriggeringDataset where

instance ToJSON TriggeringDataset where
        toJSON TriggeringDataset'{..}
          = object (catMaybes [Just ("name" .= _tdName)])

-- | An instance of a variable to be passed to the "containerAction" execution. Each variable must have a name and a value given by one of "stringValue", "datasetContentVersionValue", or "outputFileUriValue".
--
--
--
-- /See:/ 'variable' smart constructor.
data Variable = Variable'
  { _vOutputFileURIValue         :: !(Maybe OutputFileURIValue)
  , _vDoubleValue                :: !(Maybe Double)
  , _vStringValue                :: !(Maybe Text)
  , _vDatasetContentVersionValue :: !(Maybe DatasetContentVersionValue)
  , _vName                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Variable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vOutputFileURIValue' - The value of the variable as a structure that specifies an output file URI.
--
-- * 'vDoubleValue' - The value of the variable as a double (numeric).
--
-- * 'vStringValue' - The value of the variable as a string.
--
-- * 'vDatasetContentVersionValue' - The value of the variable as a structure that specifies a data set content version.
--
-- * 'vName' - The name of the variable.
variable
    :: Text -- ^ 'vName'
    -> Variable
variable pName_ =
  Variable'
    { _vOutputFileURIValue = Nothing
    , _vDoubleValue = Nothing
    , _vStringValue = Nothing
    , _vDatasetContentVersionValue = Nothing
    , _vName = pName_
    }


-- | The value of the variable as a structure that specifies an output file URI.
vOutputFileURIValue :: Lens' Variable (Maybe OutputFileURIValue)
vOutputFileURIValue = lens _vOutputFileURIValue (\ s a -> s{_vOutputFileURIValue = a})

-- | The value of the variable as a double (numeric).
vDoubleValue :: Lens' Variable (Maybe Double)
vDoubleValue = lens _vDoubleValue (\ s a -> s{_vDoubleValue = a})

-- | The value of the variable as a string.
vStringValue :: Lens' Variable (Maybe Text)
vStringValue = lens _vStringValue (\ s a -> s{_vStringValue = a})

-- | The value of the variable as a structure that specifies a data set content version.
vDatasetContentVersionValue :: Lens' Variable (Maybe DatasetContentVersionValue)
vDatasetContentVersionValue = lens _vDatasetContentVersionValue (\ s a -> s{_vDatasetContentVersionValue = a})

-- | The name of the variable.
vName :: Lens' Variable Text
vName = lens _vName (\ s a -> s{_vName = a})

instance FromJSON Variable where
        parseJSON
          = withObject "Variable"
              (\ x ->
                 Variable' <$>
                   (x .:? "outputFileUriValue") <*>
                     (x .:? "doubleValue")
                     <*> (x .:? "stringValue")
                     <*> (x .:? "datasetContentVersionValue")
                     <*> (x .: "name"))

instance Hashable Variable where

instance NFData Variable where

instance ToJSON Variable where
        toJSON Variable'{..}
          = object
              (catMaybes
                 [("outputFileUriValue" .=) <$> _vOutputFileURIValue,
                  ("doubleValue" .=) <$> _vDoubleValue,
                  ("stringValue" .=) <$> _vStringValue,
                  ("datasetContentVersionValue" .=) <$>
                    _vDatasetContentVersionValue,
                  Just ("name" .= _vName)])
