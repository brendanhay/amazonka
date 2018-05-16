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

-- | Information about a data set.
--
--
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'
  { _dCreationTime   :: !(Maybe POSIX)
  , _dStatus         :: !(Maybe DatasetStatus)
  , _dArn            :: !(Maybe Text)
  , _dActions        :: !(Maybe (List1 DatasetAction))
  , _dTriggers       :: !(Maybe [DatasetTrigger])
  , _dName           :: !(Maybe Text)
  , _dLastUpdateTime :: !(Maybe POSIX)
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
-- * 'dActions' - The "DatasetAction" objects that create the data set.
--
-- * 'dTriggers' - The "DatasetTrigger" objects that specify when the data set is automatically updated.
--
-- * 'dName' - The name of the data set.
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
    , _dName = Nothing
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

-- | The "DatasetAction" objects that create the data set.
dActions :: Lens' Dataset (Maybe (NonEmpty DatasetAction))
dActions = lens _dActions (\ s a -> s{_dActions = a}) . mapping _List1

-- | The "DatasetTrigger" objects that specify when the data set is automatically updated.
dTriggers :: Lens' Dataset [DatasetTrigger]
dTriggers = lens _dTriggers (\ s a -> s{_dTriggers = a}) . _Default . _Coerce

-- | The name of the data set.
dName :: Lens' Dataset (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

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
                     <*> (x .:? "name")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable Dataset where

instance NFData Dataset where

-- | A "DatasetAction" object specifying the query that creates the data set content.
--
--
--
-- /See:/ 'datasetAction' smart constructor.
data DatasetAction = DatasetAction'
  { _daQueryAction :: !(Maybe SqlQueryDatasetAction)
  , _daActionName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daQueryAction' - An "SqlQueryDatasetAction" object that contains the SQL query to modify the message.
--
-- * 'daActionName' - The name of the data set action.
datasetAction
    :: DatasetAction
datasetAction =
  DatasetAction' {_daQueryAction = Nothing, _daActionName = Nothing}


-- | An "SqlQueryDatasetAction" object that contains the SQL query to modify the message.
daQueryAction :: Lens' DatasetAction (Maybe SqlQueryDatasetAction)
daQueryAction = lens _daQueryAction (\ s a -> s{_daQueryAction = a})

-- | The name of the data set action.
daActionName :: Lens' DatasetAction (Maybe Text)
daActionName = lens _daActionName (\ s a -> s{_daActionName = a})

instance FromJSON DatasetAction where
        parseJSON
          = withObject "DatasetAction"
              (\ x ->
                 DatasetAction' <$>
                   (x .:? "queryAction") <*> (x .:? "actionName"))

instance Hashable DatasetAction where

instance NFData DatasetAction where

instance ToJSON DatasetAction where
        toJSON DatasetAction'{..}
          = object
              (catMaybes
                 [("queryAction" .=) <$> _daQueryAction,
                  ("actionName" .=) <$> _daActionName])

-- | The state of the data set and the reason it is in this state.
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
-- * 'dcsState' - The state of the data set. Can be one of "CREATING", "SUCCEEDED" or "FAILED".
--
-- * 'dcsReason' - The reason the data set is in this state.
datasetContentStatus
    :: DatasetContentStatus
datasetContentStatus =
  DatasetContentStatus' {_dcsState = Nothing, _dcsReason = Nothing}


-- | The state of the data set. Can be one of "CREATING", "SUCCEEDED" or "FAILED".
dcsState :: Lens' DatasetContentStatus (Maybe DatasetContentState)
dcsState = lens _dcsState (\ s a -> s{_dcsState = a})

-- | The reason the data set is in this state.
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
-- * 'dssDatasetName' - The name of the data set.
--
-- * 'dssLastUpdateTime' - The last time the data set was updated.
datasetSummary
    :: DatasetSummary
datasetSummary =
  DatasetSummary'
    { _dssCreationTime = Nothing
    , _dssStatus = Nothing
    , _dssDatasetName = Nothing
    , _dssLastUpdateTime = Nothing
    }


-- | The time the data set was created.
dssCreationTime :: Lens' DatasetSummary (Maybe UTCTime)
dssCreationTime = lens _dssCreationTime (\ s a -> s{_dssCreationTime = a}) . mapping _Time

-- | The status of the data set.
dssStatus :: Lens' DatasetSummary (Maybe DatasetStatus)
dssStatus = lens _dssStatus (\ s a -> s{_dssStatus = a})

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
                     (x .:? "datasetName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatasetSummary where

instance NFData DatasetSummary where

-- | The "DatasetTrigger" that specifies when the data set is automatically updated.
--
--
--
-- /See:/ 'datasetTrigger' smart constructor.
newtype DatasetTrigger = DatasetTrigger'
  { _dtSchedule :: Maybe Schedule
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatasetTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtSchedule' - The "Schedule" when the trigger is initiated.
datasetTrigger
    :: DatasetTrigger
datasetTrigger = DatasetTrigger' {_dtSchedule = Nothing}


-- | The "Schedule" when the trigger is initiated.
dtSchedule :: Lens' DatasetTrigger (Maybe Schedule)
dtSchedule = lens _dtSchedule (\ s a -> s{_dtSchedule = a})

instance FromJSON DatasetTrigger where
        parseJSON
          = withObject "DatasetTrigger"
              (\ x -> DatasetTrigger' <$> (x .:? "schedule"))

instance Hashable DatasetTrigger where

instance NFData DatasetTrigger where

instance ToJSON DatasetTrigger where
        toJSON DatasetTrigger'{..}
          = object
              (catMaybes [("schedule" .=) <$> _dtSchedule])

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
-- * 'faFilter' - An expression that looks like an SQL WHERE clause that must return a Boolean value.
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

-- | An expression that looks like an SQL WHERE clause that must return a Boolean value.
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
-- * 'maAttribute' - The name of the attribute that will contain the result of the math operation.
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

-- | The name of the attribute that will contain the result of the math operation.
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
-- * 'mMessageId' - The ID you wish to assign to the message.
--
-- * 'mPayload' - The payload of the message.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
message
    :: Text -- ^ 'mMessageId'
    -> ByteString -- ^ 'mPayload'
    -> Message
message pMessageId_ pPayload_ =
  Message' {_mMessageId = pMessageId_, _mPayload = _Base64 # pPayload_}


-- | The ID you wish to assign to the message.
mMessageId :: Lens' Message Text
mMessageId = lens _mMessageId (\ s a -> s{_mMessageId = a})

-- | The payload of the message.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
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
newtype SqlQueryDatasetAction = SqlQueryDatasetAction'
  { _sqdaSqlQuery :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SqlQueryDatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqdaSqlQuery' - An SQL query string.
sqlQueryDatasetAction
    :: Text -- ^ 'sqdaSqlQuery'
    -> SqlQueryDatasetAction
sqlQueryDatasetAction pSqlQuery_ =
  SqlQueryDatasetAction' {_sqdaSqlQuery = pSqlQuery_}


-- | An SQL query string.
sqdaSqlQuery :: Lens' SqlQueryDatasetAction Text
sqdaSqlQuery = lens _sqdaSqlQuery (\ s a -> s{_sqdaSqlQuery = a})

instance FromJSON SqlQueryDatasetAction where
        parseJSON
          = withObject "SqlQueryDatasetAction"
              (\ x -> SqlQueryDatasetAction' <$> (x .: "sqlQuery"))

instance Hashable SqlQueryDatasetAction where

instance NFData SqlQueryDatasetAction where

instance ToJSON SqlQueryDatasetAction where
        toJSON SqlQueryDatasetAction'{..}
          = object
              (catMaybes [Just ("sqlQuery" .= _sqdaSqlQuery)])
