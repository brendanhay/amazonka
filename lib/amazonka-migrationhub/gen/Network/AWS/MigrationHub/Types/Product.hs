{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types.Product where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.Sum
import Network.AWS.Prelude

-- | An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).
--
--
--
-- /See:/ 'createdArtifact' smart constructor.
data CreatedArtifact = CreatedArtifact'
  { _caDescription :: !(Maybe Text)
  , _caName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatedArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDescription' - A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
--
-- * 'caName' - An ARN that uniquely identifies the result of a migration task.
createdArtifact
    :: Text -- ^ 'caName'
    -> CreatedArtifact
createdArtifact pName_ =
  CreatedArtifact' {_caDescription = Nothing, _caName = pName_}


-- | A description that can be free-form text to record additional detail about the artifact for clarity or for later reference.
caDescription :: Lens' CreatedArtifact (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | An ARN that uniquely identifies the result of a migration task.
caName :: Lens' CreatedArtifact Text
caName = lens _caName (\ s a -> s{_caName = a})

instance FromJSON CreatedArtifact where
        parseJSON
          = withObject "CreatedArtifact"
              (\ x ->
                 CreatedArtifact' <$>
                   (x .:? "Description") <*> (x .: "Name"))

instance Hashable CreatedArtifact where

instance NFData CreatedArtifact where

instance ToJSON CreatedArtifact where
        toJSON CreatedArtifact'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _caDescription,
                  Just ("Name" .= _caName)])

-- | Object representing the on-premises resource being migrated.
--
--
--
-- /See:/ 'discoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { _drDescription     :: !(Maybe Text)
  , _drConfigurationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoveredResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drDescription' - A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
--
-- * 'drConfigurationId' - The configurationId in ADS that uniquely identifies the on-premise resource.
discoveredResource
    :: Text -- ^ 'drConfigurationId'
    -> DiscoveredResource
discoveredResource pConfigurationId_ =
  DiscoveredResource'
    {_drDescription = Nothing, _drConfigurationId = pConfigurationId_}


-- | A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
drDescription :: Lens' DiscoveredResource (Maybe Text)
drDescription = lens _drDescription (\ s a -> s{_drDescription = a})

-- | The configurationId in ADS that uniquely identifies the on-premise resource.
drConfigurationId :: Lens' DiscoveredResource Text
drConfigurationId = lens _drConfigurationId (\ s a -> s{_drConfigurationId = a})

instance FromJSON DiscoveredResource where
        parseJSON
          = withObject "DiscoveredResource"
              (\ x ->
                 DiscoveredResource' <$>
                   (x .:? "Description") <*> (x .: "ConfigurationId"))

instance Hashable DiscoveredResource where

instance NFData DiscoveredResource where

instance ToJSON DiscoveredResource where
        toJSON DiscoveredResource'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _drDescription,
                  Just ("ConfigurationId" .= _drConfigurationId)])

-- | Represents a migration task in a migration tool.
--
--
--
-- /See:/ 'migrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { _mtUpdateDateTime        :: !(Maybe POSIX)
  , _mtResourceAttributeList :: !(Maybe [ResourceAttribute])
  , _mtTask                  :: !(Maybe Task)
  , _mtProgressUpdateStream  :: !(Maybe Text)
  , _mtMigrationTaskName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MigrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtUpdateDateTime' - The timestamp when the task was gathered.
--
-- * 'mtResourceAttributeList' -
--
-- * 'mtTask' - Task object encapsulating task information.
--
-- * 'mtProgressUpdateStream' - A name that identifies the vendor of the migration tool being used.
--
-- * 'mtMigrationTaskName' - Unique identifier that references the migration task.
migrationTask
    :: MigrationTask
migrationTask =
  MigrationTask'
    { _mtUpdateDateTime = Nothing
    , _mtResourceAttributeList = Nothing
    , _mtTask = Nothing
    , _mtProgressUpdateStream = Nothing
    , _mtMigrationTaskName = Nothing
    }


-- | The timestamp when the task was gathered.
mtUpdateDateTime :: Lens' MigrationTask (Maybe UTCTime)
mtUpdateDateTime = lens _mtUpdateDateTime (\ s a -> s{_mtUpdateDateTime = a}) . mapping _Time

-- |
mtResourceAttributeList :: Lens' MigrationTask [ResourceAttribute]
mtResourceAttributeList = lens _mtResourceAttributeList (\ s a -> s{_mtResourceAttributeList = a}) . _Default . _Coerce

-- | Task object encapsulating task information.
mtTask :: Lens' MigrationTask (Maybe Task)
mtTask = lens _mtTask (\ s a -> s{_mtTask = a})

-- | A name that identifies the vendor of the migration tool being used.
mtProgressUpdateStream :: Lens' MigrationTask (Maybe Text)
mtProgressUpdateStream = lens _mtProgressUpdateStream (\ s a -> s{_mtProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
mtMigrationTaskName :: Lens' MigrationTask (Maybe Text)
mtMigrationTaskName = lens _mtMigrationTaskName (\ s a -> s{_mtMigrationTaskName = a})

instance FromJSON MigrationTask where
        parseJSON
          = withObject "MigrationTask"
              (\ x ->
                 MigrationTask' <$>
                   (x .:? "UpdateDateTime") <*>
                     (x .:? "ResourceAttributeList" .!= mempty)
                     <*> (x .:? "Task")
                     <*> (x .:? "ProgressUpdateStream")
                     <*> (x .:? "MigrationTaskName"))

instance Hashable MigrationTask where

instance NFData MigrationTask where

-- | MigrationTaskSummary includes @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and @UpdateDateTime@ for each task.
--
--
--
-- /See:/ 'migrationTaskSummary' smart constructor.
data MigrationTaskSummary = MigrationTaskSummary'
  { _mtsStatus               :: !(Maybe MigrationStatus)
  , _mtsUpdateDateTime       :: !(Maybe POSIX)
  , _mtsProgressPercent      :: !(Maybe Nat)
  , _mtsStatusDetail         :: !(Maybe Text)
  , _mtsProgressUpdateStream :: !(Maybe Text)
  , _mtsMigrationTaskName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MigrationTaskSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtsStatus' - Status of the task.
--
-- * 'mtsUpdateDateTime' - The timestamp when the task was gathered.
--
-- * 'mtsProgressPercent' -
--
-- * 'mtsStatusDetail' - Detail information of what is being done within the overall status state.
--
-- * 'mtsProgressUpdateStream' - An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
--
-- * 'mtsMigrationTaskName' - Unique identifier that references the migration task.
migrationTaskSummary
    :: MigrationTaskSummary
migrationTaskSummary =
  MigrationTaskSummary'
    { _mtsStatus = Nothing
    , _mtsUpdateDateTime = Nothing
    , _mtsProgressPercent = Nothing
    , _mtsStatusDetail = Nothing
    , _mtsProgressUpdateStream = Nothing
    , _mtsMigrationTaskName = Nothing
    }


-- | Status of the task.
mtsStatus :: Lens' MigrationTaskSummary (Maybe MigrationStatus)
mtsStatus = lens _mtsStatus (\ s a -> s{_mtsStatus = a})

-- | The timestamp when the task was gathered.
mtsUpdateDateTime :: Lens' MigrationTaskSummary (Maybe UTCTime)
mtsUpdateDateTime = lens _mtsUpdateDateTime (\ s a -> s{_mtsUpdateDateTime = a}) . mapping _Time

-- |
mtsProgressPercent :: Lens' MigrationTaskSummary (Maybe Natural)
mtsProgressPercent = lens _mtsProgressPercent (\ s a -> s{_mtsProgressPercent = a}) . mapping _Nat

-- | Detail information of what is being done within the overall status state.
mtsStatusDetail :: Lens' MigrationTaskSummary (Maybe Text)
mtsStatusDetail = lens _mtsStatusDetail (\ s a -> s{_mtsStatusDetail = a})

-- | An AWS resource used for access control. It should uniquely identify the migration tool as it is used for all updates made by the tool.
mtsProgressUpdateStream :: Lens' MigrationTaskSummary (Maybe Text)
mtsProgressUpdateStream = lens _mtsProgressUpdateStream (\ s a -> s{_mtsProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
mtsMigrationTaskName :: Lens' MigrationTaskSummary (Maybe Text)
mtsMigrationTaskName = lens _mtsMigrationTaskName (\ s a -> s{_mtsMigrationTaskName = a})

instance FromJSON MigrationTaskSummary where
        parseJSON
          = withObject "MigrationTaskSummary"
              (\ x ->
                 MigrationTaskSummary' <$>
                   (x .:? "Status") <*> (x .:? "UpdateDateTime") <*>
                     (x .:? "ProgressPercent")
                     <*> (x .:? "StatusDetail")
                     <*> (x .:? "ProgressUpdateStream")
                     <*> (x .:? "MigrationTaskName"))

instance Hashable MigrationTaskSummary where

instance NFData MigrationTaskSummary where

-- | Summary of the AWS resource used for access control that is implicitly linked to your AWS account.
--
--
--
-- /See:/ 'progressUpdateStreamSummary' smart constructor.
newtype ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { _pussProgressUpdateStreamName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProgressUpdateStreamSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pussProgressUpdateStreamName' - The name of the ProgressUpdateStream.
progressUpdateStreamSummary
    :: ProgressUpdateStreamSummary
progressUpdateStreamSummary =
  ProgressUpdateStreamSummary' {_pussProgressUpdateStreamName = Nothing}


-- | The name of the ProgressUpdateStream.
pussProgressUpdateStreamName :: Lens' ProgressUpdateStreamSummary (Maybe Text)
pussProgressUpdateStreamName = lens _pussProgressUpdateStreamName (\ s a -> s{_pussProgressUpdateStreamName = a})

instance FromJSON ProgressUpdateStreamSummary where
        parseJSON
          = withObject "ProgressUpdateStreamSummary"
              (\ x ->
                 ProgressUpdateStreamSummary' <$>
                   (x .:? "ProgressUpdateStreamName"))

instance Hashable ProgressUpdateStreamSummary where

instance NFData ProgressUpdateStreamSummary where

-- | Attribute associated with a resource.
--
--
-- Note the corresponding format required per type listed below:
--
--     * IPV4    * @x.x.x.x@
--
-- /where x is an integer in the range [0,255]/
--
--     * IPV6    * @y : y : y : y : y : y : y : y@
--
-- /where y is a hexadecimal between 0 and FFFF. [0, FFFF]/
--
--     * MAC_ADDRESS    * @^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$@
--
--     * FQDN    * @^[^<>{}\\\\/?,=\\p{Cntrl}]{1,256}$@
--
--
--
--
-- /See:/ 'resourceAttribute' smart constructor.
data ResourceAttribute = ResourceAttribute'
  { _raType  :: !ResourceAttributeType
  , _raValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raType' - Type of resource.
--
-- * 'raValue' - Value of the resource type.
resourceAttribute
    :: ResourceAttributeType -- ^ 'raType'
    -> Text -- ^ 'raValue'
    -> ResourceAttribute
resourceAttribute pType_ pValue_ =
  ResourceAttribute' {_raType = pType_, _raValue = pValue_}


-- | Type of resource.
raType :: Lens' ResourceAttribute ResourceAttributeType
raType = lens _raType (\ s a -> s{_raType = a})

-- | Value of the resource type.
raValue :: Lens' ResourceAttribute Text
raValue = lens _raValue (\ s a -> s{_raValue = a})

instance FromJSON ResourceAttribute where
        parseJSON
          = withObject "ResourceAttribute"
              (\ x ->
                 ResourceAttribute' <$>
                   (x .: "Type") <*> (x .: "Value"))

instance Hashable ResourceAttribute where

instance NFData ResourceAttribute where

instance ToJSON ResourceAttribute where
        toJSON ResourceAttribute'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _raType),
                  Just ("Value" .= _raValue)])

-- | Task object encapsulating task information.
--
--
--
-- /See:/ 'task' smart constructor.
data Task = Task'
  { _tProgressPercent :: !(Maybe Nat)
  , _tStatusDetail    :: !(Maybe Text)
  , _tStatus          :: !MigrationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tProgressPercent' - Indication of the percentage completion of the task.
--
-- * 'tStatusDetail' - Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
--
-- * 'tStatus' - Status of the task - Not Started, In-Progress, Complete.
task
    :: MigrationStatus -- ^ 'tStatus'
    -> Task
task pStatus_ =
  Task'
    {_tProgressPercent = Nothing, _tStatusDetail = Nothing, _tStatus = pStatus_}


-- | Indication of the percentage completion of the task.
tProgressPercent :: Lens' Task (Maybe Natural)
tProgressPercent = lens _tProgressPercent (\ s a -> s{_tProgressPercent = a}) . mapping _Nat

-- | Details of task status as notified by a migration tool. A tool might use this field to provide clarifying information about the status that is unique to that tool or that explains an error state.
tStatusDetail :: Lens' Task (Maybe Text)
tStatusDetail = lens _tStatusDetail (\ s a -> s{_tStatusDetail = a})

-- | Status of the task - Not Started, In-Progress, Complete.
tStatus :: Lens' Task MigrationStatus
tStatus = lens _tStatus (\ s a -> s{_tStatus = a})

instance FromJSON Task where
        parseJSON
          = withObject "Task"
              (\ x ->
                 Task' <$>
                   (x .:? "ProgressPercent") <*> (x .:? "StatusDetail")
                     <*> (x .: "Status"))

instance Hashable Task where

instance NFData Task where

instance ToJSON Task where
        toJSON Task'{..}
          = object
              (catMaybes
                 [("ProgressPercent" .=) <$> _tProgressPercent,
                  ("StatusDetail" .=) <$> _tStatusDetail,
                  Just ("Status" .= _tStatus)])
