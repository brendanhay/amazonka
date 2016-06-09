{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.Product where

import           Network.AWS.Discovery.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Information about agents that were instructed to start collecting data. Information includes the agent ID, a description of the operation, and whether or not the agent configuration was updated.
--
-- /See:/ 'agentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
    { _acsAgentId            :: !(Maybe Text)
    , _acsOperationSucceeded :: !(Maybe Bool)
    , _acsDescription        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentConfigurationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsAgentId'
--
-- * 'acsOperationSucceeded'
--
-- * 'acsDescription'
agentConfigurationStatus
    :: AgentConfigurationStatus
agentConfigurationStatus =
    AgentConfigurationStatus'
    { _acsAgentId = Nothing
    , _acsOperationSucceeded = Nothing
    , _acsDescription = Nothing
    }

-- | The agent ID.
acsAgentId :: Lens' AgentConfigurationStatus (Maybe Text)
acsAgentId = lens _acsAgentId (\ s a -> s{_acsAgentId = a});

-- | Information about the status of the 'StartDataCollection' and 'StopDataCollection' operations. The system has recorded the data collection operation. The agent receives this command the next time it polls for a new command.
acsOperationSucceeded :: Lens' AgentConfigurationStatus (Maybe Bool)
acsOperationSucceeded = lens _acsOperationSucceeded (\ s a -> s{_acsOperationSucceeded = a});

-- | A description of the operation performed.
acsDescription :: Lens' AgentConfigurationStatus (Maybe Text)
acsDescription = lens _acsDescription (\ s a -> s{_acsDescription = a});

instance FromJSON AgentConfigurationStatus where
        parseJSON
          = withObject "AgentConfigurationStatus"
              (\ x ->
                 AgentConfigurationStatus' <$>
                   (x .:? "agentId") <*> (x .:? "operationSucceeded")
                     <*> (x .:? "description"))

instance Hashable AgentConfigurationStatus

instance NFData AgentConfigurationStatus

-- | Information about agents associated with the user’s AWS account. Information includes agent IDs, IP addresses, media access control (MAC) addresses, agent health, hostname where the agent resides, and agent version for each agent.
--
-- /See:/ 'agentInfo' smart constructor.
data AgentInfo = AgentInfo'
    { _aiHostName             :: !(Maybe Text)
    , _aiAgentNetworkInfoList :: !(Maybe [AgentNetworkInfo])
    , _aiConnectorId          :: !(Maybe Text)
    , _aiHealth               :: !(Maybe AgentStatus)
    , _aiAgentId              :: !(Maybe Text)
    , _aiVersion              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiHostName'
--
-- * 'aiAgentNetworkInfoList'
--
-- * 'aiConnectorId'
--
-- * 'aiHealth'
--
-- * 'aiAgentId'
--
-- * 'aiVersion'
agentInfo
    :: AgentInfo
agentInfo =
    AgentInfo'
    { _aiHostName = Nothing
    , _aiAgentNetworkInfoList = Nothing
    , _aiConnectorId = Nothing
    , _aiHealth = Nothing
    , _aiAgentId = Nothing
    , _aiVersion = Nothing
    }

-- | The name of the host where the agent resides. The host can be a server or virtual machine.
aiHostName :: Lens' AgentInfo (Maybe Text)
aiHostName = lens _aiHostName (\ s a -> s{_aiHostName = a});

-- | Network details about the host where the agent resides.
aiAgentNetworkInfoList :: Lens' AgentInfo [AgentNetworkInfo]
aiAgentNetworkInfoList = lens _aiAgentNetworkInfoList (\ s a -> s{_aiAgentNetworkInfoList = a}) . _Default . _Coerce;

-- | This data type is currently not valid.
aiConnectorId :: Lens' AgentInfo (Maybe Text)
aiConnectorId = lens _aiConnectorId (\ s a -> s{_aiConnectorId = a});

-- | The health of the agent.
aiHealth :: Lens' AgentInfo (Maybe AgentStatus)
aiHealth = lens _aiHealth (\ s a -> s{_aiHealth = a});

-- | The agent ID.
aiAgentId :: Lens' AgentInfo (Maybe Text)
aiAgentId = lens _aiAgentId (\ s a -> s{_aiAgentId = a});

-- | The agent version.
aiVersion :: Lens' AgentInfo (Maybe Text)
aiVersion = lens _aiVersion (\ s a -> s{_aiVersion = a});

instance FromJSON AgentInfo where
        parseJSON
          = withObject "AgentInfo"
              (\ x ->
                 AgentInfo' <$>
                   (x .:? "hostName") <*>
                     (x .:? "agentNetworkInfoList" .!= mempty)
                     <*> (x .:? "connectorId")
                     <*> (x .:? "health")
                     <*> (x .:? "agentId")
                     <*> (x .:? "version"))

instance Hashable AgentInfo

instance NFData AgentInfo

-- | Network details about the host where the agent resides.
--
-- /See:/ 'agentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
    { _aniIpAddress  :: !(Maybe Text)
    , _aniMacAddress :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentNetworkInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aniIpAddress'
--
-- * 'aniMacAddress'
agentNetworkInfo
    :: AgentNetworkInfo
agentNetworkInfo =
    AgentNetworkInfo'
    { _aniIpAddress = Nothing
    , _aniMacAddress = Nothing
    }

-- | The IP address for the host where the agent resides.
aniIpAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniIpAddress = lens _aniIpAddress (\ s a -> s{_aniIpAddress = a});

-- | The MAC address for the host where the agent resides.
aniMacAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniMacAddress = lens _aniMacAddress (\ s a -> s{_aniMacAddress = a});

instance FromJSON AgentNetworkInfo where
        parseJSON
          = withObject "AgentNetworkInfo"
              (\ x ->
                 AgentNetworkInfo' <$>
                   (x .:? "ipAddress") <*> (x .:? "macAddress"))

instance Hashable AgentNetworkInfo

instance NFData AgentNetworkInfo

-- | Tags for a configuration item. Tags are metadata that help you categorize IT assets.
--
-- /See:/ 'configurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
    { _ctTimeOfCreation    :: !(Maybe POSIX)
    , _ctConfigurationId   :: !(Maybe Text)
    , _ctConfigurationType :: !(Maybe ConfigurationItemType)
    , _ctValue             :: !(Maybe Text)
    , _ctKey               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctTimeOfCreation'
--
-- * 'ctConfigurationId'
--
-- * 'ctConfigurationType'
--
-- * 'ctValue'
--
-- * 'ctKey'
configurationTag
    :: ConfigurationTag
configurationTag =
    ConfigurationTag'
    { _ctTimeOfCreation = Nothing
    , _ctConfigurationId = Nothing
    , _ctConfigurationType = Nothing
    , _ctValue = Nothing
    , _ctKey = Nothing
    }

-- | The time the configuration tag was created in Coordinated Universal Time (UTC).
ctTimeOfCreation :: Lens' ConfigurationTag (Maybe UTCTime)
ctTimeOfCreation = lens _ctTimeOfCreation (\ s a -> s{_ctTimeOfCreation = a}) . mapping _Time;

-- | The configuration ID for the item you want to tag. You can specify a list of keys and values.
ctConfigurationId :: Lens' ConfigurationTag (Maybe Text)
ctConfigurationId = lens _ctConfigurationId (\ s a -> s{_ctConfigurationId = a});

-- | A type of IT asset that you want to tag.
ctConfigurationType :: Lens' ConfigurationTag (Maybe ConfigurationItemType)
ctConfigurationType = lens _ctConfigurationType (\ s a -> s{_ctConfigurationType = a});

-- | A value to filter on. For example /key = serverType/ and /value = web server/.
ctValue :: Lens' ConfigurationTag (Maybe Text)
ctValue = lens _ctValue (\ s a -> s{_ctValue = a});

-- | A type of tag to filter on. For example, /serverType/.
ctKey :: Lens' ConfigurationTag (Maybe Text)
ctKey = lens _ctKey (\ s a -> s{_ctKey = a});

instance FromJSON ConfigurationTag where
        parseJSON
          = withObject "ConfigurationTag"
              (\ x ->
                 ConfigurationTag' <$>
                   (x .:? "timeOfCreation") <*>
                     (x .:? "configurationId")
                     <*> (x .:? "configurationType")
                     <*> (x .:? "value")
                     <*> (x .:? "key"))

instance Hashable ConfigurationTag

instance NFData ConfigurationTag

-- | Information regarding the export status of the discovered data. The value is an array of objects.
--
-- /See:/ 'exportInfo' smart constructor.
data ExportInfo = ExportInfo'
    { _eiConfigurationsDownloadURL :: !(Maybe Text)
    , _eiExportId                  :: !Text
    , _eiExportStatus              :: !ExportStatus
    , _eiStatusMessage             :: !Text
    , _eiExportRequestTime         :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiConfigurationsDownloadURL'
--
-- * 'eiExportId'
--
-- * 'eiExportStatus'
--
-- * 'eiStatusMessage'
--
-- * 'eiExportRequestTime'
exportInfo
    :: Text -- ^ 'eiExportId'
    -> ExportStatus -- ^ 'eiExportStatus'
    -> Text -- ^ 'eiStatusMessage'
    -> UTCTime -- ^ 'eiExportRequestTime'
    -> ExportInfo
exportInfo pExportId_ pExportStatus_ pStatusMessage_ pExportRequestTime_ =
    ExportInfo'
    { _eiConfigurationsDownloadURL = Nothing
    , _eiExportId = pExportId_
    , _eiExportStatus = pExportStatus_
    , _eiStatusMessage = pStatusMessage_
    , _eiExportRequestTime = _Time # pExportRequestTime_
    }

-- | A URL for an Amazon S3 bucket where you can review the configuration data. The URL is displayed only if the export succeeded.
eiConfigurationsDownloadURL :: Lens' ExportInfo (Maybe Text)
eiConfigurationsDownloadURL = lens _eiConfigurationsDownloadURL (\ s a -> s{_eiConfigurationsDownloadURL = a});

-- | A unique identifier that you can use to query the export.
eiExportId :: Lens' ExportInfo Text
eiExportId = lens _eiExportId (\ s a -> s{_eiExportId = a});

-- | The status of the configuration data export. The status can succeed, fail, or be in-progress.
eiExportStatus :: Lens' ExportInfo ExportStatus
eiExportStatus = lens _eiExportStatus (\ s a -> s{_eiExportStatus = a});

-- | Helpful status messages for API callers. For example: Too many exports in the last 6 hours. Export in progress. Export was successful.
eiStatusMessage :: Lens' ExportInfo Text
eiStatusMessage = lens _eiStatusMessage (\ s a -> s{_eiStatusMessage = a});

-- | The time the configuration data export was initiated.
eiExportRequestTime :: Lens' ExportInfo UTCTime
eiExportRequestTime = lens _eiExportRequestTime (\ s a -> s{_eiExportRequestTime = a}) . _Time;

instance FromJSON ExportInfo where
        parseJSON
          = withObject "ExportInfo"
              (\ x ->
                 ExportInfo' <$>
                   (x .:? "configurationsDownloadUrl") <*>
                     (x .: "exportId")
                     <*> (x .: "exportStatus")
                     <*> (x .: "statusMessage")
                     <*> (x .: "exportRequestTime"))

instance Hashable ExportInfo

instance NFData ExportInfo

-- | A filter that can use conditional operators.
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
    { _fName      :: !Text
    , _fValues    :: ![Text]
    , _fCondition :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName'
--
-- * 'fValues'
--
-- * 'fCondition'
filter'
    :: Text -- ^ 'fName'
    -> Text -- ^ 'fCondition'
    -> Filter
filter' pName_ pCondition_ =
    Filter'
    { _fName = pName_
    , _fValues = mempty
    , _fCondition = pCondition_
    }

-- | The name of the filter. The following filter names are allowed for 'SERVER' configuration items.
--
-- __Server__
--
-- -   'server.hostName'
--
-- -   'server.osName'
--
-- -   'server.osVersion'
--
-- -   'server.configurationid'
--
-- -   'server.agentid'
--
-- The name of the filter. The following filter names are allowed for 'PROCESS' configuration items.
--
-- __Process__
--
-- -   'process.configurationid'
--
-- -   'process.name'
--
-- -   'process.commandLine'
--
-- -   'server.configurationid'
--
-- -   'server.hostName'
--
-- -   'server.osName'
--
-- -   'server.osVersion'
--
-- -   'server.agentId'
--
-- The name of the filter. The following filter names are allowed for 'CONNECTION' configuration items.
--
-- __Connection__
--
-- -   'connection.sourceIp'
--
-- -   'connection.destinationIp'
--
-- -   'connection.destinationPort'
--
-- -   'sourceProcess.configurationId'
--
-- -   'sourceProcess.name'
--
-- -   'sourceProcess.commandLine'
--
-- -   'destinationProcess.configurationId'
--
-- -   'destinationProcess.name'
--
-- -   'destinationProcess.commandLine'
--
-- -   'sourceServer.configurationId'
--
-- -   'sourceServer.hostName'
--
-- -   'sourceServer.osName'
--
-- -   'sourceServer.osVersion'
--
-- -   'sourceServer.agentId'
--
-- -   'destinationServer.configurationId'
--
-- -   'destinationServer.hostName'
--
-- -   'destinationServer.osName'
--
-- -   'destinationServer.osVersion'
--
-- -   'destinationServer.agentId'
--
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a});

-- | A string value that you want to filter on. For example, if you choose the 'destinationServer.osVersion' filter name, you could specify 'Ubuntu' for the value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce;

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/. If you specify multiple values for a particular filter, the system differentiates the values using /OR/. Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
fCondition :: Lens' Filter Text
fCondition = lens _fCondition (\ s a -> s{_fCondition = a});

instance Hashable Filter

instance NFData Filter

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("name" .= _fName),
                  Just ("values" .= _fValues),
                  Just ("condition" .= _fCondition)])

-- | Metadata that help you categorize IT assets.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey   :: !Text
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey'
--
-- * 'tagValue'
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ =
    Tag'
    { _tagKey = pKey_
    , _tagValue = pValue_
    }

-- | A type of tag to filter on.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | A value for a tag key to filter on.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("key" .= _tagKey),
                  Just ("value" .= _tagValue)])

-- | The name of a tag filter. Valid names are: 'tagKey', 'tagValue', 'configurationId'.
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
    { _tfName   :: !Text
    , _tfValues :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfName'
--
-- * 'tfValues'
tagFilter
    :: Text -- ^ 'tfName'
    -> TagFilter
tagFilter pName_ =
    TagFilter'
    { _tfName = pName_
    , _tfValues = mempty
    }

-- | A name of a tag filter.
tfName :: Lens' TagFilter Text
tfName = lens _tfName (\ s a -> s{_tfName = a});

-- | Values of a tag filter.
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\ s a -> s{_tfValues = a}) . _Coerce;

instance Hashable TagFilter

instance NFData TagFilter

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              (catMaybes
                 [Just ("name" .= _tfName),
                  Just ("values" .= _tfValues)])
