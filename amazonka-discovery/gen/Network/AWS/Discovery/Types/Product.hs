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

-- | Information about agents or Connectors that were instructed to start collecting data. Information includes the agent/Connector ID, a description of the operation, and whether or not the agent/Connector configuration was updated.
--
--
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
-- * 'acsAgentId' - The agent/Connector ID.
--
-- * 'acsOperationSucceeded' - Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/Connector receives this command the next time it polls for a new command.
--
-- * 'acsDescription' - A description of the operation performed.
agentConfigurationStatus
    :: AgentConfigurationStatus
agentConfigurationStatus =
    AgentConfigurationStatus'
    { _acsAgentId = Nothing
    , _acsOperationSucceeded = Nothing
    , _acsDescription = Nothing
    }

-- | The agent/Connector ID.
acsAgentId :: Lens' AgentConfigurationStatus (Maybe Text)
acsAgentId = lens _acsAgentId (\ s a -> s{_acsAgentId = a});

-- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/Connector receives this command the next time it polls for a new command.
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

-- | Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.
--
--
--
-- /See:/ 'agentInfo' smart constructor.
data AgentInfo = AgentInfo'
    { _aiHostName             :: !(Maybe Text)
    , _aiLastHealthPingTime   :: !(Maybe Text)
    , _aiAgentNetworkInfoList :: !(Maybe [AgentNetworkInfo])
    , _aiConnectorId          :: !(Maybe Text)
    , _aiHealth               :: !(Maybe AgentStatus)
    , _aiAgentId              :: !(Maybe Text)
    , _aiVersion              :: !(Maybe Text)
    , _aiCollectionStatus     :: !(Maybe Text)
    , _aiRegisteredTime       :: !(Maybe Text)
    , _aiAgentType            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AgentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiHostName' - The name of the host where the agent or connector resides. The host can be a server or virtual machine.
--
-- * 'aiLastHealthPingTime' - Time since agent or connector health was reported.
--
-- * 'aiAgentNetworkInfoList' - Network details about the host where the agent or connector resides.
--
-- * 'aiConnectorId' - The ID of the connector.
--
-- * 'aiHealth' - The health of the agent or connector.
--
-- * 'aiAgentId' - The agent or connector ID.
--
-- * 'aiVersion' - The agent or connector version.
--
-- * 'aiCollectionStatus' - Status of the collection process for an agent or connector.
--
-- * 'aiRegisteredTime' - Agent's first registration time stamp in UTC.
--
-- * 'aiAgentType' - Type of agent.
agentInfo
    :: AgentInfo
agentInfo =
    AgentInfo'
    { _aiHostName = Nothing
    , _aiLastHealthPingTime = Nothing
    , _aiAgentNetworkInfoList = Nothing
    , _aiConnectorId = Nothing
    , _aiHealth = Nothing
    , _aiAgentId = Nothing
    , _aiVersion = Nothing
    , _aiCollectionStatus = Nothing
    , _aiRegisteredTime = Nothing
    , _aiAgentType = Nothing
    }

-- | The name of the host where the agent or connector resides. The host can be a server or virtual machine.
aiHostName :: Lens' AgentInfo (Maybe Text)
aiHostName = lens _aiHostName (\ s a -> s{_aiHostName = a});

-- | Time since agent or connector health was reported.
aiLastHealthPingTime :: Lens' AgentInfo (Maybe Text)
aiLastHealthPingTime = lens _aiLastHealthPingTime (\ s a -> s{_aiLastHealthPingTime = a});

-- | Network details about the host where the agent or connector resides.
aiAgentNetworkInfoList :: Lens' AgentInfo [AgentNetworkInfo]
aiAgentNetworkInfoList = lens _aiAgentNetworkInfoList (\ s a -> s{_aiAgentNetworkInfoList = a}) . _Default . _Coerce;

-- | The ID of the connector.
aiConnectorId :: Lens' AgentInfo (Maybe Text)
aiConnectorId = lens _aiConnectorId (\ s a -> s{_aiConnectorId = a});

-- | The health of the agent or connector.
aiHealth :: Lens' AgentInfo (Maybe AgentStatus)
aiHealth = lens _aiHealth (\ s a -> s{_aiHealth = a});

-- | The agent or connector ID.
aiAgentId :: Lens' AgentInfo (Maybe Text)
aiAgentId = lens _aiAgentId (\ s a -> s{_aiAgentId = a});

-- | The agent or connector version.
aiVersion :: Lens' AgentInfo (Maybe Text)
aiVersion = lens _aiVersion (\ s a -> s{_aiVersion = a});

-- | Status of the collection process for an agent or connector.
aiCollectionStatus :: Lens' AgentInfo (Maybe Text)
aiCollectionStatus = lens _aiCollectionStatus (\ s a -> s{_aiCollectionStatus = a});

-- | Agent's first registration time stamp in UTC.
aiRegisteredTime :: Lens' AgentInfo (Maybe Text)
aiRegisteredTime = lens _aiRegisteredTime (\ s a -> s{_aiRegisteredTime = a});

-- | Type of agent.
aiAgentType :: Lens' AgentInfo (Maybe Text)
aiAgentType = lens _aiAgentType (\ s a -> s{_aiAgentType = a});

instance FromJSON AgentInfo where
        parseJSON
          = withObject "AgentInfo"
              (\ x ->
                 AgentInfo' <$>
                   (x .:? "hostName") <*> (x .:? "lastHealthPingTime")
                     <*> (x .:? "agentNetworkInfoList" .!= mempty)
                     <*> (x .:? "connectorId")
                     <*> (x .:? "health")
                     <*> (x .:? "agentId")
                     <*> (x .:? "version")
                     <*> (x .:? "collectionStatus")
                     <*> (x .:? "registeredTime")
                     <*> (x .:? "agentType"))

instance Hashable AgentInfo

instance NFData AgentInfo

-- | Network details about the host where the agent/Connector resides.
--
--
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
-- * 'aniIpAddress' - The IP address for the host where the agent/Connector resides.
--
-- * 'aniMacAddress' - The MAC address for the host where the agent/Connector resides.
agentNetworkInfo
    :: AgentNetworkInfo
agentNetworkInfo =
    AgentNetworkInfo'
    { _aniIpAddress = Nothing
    , _aniMacAddress = Nothing
    }

-- | The IP address for the host where the agent/Connector resides.
aniIpAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniIpAddress = lens _aniIpAddress (\ s a -> s{_aniIpAddress = a});

-- | The MAC address for the host where the agent/Connector resides.
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
--
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
-- * 'ctTimeOfCreation' - The time the configuration tag was created in Coordinated Universal Time (UTC).
--
-- * 'ctConfigurationId' - The configuration ID for the item you want to tag. You can specify a list of keys and values.
--
-- * 'ctConfigurationType' - A type of IT asset that you want to tag.
--
-- * 'ctValue' - A value to filter on. For example /key = serverType/ and /value = web server/ .
--
-- * 'ctKey' - A type of tag to filter on. For example, /serverType/ .
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

-- | A value to filter on. For example /key = serverType/ and /value = web server/ .
ctValue :: Lens' ConfigurationTag (Maybe Text)
ctValue = lens _ctValue (\ s a -> s{_ctValue = a});

-- | A type of tag to filter on. For example, /serverType/ .
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

-- | Inventory data for installed discovery agents.
--
--
--
-- /See:/ 'customerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
    { _caiActiveAgents      :: !Int
    , _caiHealthyAgents     :: !Int
    , _caiBlackListedAgents :: !Int
    , _caiShutdownAgents    :: !Int
    , _caiUnhealthyAgents   :: !Int
    , _caiTotalAgents       :: !Int
    , _caiUnknownAgents     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomerAgentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caiActiveAgents' - Number of active discovery agents.
--
-- * 'caiHealthyAgents' - Number of healthy discovery agents
--
-- * 'caiBlackListedAgents' - Number of blacklisted discovery agents.
--
-- * 'caiShutdownAgents' - Number of discovery agents with status SHUTDOWN.
--
-- * 'caiUnhealthyAgents' - Number of unhealthy discovery agents.
--
-- * 'caiTotalAgents' - Total number of discovery agents.
--
-- * 'caiUnknownAgents' - Number of unknown discovery agents.
customerAgentInfo
    :: Int -- ^ 'caiActiveAgents'
    -> Int -- ^ 'caiHealthyAgents'
    -> Int -- ^ 'caiBlackListedAgents'
    -> Int -- ^ 'caiShutdownAgents'
    -> Int -- ^ 'caiUnhealthyAgents'
    -> Int -- ^ 'caiTotalAgents'
    -> Int -- ^ 'caiUnknownAgents'
    -> CustomerAgentInfo
customerAgentInfo pActiveAgents_ pHealthyAgents_ pBlackListedAgents_ pShutdownAgents_ pUnhealthyAgents_ pTotalAgents_ pUnknownAgents_ =
    CustomerAgentInfo'
    { _caiActiveAgents = pActiveAgents_
    , _caiHealthyAgents = pHealthyAgents_
    , _caiBlackListedAgents = pBlackListedAgents_
    , _caiShutdownAgents = pShutdownAgents_
    , _caiUnhealthyAgents = pUnhealthyAgents_
    , _caiTotalAgents = pTotalAgents_
    , _caiUnknownAgents = pUnknownAgents_
    }

-- | Number of active discovery agents.
caiActiveAgents :: Lens' CustomerAgentInfo Int
caiActiveAgents = lens _caiActiveAgents (\ s a -> s{_caiActiveAgents = a});

-- | Number of healthy discovery agents
caiHealthyAgents :: Lens' CustomerAgentInfo Int
caiHealthyAgents = lens _caiHealthyAgents (\ s a -> s{_caiHealthyAgents = a});

-- | Number of blacklisted discovery agents.
caiBlackListedAgents :: Lens' CustomerAgentInfo Int
caiBlackListedAgents = lens _caiBlackListedAgents (\ s a -> s{_caiBlackListedAgents = a});

-- | Number of discovery agents with status SHUTDOWN.
caiShutdownAgents :: Lens' CustomerAgentInfo Int
caiShutdownAgents = lens _caiShutdownAgents (\ s a -> s{_caiShutdownAgents = a});

-- | Number of unhealthy discovery agents.
caiUnhealthyAgents :: Lens' CustomerAgentInfo Int
caiUnhealthyAgents = lens _caiUnhealthyAgents (\ s a -> s{_caiUnhealthyAgents = a});

-- | Total number of discovery agents.
caiTotalAgents :: Lens' CustomerAgentInfo Int
caiTotalAgents = lens _caiTotalAgents (\ s a -> s{_caiTotalAgents = a});

-- | Number of unknown discovery agents.
caiUnknownAgents :: Lens' CustomerAgentInfo Int
caiUnknownAgents = lens _caiUnknownAgents (\ s a -> s{_caiUnknownAgents = a});

instance FromJSON CustomerAgentInfo where
        parseJSON
          = withObject "CustomerAgentInfo"
              (\ x ->
                 CustomerAgentInfo' <$>
                   (x .: "activeAgents") <*> (x .: "healthyAgents") <*>
                     (x .: "blackListedAgents")
                     <*> (x .: "shutdownAgents")
                     <*> (x .: "unhealthyAgents")
                     <*> (x .: "totalAgents")
                     <*> (x .: "unknownAgents"))

instance Hashable CustomerAgentInfo

instance NFData CustomerAgentInfo

-- | Inventory data for installed discovery connectors.
--
--
--
-- /See:/ 'customerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
    { _cciActiveConnectors      :: !Int
    , _cciHealthyConnectors     :: !Int
    , _cciBlackListedConnectors :: !Int
    , _cciShutdownConnectors    :: !Int
    , _cciUnhealthyConnectors   :: !Int
    , _cciTotalConnectors       :: !Int
    , _cciUnknownConnectors     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomerConnectorInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cciActiveConnectors' - Number of active discovery connectors.
--
-- * 'cciHealthyConnectors' - Number of healthy discovery connectors.
--
-- * 'cciBlackListedConnectors' - Number of blacklisted discovery connectors.
--
-- * 'cciShutdownConnectors' - Number of discovery connectors with status SHUTDOWN,
--
-- * 'cciUnhealthyConnectors' - Number of unhealthy discovery connectors.
--
-- * 'cciTotalConnectors' - Total number of discovery connectors.
--
-- * 'cciUnknownConnectors' - Number of unknown discovery connectors.
customerConnectorInfo
    :: Int -- ^ 'cciActiveConnectors'
    -> Int -- ^ 'cciHealthyConnectors'
    -> Int -- ^ 'cciBlackListedConnectors'
    -> Int -- ^ 'cciShutdownConnectors'
    -> Int -- ^ 'cciUnhealthyConnectors'
    -> Int -- ^ 'cciTotalConnectors'
    -> Int -- ^ 'cciUnknownConnectors'
    -> CustomerConnectorInfo
customerConnectorInfo pActiveConnectors_ pHealthyConnectors_ pBlackListedConnectors_ pShutdownConnectors_ pUnhealthyConnectors_ pTotalConnectors_ pUnknownConnectors_ =
    CustomerConnectorInfo'
    { _cciActiveConnectors = pActiveConnectors_
    , _cciHealthyConnectors = pHealthyConnectors_
    , _cciBlackListedConnectors = pBlackListedConnectors_
    , _cciShutdownConnectors = pShutdownConnectors_
    , _cciUnhealthyConnectors = pUnhealthyConnectors_
    , _cciTotalConnectors = pTotalConnectors_
    , _cciUnknownConnectors = pUnknownConnectors_
    }

-- | Number of active discovery connectors.
cciActiveConnectors :: Lens' CustomerConnectorInfo Int
cciActiveConnectors = lens _cciActiveConnectors (\ s a -> s{_cciActiveConnectors = a});

-- | Number of healthy discovery connectors.
cciHealthyConnectors :: Lens' CustomerConnectorInfo Int
cciHealthyConnectors = lens _cciHealthyConnectors (\ s a -> s{_cciHealthyConnectors = a});

-- | Number of blacklisted discovery connectors.
cciBlackListedConnectors :: Lens' CustomerConnectorInfo Int
cciBlackListedConnectors = lens _cciBlackListedConnectors (\ s a -> s{_cciBlackListedConnectors = a});

-- | Number of discovery connectors with status SHUTDOWN,
cciShutdownConnectors :: Lens' CustomerConnectorInfo Int
cciShutdownConnectors = lens _cciShutdownConnectors (\ s a -> s{_cciShutdownConnectors = a});

-- | Number of unhealthy discovery connectors.
cciUnhealthyConnectors :: Lens' CustomerConnectorInfo Int
cciUnhealthyConnectors = lens _cciUnhealthyConnectors (\ s a -> s{_cciUnhealthyConnectors = a});

-- | Total number of discovery connectors.
cciTotalConnectors :: Lens' CustomerConnectorInfo Int
cciTotalConnectors = lens _cciTotalConnectors (\ s a -> s{_cciTotalConnectors = a});

-- | Number of unknown discovery connectors.
cciUnknownConnectors :: Lens' CustomerConnectorInfo Int
cciUnknownConnectors = lens _cciUnknownConnectors (\ s a -> s{_cciUnknownConnectors = a});

instance FromJSON CustomerConnectorInfo where
        parseJSON
          = withObject "CustomerConnectorInfo"
              (\ x ->
                 CustomerConnectorInfo' <$>
                   (x .: "activeConnectors") <*>
                     (x .: "healthyConnectors")
                     <*> (x .: "blackListedConnectors")
                     <*> (x .: "shutdownConnectors")
                     <*> (x .: "unhealthyConnectors")
                     <*> (x .: "totalConnectors")
                     <*> (x .: "unknownConnectors"))

instance Hashable CustomerConnectorInfo

instance NFData CustomerConnectorInfo

-- | Information regarding the export status of the discovered data. The value is an array of objects.
--
--
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
-- * 'eiConfigurationsDownloadURL' - A URL for an Amazon S3 bucket where you can review the configuration data. The URL is displayed only if the export succeeded.
--
-- * 'eiExportId' - A unique identifier that you can use to query the export.
--
-- * 'eiExportStatus' - The status of the configuration data export. The status can succeed, fail, or be in-progress.
--
-- * 'eiStatusMessage' - Helpful status messages for API callers. For example: Too many exports in the last 6 hours. Export in progress. Export was successful.
--
-- * 'eiExportRequestTime' - The time the configuration data export was initiated.
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
--
-- for a complete list of filters and guidance for using them with the Application Discovery Service, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/querying-configuration-items.html Querying Discovered Configuration Items> .
--
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
-- * 'fName' - The name of the filter.
--
-- * 'fValues' - A string value that you want to filter on. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
--
-- * 'fCondition' - A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
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

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a});

-- | A string value that you want to filter on. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce;

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
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

-- | Details about neighboring servers.
--
--
--
-- /See:/ 'neighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
    { _ncdTransportProtocol   :: !(Maybe Text)
    , _ncdDestinationPort     :: !(Maybe Int)
    , _ncdSourceServerId      :: !Text
    , _ncdDestinationServerId :: !Text
    , _ncdConnectionsCount    :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NeighborConnectionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncdTransportProtocol' - Network protocol used for the connection.
--
-- * 'ncdDestinationPort' - Destination network port for the connection.
--
-- * 'ncdSourceServerId' - ID of server that opened the network connection.
--
-- * 'ncdDestinationServerId' - ID of the server that accepted the networker connection.
--
-- * 'ncdConnectionsCount' - Number of open network connections with the neighboring server.
neighborConnectionDetail
    :: Text -- ^ 'ncdSourceServerId'
    -> Text -- ^ 'ncdDestinationServerId'
    -> Integer -- ^ 'ncdConnectionsCount'
    -> NeighborConnectionDetail
neighborConnectionDetail pSourceServerId_ pDestinationServerId_ pConnectionsCount_ =
    NeighborConnectionDetail'
    { _ncdTransportProtocol = Nothing
    , _ncdDestinationPort = Nothing
    , _ncdSourceServerId = pSourceServerId_
    , _ncdDestinationServerId = pDestinationServerId_
    , _ncdConnectionsCount = pConnectionsCount_
    }

-- | Network protocol used for the connection.
ncdTransportProtocol :: Lens' NeighborConnectionDetail (Maybe Text)
ncdTransportProtocol = lens _ncdTransportProtocol (\ s a -> s{_ncdTransportProtocol = a});

-- | Destination network port for the connection.
ncdDestinationPort :: Lens' NeighborConnectionDetail (Maybe Int)
ncdDestinationPort = lens _ncdDestinationPort (\ s a -> s{_ncdDestinationPort = a});

-- | ID of server that opened the network connection.
ncdSourceServerId :: Lens' NeighborConnectionDetail Text
ncdSourceServerId = lens _ncdSourceServerId (\ s a -> s{_ncdSourceServerId = a});

-- | ID of the server that accepted the networker connection.
ncdDestinationServerId :: Lens' NeighborConnectionDetail Text
ncdDestinationServerId = lens _ncdDestinationServerId (\ s a -> s{_ncdDestinationServerId = a});

-- | Number of open network connections with the neighboring server.
ncdConnectionsCount :: Lens' NeighborConnectionDetail Integer
ncdConnectionsCount = lens _ncdConnectionsCount (\ s a -> s{_ncdConnectionsCount = a});

instance FromJSON NeighborConnectionDetail where
        parseJSON
          = withObject "NeighborConnectionDetail"
              (\ x ->
                 NeighborConnectionDetail' <$>
                   (x .:? "transportProtocol") <*>
                     (x .:? "destinationPort")
                     <*> (x .: "sourceServerId")
                     <*> (x .: "destinationServerId")
                     <*> (x .: "connectionsCount"))

instance Hashable NeighborConnectionDetail

instance NFData NeighborConnectionDetail

-- | Field and direction for ordered output.
--
--
--
-- /See:/ 'orderByElement' smart constructor.
data OrderByElement = OrderByElement'
    { _obeSortOrder :: !(Maybe OrderString)
    , _obeFieldName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderByElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'obeSortOrder' - Ordering direction.
--
-- * 'obeFieldName' - Field to order on.
orderByElement
    :: Text -- ^ 'obeFieldName'
    -> OrderByElement
orderByElement pFieldName_ =
    OrderByElement'
    { _obeSortOrder = Nothing
    , _obeFieldName = pFieldName_
    }

-- | Ordering direction.
obeSortOrder :: Lens' OrderByElement (Maybe OrderString)
obeSortOrder = lens _obeSortOrder (\ s a -> s{_obeSortOrder = a});

-- | Field to order on.
obeFieldName :: Lens' OrderByElement Text
obeFieldName = lens _obeFieldName (\ s a -> s{_obeFieldName = a});

instance Hashable OrderByElement

instance NFData OrderByElement

instance ToJSON OrderByElement where
        toJSON OrderByElement'{..}
          = object
              (catMaybes
                 [("sortOrder" .=) <$> _obeSortOrder,
                  Just ("fieldName" .= _obeFieldName)])

-- | Metadata that help you categorize IT assets.
--
--
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
-- * 'tagKey' - A type of tag to filter on.
--
-- * 'tagValue' - A value for a tag key to filter on.
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

-- | The name of a tag filter. Valid names are: @tagKey@ , @tagValue@ , @configurationId@ .
--
--
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
-- * 'tfName' - A name of a tag filter.
--
-- * 'tfValues' - Values of a tag filter.
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
