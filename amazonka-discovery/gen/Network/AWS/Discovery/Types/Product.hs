{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.Product where

import Network.AWS.Discovery.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.
--
--
--
-- /See:/ 'agentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { _acsAgentId            :: !(Maybe Text)
  , _acsOperationSucceeded :: !(Maybe Bool)
  , _acsDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentConfigurationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsAgentId' - The agent/connector ID.
--
-- * 'acsOperationSucceeded' - Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
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


-- | The agent/connector ID.
acsAgentId :: Lens' AgentConfigurationStatus (Maybe Text)
acsAgentId = lens _acsAgentId (\ s a -> s{_acsAgentId = a})

-- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
acsOperationSucceeded :: Lens' AgentConfigurationStatus (Maybe Bool)
acsOperationSucceeded = lens _acsOperationSucceeded (\ s a -> s{_acsOperationSucceeded = a})

-- | A description of the operation performed.
acsDescription :: Lens' AgentConfigurationStatus (Maybe Text)
acsDescription = lens _acsDescription (\ s a -> s{_acsDescription = a})

instance FromJSON AgentConfigurationStatus where
        parseJSON
          = withObject "AgentConfigurationStatus"
              (\ x ->
                 AgentConfigurationStatus' <$>
                   (x .:? "agentId") <*> (x .:? "operationSucceeded")
                     <*> (x .:? "description"))

instance Hashable AgentConfigurationStatus where

instance NFData AgentConfigurationStatus where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'aiRegisteredTime' - Agent's first registration timestamp in UTC.
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
aiHostName = lens _aiHostName (\ s a -> s{_aiHostName = a})

-- | Time since agent or connector health was reported.
aiLastHealthPingTime :: Lens' AgentInfo (Maybe Text)
aiLastHealthPingTime = lens _aiLastHealthPingTime (\ s a -> s{_aiLastHealthPingTime = a})

-- | Network details about the host where the agent or connector resides.
aiAgentNetworkInfoList :: Lens' AgentInfo [AgentNetworkInfo]
aiAgentNetworkInfoList = lens _aiAgentNetworkInfoList (\ s a -> s{_aiAgentNetworkInfoList = a}) . _Default . _Coerce

-- | The ID of the connector.
aiConnectorId :: Lens' AgentInfo (Maybe Text)
aiConnectorId = lens _aiConnectorId (\ s a -> s{_aiConnectorId = a})

-- | The health of the agent or connector.
aiHealth :: Lens' AgentInfo (Maybe AgentStatus)
aiHealth = lens _aiHealth (\ s a -> s{_aiHealth = a})

-- | The agent or connector ID.
aiAgentId :: Lens' AgentInfo (Maybe Text)
aiAgentId = lens _aiAgentId (\ s a -> s{_aiAgentId = a})

-- | The agent or connector version.
aiVersion :: Lens' AgentInfo (Maybe Text)
aiVersion = lens _aiVersion (\ s a -> s{_aiVersion = a})

-- | Status of the collection process for an agent or connector.
aiCollectionStatus :: Lens' AgentInfo (Maybe Text)
aiCollectionStatus = lens _aiCollectionStatus (\ s a -> s{_aiCollectionStatus = a})

-- | Agent's first registration timestamp in UTC.
aiRegisteredTime :: Lens' AgentInfo (Maybe Text)
aiRegisteredTime = lens _aiRegisteredTime (\ s a -> s{_aiRegisteredTime = a})

-- | Type of agent.
aiAgentType :: Lens' AgentInfo (Maybe Text)
aiAgentType = lens _aiAgentType (\ s a -> s{_aiAgentType = a})

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

instance Hashable AgentInfo where

instance NFData AgentInfo where

-- | Network details about the host where the agent/connector resides.
--
--
--
-- /See:/ 'agentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { _aniIpAddress  :: !(Maybe Text)
  , _aniMacAddress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentNetworkInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aniIpAddress' - The IP address for the host where the agent/connector resides.
--
-- * 'aniMacAddress' - The MAC address for the host where the agent/connector resides.
agentNetworkInfo
    :: AgentNetworkInfo
agentNetworkInfo =
  AgentNetworkInfo' {_aniIpAddress = Nothing, _aniMacAddress = Nothing}


-- | The IP address for the host where the agent/connector resides.
aniIpAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniIpAddress = lens _aniIpAddress (\ s a -> s{_aniIpAddress = a})

-- | The MAC address for the host where the agent/connector resides.
aniMacAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniMacAddress = lens _aniMacAddress (\ s a -> s{_aniMacAddress = a})

instance FromJSON AgentNetworkInfo where
        parseJSON
          = withObject "AgentNetworkInfo"
              (\ x ->
                 AgentNetworkInfo' <$>
                   (x .:? "ipAddress") <*> (x .:? "macAddress"))

instance Hashable AgentNetworkInfo where

instance NFData AgentNetworkInfo where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctTimeOfCreation' - The time the configuration tag was created in Coordinated Universal Time (UTC).
--
-- * 'ctConfigurationId' - The configuration ID for the item to tag. You can specify a list of keys and values.
--
-- * 'ctConfigurationType' - A type of IT asset to tag.
--
-- * 'ctValue' - A value on which to filter. For example /key = serverType/ and /value = web server/ .
--
-- * 'ctKey' - A type of tag on which to filter. For example, /serverType/ .
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
ctTimeOfCreation = lens _ctTimeOfCreation (\ s a -> s{_ctTimeOfCreation = a}) . mapping _Time

-- | The configuration ID for the item to tag. You can specify a list of keys and values.
ctConfigurationId :: Lens' ConfigurationTag (Maybe Text)
ctConfigurationId = lens _ctConfigurationId (\ s a -> s{_ctConfigurationId = a})

-- | A type of IT asset to tag.
ctConfigurationType :: Lens' ConfigurationTag (Maybe ConfigurationItemType)
ctConfigurationType = lens _ctConfigurationType (\ s a -> s{_ctConfigurationType = a})

-- | A value on which to filter. For example /key = serverType/ and /value = web server/ .
ctValue :: Lens' ConfigurationTag (Maybe Text)
ctValue = lens _ctValue (\ s a -> s{_ctValue = a})

-- | A type of tag on which to filter. For example, /serverType/ .
ctKey :: Lens' ConfigurationTag (Maybe Text)
ctKey = lens _ctKey (\ s a -> s{_ctKey = a})

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

instance Hashable ConfigurationTag where

instance NFData ConfigurationTag where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
caiActiveAgents = lens _caiActiveAgents (\ s a -> s{_caiActiveAgents = a})

-- | Number of healthy discovery agents
caiHealthyAgents :: Lens' CustomerAgentInfo Int
caiHealthyAgents = lens _caiHealthyAgents (\ s a -> s{_caiHealthyAgents = a})

-- | Number of blacklisted discovery agents.
caiBlackListedAgents :: Lens' CustomerAgentInfo Int
caiBlackListedAgents = lens _caiBlackListedAgents (\ s a -> s{_caiBlackListedAgents = a})

-- | Number of discovery agents with status SHUTDOWN.
caiShutdownAgents :: Lens' CustomerAgentInfo Int
caiShutdownAgents = lens _caiShutdownAgents (\ s a -> s{_caiShutdownAgents = a})

-- | Number of unhealthy discovery agents.
caiUnhealthyAgents :: Lens' CustomerAgentInfo Int
caiUnhealthyAgents = lens _caiUnhealthyAgents (\ s a -> s{_caiUnhealthyAgents = a})

-- | Total number of discovery agents.
caiTotalAgents :: Lens' CustomerAgentInfo Int
caiTotalAgents = lens _caiTotalAgents (\ s a -> s{_caiTotalAgents = a})

-- | Number of unknown discovery agents.
caiUnknownAgents :: Lens' CustomerAgentInfo Int
caiUnknownAgents = lens _caiUnknownAgents (\ s a -> s{_caiUnknownAgents = a})

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

instance Hashable CustomerAgentInfo where

instance NFData CustomerAgentInfo where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
cciActiveConnectors = lens _cciActiveConnectors (\ s a -> s{_cciActiveConnectors = a})

-- | Number of healthy discovery connectors.
cciHealthyConnectors :: Lens' CustomerConnectorInfo Int
cciHealthyConnectors = lens _cciHealthyConnectors (\ s a -> s{_cciHealthyConnectors = a})

-- | Number of blacklisted discovery connectors.
cciBlackListedConnectors :: Lens' CustomerConnectorInfo Int
cciBlackListedConnectors = lens _cciBlackListedConnectors (\ s a -> s{_cciBlackListedConnectors = a})

-- | Number of discovery connectors with status SHUTDOWN,
cciShutdownConnectors :: Lens' CustomerConnectorInfo Int
cciShutdownConnectors = lens _cciShutdownConnectors (\ s a -> s{_cciShutdownConnectors = a})

-- | Number of unhealthy discovery connectors.
cciUnhealthyConnectors :: Lens' CustomerConnectorInfo Int
cciUnhealthyConnectors = lens _cciUnhealthyConnectors (\ s a -> s{_cciUnhealthyConnectors = a})

-- | Total number of discovery connectors.
cciTotalConnectors :: Lens' CustomerConnectorInfo Int
cciTotalConnectors = lens _cciTotalConnectors (\ s a -> s{_cciTotalConnectors = a})

-- | Number of unknown discovery connectors.
cciUnknownConnectors :: Lens' CustomerConnectorInfo Int
cciUnknownConnectors = lens _cciUnknownConnectors (\ s a -> s{_cciUnknownConnectors = a})

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

instance Hashable CustomerConnectorInfo where

instance NFData CustomerConnectorInfo where

-- | Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask> action.
--
--
--
-- /See:/ 'exportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { _efName      :: !Text
  , _efValues    :: ![Text]
  , _efCondition :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efName' - A single @ExportFilter@ name. Supported filters: @agentId@ .
--
-- * 'efValues' - A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
--
-- * 'efCondition' - Supported condition: @EQUALS@
exportFilter
    :: Text -- ^ 'efName'
    -> Text -- ^ 'efCondition'
    -> ExportFilter
exportFilter pName_ pCondition_ =
  ExportFilter'
    {_efName = pName_, _efValues = mempty, _efCondition = pCondition_}


-- | A single @ExportFilter@ name. Supported filters: @agentId@ .
efName :: Lens' ExportFilter Text
efName = lens _efName (\ s a -> s{_efName = a})

-- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
efValues :: Lens' ExportFilter [Text]
efValues = lens _efValues (\ s a -> s{_efValues = a}) . _Coerce

-- | Supported condition: @EQUALS@
efCondition :: Lens' ExportFilter Text
efCondition = lens _efCondition (\ s a -> s{_efCondition = a})

instance Hashable ExportFilter where

instance NFData ExportFilter where

instance ToJSON ExportFilter where
        toJSON ExportFilter'{..}
          = object
              (catMaybes
                 [Just ("name" .= _efName),
                  Just ("values" .= _efValues),
                  Just ("condition" .= _efCondition)])

-- | Information regarding the export status of discovered data. The value is an array of objects.
--
--
--
-- /See:/ 'exportInfo' smart constructor.
data ExportInfo = ExportInfo'
  { _eiConfigurationsDownloadURL :: !(Maybe Text)
  , _eiRequestedStartTime        :: !(Maybe POSIX)
  , _eiRequestedEndTime          :: !(Maybe POSIX)
  , _eiIsTruncated               :: !(Maybe Bool)
  , _eiExportId                  :: !Text
  , _eiExportStatus              :: !ExportStatus
  , _eiStatusMessage             :: !Text
  , _eiExportRequestTime         :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiConfigurationsDownloadURL' - A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
--
-- * 'eiRequestedStartTime' - The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- * 'eiRequestedEndTime' - The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
--
-- * 'eiIsTruncated' - If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
--
-- * 'eiExportId' - A unique identifier used to query an export.
--
-- * 'eiExportStatus' - The status of the data export job.
--
-- * 'eiStatusMessage' - A status message provided for API callers.
--
-- * 'eiExportRequestTime' - The time that the data export was initiated.
exportInfo
    :: Text -- ^ 'eiExportId'
    -> ExportStatus -- ^ 'eiExportStatus'
    -> Text -- ^ 'eiStatusMessage'
    -> UTCTime -- ^ 'eiExportRequestTime'
    -> ExportInfo
exportInfo pExportId_ pExportStatus_ pStatusMessage_ pExportRequestTime_ =
  ExportInfo'
    { _eiConfigurationsDownloadURL = Nothing
    , _eiRequestedStartTime = Nothing
    , _eiRequestedEndTime = Nothing
    , _eiIsTruncated = Nothing
    , _eiExportId = pExportId_
    , _eiExportStatus = pExportStatus_
    , _eiStatusMessage = pStatusMessage_
    , _eiExportRequestTime = _Time # pExportRequestTime_
    }


-- | A URL for an Amazon S3 bucket where you can review the exported data. The URL is displayed only if the export succeeded.
eiConfigurationsDownloadURL :: Lens' ExportInfo (Maybe Text)
eiConfigurationsDownloadURL = lens _eiConfigurationsDownloadURL (\ s a -> s{_eiConfigurationsDownloadURL = a})

-- | The value of @startTime@ parameter in the @StartExportTask@ request. If no @startTime@ was requested, this result does not appear in @ExportInfo@ .
eiRequestedStartTime :: Lens' ExportInfo (Maybe UTCTime)
eiRequestedStartTime = lens _eiRequestedStartTime (\ s a -> s{_eiRequestedStartTime = a}) . mapping _Time

-- | The @endTime@ used in the @StartExportTask@ request. If no @endTime@ was requested, this result does not appear in @ExportInfo@ .
eiRequestedEndTime :: Lens' ExportInfo (Maybe UTCTime)
eiRequestedEndTime = lens _eiRequestedEndTime (\ s a -> s{_eiRequestedEndTime = a}) . mapping _Time

-- | If true, the export of agent information exceeded the size limit for a single export and the exported data is incomplete for the requested time range. To address this, select a smaller time range for the export by using @startDate@ and @endDate@ .
eiIsTruncated :: Lens' ExportInfo (Maybe Bool)
eiIsTruncated = lens _eiIsTruncated (\ s a -> s{_eiIsTruncated = a})

-- | A unique identifier used to query an export.
eiExportId :: Lens' ExportInfo Text
eiExportId = lens _eiExportId (\ s a -> s{_eiExportId = a})

-- | The status of the data export job.
eiExportStatus :: Lens' ExportInfo ExportStatus
eiExportStatus = lens _eiExportStatus (\ s a -> s{_eiExportStatus = a})

-- | A status message provided for API callers.
eiStatusMessage :: Lens' ExportInfo Text
eiStatusMessage = lens _eiStatusMessage (\ s a -> s{_eiStatusMessage = a})

-- | The time that the data export was initiated.
eiExportRequestTime :: Lens' ExportInfo UTCTime
eiExportRequestTime = lens _eiExportRequestTime (\ s a -> s{_eiExportRequestTime = a}) . _Time

instance FromJSON ExportInfo where
        parseJSON
          = withObject "ExportInfo"
              (\ x ->
                 ExportInfo' <$>
                   (x .:? "configurationsDownloadUrl") <*>
                     (x .:? "requestedStartTime")
                     <*> (x .:? "requestedEndTime")
                     <*> (x .:? "isTruncated")
                     <*> (x .: "exportId")
                     <*> (x .: "exportStatus")
                     <*> (x .: "statusMessage")
                     <*> (x .: "exportRequestTime"))

instance Hashable ExportInfo where

instance NFData ExportInfo where

-- | A filter that can use conditional operators.
--
--
-- For more information about filters, see <http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html Querying Discovered Configuration Items> .
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fName      :: !Text
  , _fValues    :: ![Text]
  , _fCondition :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter.
--
-- * 'fValues' - A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
--
-- * 'fCondition' - A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
filter'
    :: Text -- ^ 'fName'
    -> Text -- ^ 'fCondition'
    -> Filter
filter' pName_ pCondition_ =
  Filter' {_fName = pName_, _fValues = mempty, _fCondition = pCondition_}


-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
fCondition :: Lens' Filter Text
fCondition = lens _fCondition (\ s a -> s{_fCondition = a})

instance Hashable Filter where

instance NFData Filter where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NeighborConnectionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncdTransportProtocol' - The network protocol used for the connection.
--
-- * 'ncdDestinationPort' - The destination network port for the connection.
--
-- * 'ncdSourceServerId' - The ID of the server that opened the network connection.
--
-- * 'ncdDestinationServerId' - The ID of the server that accepted the network connection.
--
-- * 'ncdConnectionsCount' - The number of open network connections with the neighboring server.
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


-- | The network protocol used for the connection.
ncdTransportProtocol :: Lens' NeighborConnectionDetail (Maybe Text)
ncdTransportProtocol = lens _ncdTransportProtocol (\ s a -> s{_ncdTransportProtocol = a})

-- | The destination network port for the connection.
ncdDestinationPort :: Lens' NeighborConnectionDetail (Maybe Int)
ncdDestinationPort = lens _ncdDestinationPort (\ s a -> s{_ncdDestinationPort = a})

-- | The ID of the server that opened the network connection.
ncdSourceServerId :: Lens' NeighborConnectionDetail Text
ncdSourceServerId = lens _ncdSourceServerId (\ s a -> s{_ncdSourceServerId = a})

-- | The ID of the server that accepted the network connection.
ncdDestinationServerId :: Lens' NeighborConnectionDetail Text
ncdDestinationServerId = lens _ncdDestinationServerId (\ s a -> s{_ncdDestinationServerId = a})

-- | The number of open network connections with the neighboring server.
ncdConnectionsCount :: Lens' NeighborConnectionDetail Integer
ncdConnectionsCount = lens _ncdConnectionsCount (\ s a -> s{_ncdConnectionsCount = a})

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

instance Hashable NeighborConnectionDetail where

instance NFData NeighborConnectionDetail where

-- | A field and direction for ordered output.
--
--
--
-- /See:/ 'orderByElement' smart constructor.
data OrderByElement = OrderByElement'
  { _obeSortOrder :: !(Maybe OrderString)
  , _obeFieldName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrderByElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'obeSortOrder' - Ordering direction.
--
-- * 'obeFieldName' - The field on which to order.
orderByElement
    :: Text -- ^ 'obeFieldName'
    -> OrderByElement
orderByElement pFieldName_ =
  OrderByElement' {_obeSortOrder = Nothing, _obeFieldName = pFieldName_}


-- | Ordering direction.
obeSortOrder :: Lens' OrderByElement (Maybe OrderString)
obeSortOrder = lens _obeSortOrder (\ s a -> s{_obeSortOrder = a})

-- | The field on which to order.
obeFieldName :: Lens' OrderByElement Text
obeFieldName = lens _obeFieldName (\ s a -> s{_obeFieldName = a})

instance Hashable OrderByElement where

instance NFData OrderByElement where

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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The type of tag on which to filter.
--
-- * 'tagValue' - A value for a tag key on which to filter.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The type of tag on which to filter.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | A value for a tag key on which to filter.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("key" .= _tagKey),
                  Just ("value" .= _tagValue)])

-- | The tag filter. Valid names are: @tagKey@ , @tagValue@ , @configurationId@ .
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
  { _tfName   :: !Text
  , _tfValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfName' - A name of the tag filter.
--
-- * 'tfValues' - Values for the tag filter.
tagFilter
    :: Text -- ^ 'tfName'
    -> TagFilter
tagFilter pName_ = TagFilter' {_tfName = pName_, _tfValues = mempty}


-- | A name of the tag filter.
tfName :: Lens' TagFilter Text
tfName = lens _tfName (\ s a -> s{_tfName = a})

-- | Values for the tag filter.
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\ s a -> s{_tfValues = a}) . _Coerce

instance Hashable TagFilter where

instance NFData TagFilter where

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              (catMaybes
                 [Just ("name" .= _tfName),
                  Just ("values" .= _tfValues)])
