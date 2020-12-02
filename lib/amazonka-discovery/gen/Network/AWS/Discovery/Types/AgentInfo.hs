{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentInfo where

import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.
--
--
--
-- /See:/ 'agentInfo' smart constructor.
data AgentInfo = AgentInfo'
  { _aiHostName :: !(Maybe Text),
    _aiLastHealthPingTime :: !(Maybe Text),
    _aiAgentNetworkInfoList :: !(Maybe [AgentNetworkInfo]),
    _aiConnectorId :: !(Maybe Text),
    _aiHealth :: !(Maybe AgentStatus),
    _aiAgentId :: !(Maybe Text),
    _aiVersion :: !(Maybe Text),
    _aiCollectionStatus :: !(Maybe Text),
    _aiRegisteredTime :: !(Maybe Text),
    _aiAgentType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
agentInfo ::
  AgentInfo
agentInfo =
  AgentInfo'
    { _aiHostName = Nothing,
      _aiLastHealthPingTime = Nothing,
      _aiAgentNetworkInfoList = Nothing,
      _aiConnectorId = Nothing,
      _aiHealth = Nothing,
      _aiAgentId = Nothing,
      _aiVersion = Nothing,
      _aiCollectionStatus = Nothing,
      _aiRegisteredTime = Nothing,
      _aiAgentType = Nothing
    }

-- | The name of the host where the agent or connector resides. The host can be a server or virtual machine.
aiHostName :: Lens' AgentInfo (Maybe Text)
aiHostName = lens _aiHostName (\s a -> s {_aiHostName = a})

-- | Time since agent or connector health was reported.
aiLastHealthPingTime :: Lens' AgentInfo (Maybe Text)
aiLastHealthPingTime = lens _aiLastHealthPingTime (\s a -> s {_aiLastHealthPingTime = a})

-- | Network details about the host where the agent or connector resides.
aiAgentNetworkInfoList :: Lens' AgentInfo [AgentNetworkInfo]
aiAgentNetworkInfoList = lens _aiAgentNetworkInfoList (\s a -> s {_aiAgentNetworkInfoList = a}) . _Default . _Coerce

-- | The ID of the connector.
aiConnectorId :: Lens' AgentInfo (Maybe Text)
aiConnectorId = lens _aiConnectorId (\s a -> s {_aiConnectorId = a})

-- | The health of the agent or connector.
aiHealth :: Lens' AgentInfo (Maybe AgentStatus)
aiHealth = lens _aiHealth (\s a -> s {_aiHealth = a})

-- | The agent or connector ID.
aiAgentId :: Lens' AgentInfo (Maybe Text)
aiAgentId = lens _aiAgentId (\s a -> s {_aiAgentId = a})

-- | The agent or connector version.
aiVersion :: Lens' AgentInfo (Maybe Text)
aiVersion = lens _aiVersion (\s a -> s {_aiVersion = a})

-- | Status of the collection process for an agent or connector.
aiCollectionStatus :: Lens' AgentInfo (Maybe Text)
aiCollectionStatus = lens _aiCollectionStatus (\s a -> s {_aiCollectionStatus = a})

-- | Agent's first registration timestamp in UTC.
aiRegisteredTime :: Lens' AgentInfo (Maybe Text)
aiRegisteredTime = lens _aiRegisteredTime (\s a -> s {_aiRegisteredTime = a})

-- | Type of agent.
aiAgentType :: Lens' AgentInfo (Maybe Text)
aiAgentType = lens _aiAgentType (\s a -> s {_aiAgentType = a})

instance FromJSON AgentInfo where
  parseJSON =
    withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            <$> (x .:? "hostName")
            <*> (x .:? "lastHealthPingTime")
            <*> (x .:? "agentNetworkInfoList" .!= mempty)
            <*> (x .:? "connectorId")
            <*> (x .:? "health")
            <*> (x .:? "agentId")
            <*> (x .:? "version")
            <*> (x .:? "collectionStatus")
            <*> (x .:? "registeredTime")
            <*> (x .:? "agentType")
      )

instance Hashable AgentInfo

instance NFData AgentInfo
