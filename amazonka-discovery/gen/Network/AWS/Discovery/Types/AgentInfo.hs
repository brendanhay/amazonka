{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentInfo where

import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about agents or connectors associated with the user’s AWS
-- account. Information includes agent or connector IDs, IP addresses,
-- media access control (MAC) addresses, agent or connector health,
-- hostname where the agent or connector resides, and agent version for
-- each agent.
--
-- /See:/ 'newAgentInfo' smart constructor.
data AgentInfo = AgentInfo'
  { -- | The name of the host where the agent or connector resides. The host can
    -- be a server or virtual machine.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The agent or connector ID.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | Type of agent.
    agentType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
    -- | Network details about the host where the agent or connector resides.
    agentNetworkInfoList :: Prelude.Maybe [AgentNetworkInfo],
    -- | Time since agent or connector health was reported.
    lastHealthPingTime :: Prelude.Maybe Prelude.Text,
    -- | Agent\'s first registration timestamp in UTC.
    registeredTime :: Prelude.Maybe Prelude.Text,
    -- | The agent or connector version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The health of the agent or connector.
    health :: Prelude.Maybe AgentStatus,
    -- | Status of the collection process for an agent or connector.
    collectionStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AgentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostName', 'agentInfo_hostName' - The name of the host where the agent or connector resides. The host can
-- be a server or virtual machine.
--
-- 'agentId', 'agentInfo_agentId' - The agent or connector ID.
--
-- 'agentType', 'agentInfo_agentType' - Type of agent.
--
-- 'connectorId', 'agentInfo_connectorId' - The ID of the connector.
--
-- 'agentNetworkInfoList', 'agentInfo_agentNetworkInfoList' - Network details about the host where the agent or connector resides.
--
-- 'lastHealthPingTime', 'agentInfo_lastHealthPingTime' - Time since agent or connector health was reported.
--
-- 'registeredTime', 'agentInfo_registeredTime' - Agent\'s first registration timestamp in UTC.
--
-- 'version', 'agentInfo_version' - The agent or connector version.
--
-- 'health', 'agentInfo_health' - The health of the agent or connector.
--
-- 'collectionStatus', 'agentInfo_collectionStatus' - Status of the collection process for an agent or connector.
newAgentInfo ::
  AgentInfo
newAgentInfo =
  AgentInfo'
    { hostName = Prelude.Nothing,
      agentId = Prelude.Nothing,
      agentType = Prelude.Nothing,
      connectorId = Prelude.Nothing,
      agentNetworkInfoList = Prelude.Nothing,
      lastHealthPingTime = Prelude.Nothing,
      registeredTime = Prelude.Nothing,
      version = Prelude.Nothing,
      health = Prelude.Nothing,
      collectionStatus = Prelude.Nothing
    }

-- | The name of the host where the agent or connector resides. The host can
-- be a server or virtual machine.
agentInfo_hostName :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_hostName = Lens.lens (\AgentInfo' {hostName} -> hostName) (\s@AgentInfo' {} a -> s {hostName = a} :: AgentInfo)

-- | The agent or connector ID.
agentInfo_agentId :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_agentId = Lens.lens (\AgentInfo' {agentId} -> agentId) (\s@AgentInfo' {} a -> s {agentId = a} :: AgentInfo)

-- | Type of agent.
agentInfo_agentType :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_agentType = Lens.lens (\AgentInfo' {agentType} -> agentType) (\s@AgentInfo' {} a -> s {agentType = a} :: AgentInfo)

-- | The ID of the connector.
agentInfo_connectorId :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_connectorId = Lens.lens (\AgentInfo' {connectorId} -> connectorId) (\s@AgentInfo' {} a -> s {connectorId = a} :: AgentInfo)

-- | Network details about the host where the agent or connector resides.
agentInfo_agentNetworkInfoList :: Lens.Lens' AgentInfo (Prelude.Maybe [AgentNetworkInfo])
agentInfo_agentNetworkInfoList = Lens.lens (\AgentInfo' {agentNetworkInfoList} -> agentNetworkInfoList) (\s@AgentInfo' {} a -> s {agentNetworkInfoList = a} :: AgentInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Time since agent or connector health was reported.
agentInfo_lastHealthPingTime :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_lastHealthPingTime = Lens.lens (\AgentInfo' {lastHealthPingTime} -> lastHealthPingTime) (\s@AgentInfo' {} a -> s {lastHealthPingTime = a} :: AgentInfo)

-- | Agent\'s first registration timestamp in UTC.
agentInfo_registeredTime :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_registeredTime = Lens.lens (\AgentInfo' {registeredTime} -> registeredTime) (\s@AgentInfo' {} a -> s {registeredTime = a} :: AgentInfo)

-- | The agent or connector version.
agentInfo_version :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_version = Lens.lens (\AgentInfo' {version} -> version) (\s@AgentInfo' {} a -> s {version = a} :: AgentInfo)

-- | The health of the agent or connector.
agentInfo_health :: Lens.Lens' AgentInfo (Prelude.Maybe AgentStatus)
agentInfo_health = Lens.lens (\AgentInfo' {health} -> health) (\s@AgentInfo' {} a -> s {health = a} :: AgentInfo)

-- | Status of the collection process for an agent or connector.
agentInfo_collectionStatus :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_collectionStatus = Lens.lens (\AgentInfo' {collectionStatus} -> collectionStatus) (\s@AgentInfo' {} a -> s {collectionStatus = a} :: AgentInfo)

instance Prelude.FromJSON AgentInfo where
  parseJSON =
    Prelude.withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            Prelude.<$> (x Prelude..:? "hostName")
            Prelude.<*> (x Prelude..:? "agentId")
            Prelude.<*> (x Prelude..:? "agentType")
            Prelude.<*> (x Prelude..:? "connectorId")
            Prelude.<*> ( x Prelude..:? "agentNetworkInfoList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "lastHealthPingTime")
            Prelude.<*> (x Prelude..:? "registeredTime")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "health")
            Prelude.<*> (x Prelude..:? "collectionStatus")
      )

instance Prelude.Hashable AgentInfo

instance Prelude.NFData AgentInfo
