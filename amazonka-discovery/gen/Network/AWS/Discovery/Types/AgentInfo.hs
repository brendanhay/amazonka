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

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import qualified Network.AWS.Lens as Lens

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
    hostName :: Core.Maybe Core.Text,
    -- | The agent or connector ID.
    agentId :: Core.Maybe Core.Text,
    -- | Type of agent.
    agentType :: Core.Maybe Core.Text,
    -- | The ID of the connector.
    connectorId :: Core.Maybe Core.Text,
    -- | Network details about the host where the agent or connector resides.
    agentNetworkInfoList :: Core.Maybe [AgentNetworkInfo],
    -- | Time since agent or connector health was reported.
    lastHealthPingTime :: Core.Maybe Core.Text,
    -- | Agent\'s first registration timestamp in UTC.
    registeredTime :: Core.Maybe Core.Text,
    -- | The agent or connector version.
    version :: Core.Maybe Core.Text,
    -- | The health of the agent or connector.
    health :: Core.Maybe AgentStatus,
    -- | Status of the collection process for an agent or connector.
    collectionStatus :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { hostName = Core.Nothing,
      agentId = Core.Nothing,
      agentType = Core.Nothing,
      connectorId = Core.Nothing,
      agentNetworkInfoList = Core.Nothing,
      lastHealthPingTime = Core.Nothing,
      registeredTime = Core.Nothing,
      version = Core.Nothing,
      health = Core.Nothing,
      collectionStatus = Core.Nothing
    }

-- | The name of the host where the agent or connector resides. The host can
-- be a server or virtual machine.
agentInfo_hostName :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_hostName = Lens.lens (\AgentInfo' {hostName} -> hostName) (\s@AgentInfo' {} a -> s {hostName = a} :: AgentInfo)

-- | The agent or connector ID.
agentInfo_agentId :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_agentId = Lens.lens (\AgentInfo' {agentId} -> agentId) (\s@AgentInfo' {} a -> s {agentId = a} :: AgentInfo)

-- | Type of agent.
agentInfo_agentType :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_agentType = Lens.lens (\AgentInfo' {agentType} -> agentType) (\s@AgentInfo' {} a -> s {agentType = a} :: AgentInfo)

-- | The ID of the connector.
agentInfo_connectorId :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_connectorId = Lens.lens (\AgentInfo' {connectorId} -> connectorId) (\s@AgentInfo' {} a -> s {connectorId = a} :: AgentInfo)

-- | Network details about the host where the agent or connector resides.
agentInfo_agentNetworkInfoList :: Lens.Lens' AgentInfo (Core.Maybe [AgentNetworkInfo])
agentInfo_agentNetworkInfoList = Lens.lens (\AgentInfo' {agentNetworkInfoList} -> agentNetworkInfoList) (\s@AgentInfo' {} a -> s {agentNetworkInfoList = a} :: AgentInfo) Core.. Lens.mapping Lens._Coerce

-- | Time since agent or connector health was reported.
agentInfo_lastHealthPingTime :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_lastHealthPingTime = Lens.lens (\AgentInfo' {lastHealthPingTime} -> lastHealthPingTime) (\s@AgentInfo' {} a -> s {lastHealthPingTime = a} :: AgentInfo)

-- | Agent\'s first registration timestamp in UTC.
agentInfo_registeredTime :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_registeredTime = Lens.lens (\AgentInfo' {registeredTime} -> registeredTime) (\s@AgentInfo' {} a -> s {registeredTime = a} :: AgentInfo)

-- | The agent or connector version.
agentInfo_version :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_version = Lens.lens (\AgentInfo' {version} -> version) (\s@AgentInfo' {} a -> s {version = a} :: AgentInfo)

-- | The health of the agent or connector.
agentInfo_health :: Lens.Lens' AgentInfo (Core.Maybe AgentStatus)
agentInfo_health = Lens.lens (\AgentInfo' {health} -> health) (\s@AgentInfo' {} a -> s {health = a} :: AgentInfo)

-- | Status of the collection process for an agent or connector.
agentInfo_collectionStatus :: Lens.Lens' AgentInfo (Core.Maybe Core.Text)
agentInfo_collectionStatus = Lens.lens (\AgentInfo' {collectionStatus} -> collectionStatus) (\s@AgentInfo' {} a -> s {collectionStatus = a} :: AgentInfo)

instance Core.FromJSON AgentInfo where
  parseJSON =
    Core.withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            Core.<$> (x Core..:? "hostName")
            Core.<*> (x Core..:? "agentId")
            Core.<*> (x Core..:? "agentType")
            Core.<*> (x Core..:? "connectorId")
            Core.<*> ( x Core..:? "agentNetworkInfoList"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "lastHealthPingTime")
            Core.<*> (x Core..:? "registeredTime")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "health")
            Core.<*> (x Core..:? "collectionStatus")
      )

instance Core.Hashable AgentInfo

instance Core.NFData AgentInfo
