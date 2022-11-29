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
-- Module      : Amazonka.Discovery.Types.AgentInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.AgentInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types.AgentNetworkInfo
import Amazonka.Discovery.Types.AgentStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about agents or connectors associated with the user’s Amazon
-- Web Services account. Information includes agent or connector IDs, IP
-- addresses, media access control (MAC) addresses, agent or connector
-- health, hostname where the agent or connector resides, and agent version
-- for each agent.
--
-- /See:/ 'newAgentInfo' smart constructor.
data AgentInfo = AgentInfo'
  { -- | Type of agent.
    agentType :: Prelude.Maybe Prelude.Text,
    -- | Status of the collection process for an agent or connector.
    collectionStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
    -- | Time since agent or connector health was reported.
    lastHealthPingTime :: Prelude.Maybe Prelude.Text,
    -- | The name of the host where the agent or connector resides. The host can
    -- be a server or virtual machine.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The agent or connector ID.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | The health of the agent or connector.
    health :: Prelude.Maybe AgentStatus,
    -- | Network details about the host where the agent or connector resides.
    agentNetworkInfoList :: Prelude.Maybe [AgentNetworkInfo],
    -- | Agent\'s first registration timestamp in UTC.
    registeredTime :: Prelude.Maybe Prelude.Text,
    -- | The agent or connector version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentType', 'agentInfo_agentType' - Type of agent.
--
-- 'collectionStatus', 'agentInfo_collectionStatus' - Status of the collection process for an agent or connector.
--
-- 'connectorId', 'agentInfo_connectorId' - The ID of the connector.
--
-- 'lastHealthPingTime', 'agentInfo_lastHealthPingTime' - Time since agent or connector health was reported.
--
-- 'hostName', 'agentInfo_hostName' - The name of the host where the agent or connector resides. The host can
-- be a server or virtual machine.
--
-- 'agentId', 'agentInfo_agentId' - The agent or connector ID.
--
-- 'health', 'agentInfo_health' - The health of the agent or connector.
--
-- 'agentNetworkInfoList', 'agentInfo_agentNetworkInfoList' - Network details about the host where the agent or connector resides.
--
-- 'registeredTime', 'agentInfo_registeredTime' - Agent\'s first registration timestamp in UTC.
--
-- 'version', 'agentInfo_version' - The agent or connector version.
newAgentInfo ::
  AgentInfo
newAgentInfo =
  AgentInfo'
    { agentType = Prelude.Nothing,
      collectionStatus = Prelude.Nothing,
      connectorId = Prelude.Nothing,
      lastHealthPingTime = Prelude.Nothing,
      hostName = Prelude.Nothing,
      agentId = Prelude.Nothing,
      health = Prelude.Nothing,
      agentNetworkInfoList = Prelude.Nothing,
      registeredTime = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Type of agent.
agentInfo_agentType :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_agentType = Lens.lens (\AgentInfo' {agentType} -> agentType) (\s@AgentInfo' {} a -> s {agentType = a} :: AgentInfo)

-- | Status of the collection process for an agent or connector.
agentInfo_collectionStatus :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_collectionStatus = Lens.lens (\AgentInfo' {collectionStatus} -> collectionStatus) (\s@AgentInfo' {} a -> s {collectionStatus = a} :: AgentInfo)

-- | The ID of the connector.
agentInfo_connectorId :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_connectorId = Lens.lens (\AgentInfo' {connectorId} -> connectorId) (\s@AgentInfo' {} a -> s {connectorId = a} :: AgentInfo)

-- | Time since agent or connector health was reported.
agentInfo_lastHealthPingTime :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_lastHealthPingTime = Lens.lens (\AgentInfo' {lastHealthPingTime} -> lastHealthPingTime) (\s@AgentInfo' {} a -> s {lastHealthPingTime = a} :: AgentInfo)

-- | The name of the host where the agent or connector resides. The host can
-- be a server or virtual machine.
agentInfo_hostName :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_hostName = Lens.lens (\AgentInfo' {hostName} -> hostName) (\s@AgentInfo' {} a -> s {hostName = a} :: AgentInfo)

-- | The agent or connector ID.
agentInfo_agentId :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_agentId = Lens.lens (\AgentInfo' {agentId} -> agentId) (\s@AgentInfo' {} a -> s {agentId = a} :: AgentInfo)

-- | The health of the agent or connector.
agentInfo_health :: Lens.Lens' AgentInfo (Prelude.Maybe AgentStatus)
agentInfo_health = Lens.lens (\AgentInfo' {health} -> health) (\s@AgentInfo' {} a -> s {health = a} :: AgentInfo)

-- | Network details about the host where the agent or connector resides.
agentInfo_agentNetworkInfoList :: Lens.Lens' AgentInfo (Prelude.Maybe [AgentNetworkInfo])
agentInfo_agentNetworkInfoList = Lens.lens (\AgentInfo' {agentNetworkInfoList} -> agentNetworkInfoList) (\s@AgentInfo' {} a -> s {agentNetworkInfoList = a} :: AgentInfo) Prelude.. Lens.mapping Lens.coerced

-- | Agent\'s first registration timestamp in UTC.
agentInfo_registeredTime :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_registeredTime = Lens.lens (\AgentInfo' {registeredTime} -> registeredTime) (\s@AgentInfo' {} a -> s {registeredTime = a} :: AgentInfo)

-- | The agent or connector version.
agentInfo_version :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_version = Lens.lens (\AgentInfo' {version} -> version) (\s@AgentInfo' {} a -> s {version = a} :: AgentInfo)

instance Core.FromJSON AgentInfo where
  parseJSON =
    Core.withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            Prelude.<$> (x Core..:? "agentType")
            Prelude.<*> (x Core..:? "collectionStatus")
            Prelude.<*> (x Core..:? "connectorId")
            Prelude.<*> (x Core..:? "lastHealthPingTime")
            Prelude.<*> (x Core..:? "hostName")
            Prelude.<*> (x Core..:? "agentId")
            Prelude.<*> (x Core..:? "health")
            Prelude.<*> ( x Core..:? "agentNetworkInfoList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "registeredTime")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable AgentInfo where
  hashWithSalt _salt AgentInfo' {..} =
    _salt `Prelude.hashWithSalt` agentType
      `Prelude.hashWithSalt` collectionStatus
      `Prelude.hashWithSalt` connectorId
      `Prelude.hashWithSalt` lastHealthPingTime
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` agentNetworkInfoList
      `Prelude.hashWithSalt` registeredTime
      `Prelude.hashWithSalt` version

instance Prelude.NFData AgentInfo where
  rnf AgentInfo' {..} =
    Prelude.rnf agentType
      `Prelude.seq` Prelude.rnf collectionStatus
      `Prelude.seq` Prelude.rnf connectorId
      `Prelude.seq` Prelude.rnf lastHealthPingTime
      `Prelude.seq` Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf agentId
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf agentNetworkInfoList
      `Prelude.seq` Prelude.rnf registeredTime
      `Prelude.seq` Prelude.rnf version
