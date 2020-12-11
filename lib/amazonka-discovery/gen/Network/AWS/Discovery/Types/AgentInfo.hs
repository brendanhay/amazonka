-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentInfo
  ( AgentInfo (..),

    -- * Smart constructor
    mkAgentInfo,

    -- * Lenses
    aiHostName,
    aiLastHealthPingTime,
    aiAgentNetworkInfoList,
    aiConnectorId,
    aiHealth,
    aiAgentId,
    aiVersion,
    aiCollectionStatus,
    aiRegisteredTime,
    aiAgentType,
  )
where

import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.
--
-- /See:/ 'mkAgentInfo' smart constructor.
data AgentInfo = AgentInfo'
  { hostName :: Lude.Maybe Lude.Text,
    lastHealthPingTime :: Lude.Maybe Lude.Text,
    agentNetworkInfoList :: Lude.Maybe [AgentNetworkInfo],
    connectorId :: Lude.Maybe Lude.Text,
    health :: Lude.Maybe AgentStatus,
    agentId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    collectionStatus :: Lude.Maybe Lude.Text,
    registeredTime :: Lude.Maybe Lude.Text,
    agentType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentInfo' with the minimum fields required to make a request.
--
-- * 'agentId' - The agent or connector ID.
-- * 'agentNetworkInfoList' - Network details about the host where the agent or connector resides.
-- * 'agentType' - Type of agent.
-- * 'collectionStatus' - Status of the collection process for an agent or connector.
-- * 'connectorId' - The ID of the connector.
-- * 'health' - The health of the agent or connector.
-- * 'hostName' - The name of the host where the agent or connector resides. The host can be a server or virtual machine.
-- * 'lastHealthPingTime' - Time since agent or connector health was reported.
-- * 'registeredTime' - Agent's first registration timestamp in UTC.
-- * 'version' - The agent or connector version.
mkAgentInfo ::
  AgentInfo
mkAgentInfo =
  AgentInfo'
    { hostName = Lude.Nothing,
      lastHealthPingTime = Lude.Nothing,
      agentNetworkInfoList = Lude.Nothing,
      connectorId = Lude.Nothing,
      health = Lude.Nothing,
      agentId = Lude.Nothing,
      version = Lude.Nothing,
      collectionStatus = Lude.Nothing,
      registeredTime = Lude.Nothing,
      agentType = Lude.Nothing
    }

-- | The name of the host where the agent or connector resides. The host can be a server or virtual machine.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiHostName :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiHostName = Lens.lens (hostName :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {hostName = a} :: AgentInfo)
{-# DEPRECATED aiHostName "Use generic-lens or generic-optics with 'hostName' instead." #-}

-- | Time since agent or connector health was reported.
--
-- /Note:/ Consider using 'lastHealthPingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiLastHealthPingTime :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiLastHealthPingTime = Lens.lens (lastHealthPingTime :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {lastHealthPingTime = a} :: AgentInfo)
{-# DEPRECATED aiLastHealthPingTime "Use generic-lens or generic-optics with 'lastHealthPingTime' instead." #-}

-- | Network details about the host where the agent or connector resides.
--
-- /Note:/ Consider using 'agentNetworkInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiAgentNetworkInfoList :: Lens.Lens' AgentInfo (Lude.Maybe [AgentNetworkInfo])
aiAgentNetworkInfoList = Lens.lens (agentNetworkInfoList :: AgentInfo -> Lude.Maybe [AgentNetworkInfo]) (\s a -> s {agentNetworkInfoList = a} :: AgentInfo)
{-# DEPRECATED aiAgentNetworkInfoList "Use generic-lens or generic-optics with 'agentNetworkInfoList' instead." #-}

-- | The ID of the connector.
--
-- /Note:/ Consider using 'connectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiConnectorId :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiConnectorId = Lens.lens (connectorId :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {connectorId = a} :: AgentInfo)
{-# DEPRECATED aiConnectorId "Use generic-lens or generic-optics with 'connectorId' instead." #-}

-- | The health of the agent or connector.
--
-- /Note:/ Consider using 'health' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiHealth :: Lens.Lens' AgentInfo (Lude.Maybe AgentStatus)
aiHealth = Lens.lens (health :: AgentInfo -> Lude.Maybe AgentStatus) (\s a -> s {health = a} :: AgentInfo)
{-# DEPRECATED aiHealth "Use generic-lens or generic-optics with 'health' instead." #-}

-- | The agent or connector ID.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiAgentId :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiAgentId = Lens.lens (agentId :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {agentId = a} :: AgentInfo)
{-# DEPRECATED aiAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

-- | The agent or connector version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiVersion :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiVersion = Lens.lens (version :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: AgentInfo)
{-# DEPRECATED aiVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Status of the collection process for an agent or connector.
--
-- /Note:/ Consider using 'collectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiCollectionStatus :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiCollectionStatus = Lens.lens (collectionStatus :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {collectionStatus = a} :: AgentInfo)
{-# DEPRECATED aiCollectionStatus "Use generic-lens or generic-optics with 'collectionStatus' instead." #-}

-- | Agent's first registration timestamp in UTC.
--
-- /Note:/ Consider using 'registeredTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiRegisteredTime :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiRegisteredTime = Lens.lens (registeredTime :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {registeredTime = a} :: AgentInfo)
{-# DEPRECATED aiRegisteredTime "Use generic-lens or generic-optics with 'registeredTime' instead." #-}

-- | Type of agent.
--
-- /Note:/ Consider using 'agentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiAgentType :: Lens.Lens' AgentInfo (Lude.Maybe Lude.Text)
aiAgentType = Lens.lens (agentType :: AgentInfo -> Lude.Maybe Lude.Text) (\s a -> s {agentType = a} :: AgentInfo)
{-# DEPRECATED aiAgentType "Use generic-lens or generic-optics with 'agentType' instead." #-}

instance Lude.FromJSON AgentInfo where
  parseJSON =
    Lude.withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            Lude.<$> (x Lude..:? "hostName")
            Lude.<*> (x Lude..:? "lastHealthPingTime")
            Lude.<*> (x Lude..:? "agentNetworkInfoList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "connectorId")
            Lude.<*> (x Lude..:? "health")
            Lude.<*> (x Lude..:? "agentId")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "collectionStatus")
            Lude.<*> (x Lude..:? "registeredTime")
            Lude.<*> (x Lude..:? "agentType")
      )
