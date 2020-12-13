{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StopDataCollectionByAgentIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to stop collecting data.
module Network.AWS.Discovery.StopDataCollectionByAgentIds
  ( -- * Creating a request
    StopDataCollectionByAgentIds (..),
    mkStopDataCollectionByAgentIds,

    -- ** Request lenses
    sAgentIds,

    -- * Destructuring the response
    StopDataCollectionByAgentIdsResponse (..),
    mkStopDataCollectionByAgentIdsResponse,

    -- ** Response lenses
    sdcbairsAgentsConfigurationStatus,
    sdcbairsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopDataCollectionByAgentIds' smart constructor.
newtype StopDataCollectionByAgentIds = StopDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to stop collecting data.
    agentIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDataCollectionByAgentIds' with the minimum fields required to make a request.
--
-- * 'agentIds' - The IDs of the agents or connectors from which to stop collecting data.
mkStopDataCollectionByAgentIds ::
  StopDataCollectionByAgentIds
mkStopDataCollectionByAgentIds =
  StopDataCollectionByAgentIds' {agentIds = Lude.mempty}

-- | The IDs of the agents or connectors from which to stop collecting data.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentIds :: Lens.Lens' StopDataCollectionByAgentIds [Lude.Text]
sAgentIds = Lens.lens (agentIds :: StopDataCollectionByAgentIds -> [Lude.Text]) (\s a -> s {agentIds = a} :: StopDataCollectionByAgentIds)
{-# DEPRECATED sAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

instance Lude.AWSRequest StopDataCollectionByAgentIds where
  type
    Rs StopDataCollectionByAgentIds =
      StopDataCollectionByAgentIdsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopDataCollectionByAgentIdsResponse'
            Lude.<$> (x Lude..?> "agentsConfigurationStatus" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDataCollectionByAgentIds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StopDataCollectionByAgentIds" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopDataCollectionByAgentIds where
  toJSON StopDataCollectionByAgentIds' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("agentIds" Lude..= agentIds)])

instance Lude.ToPath StopDataCollectionByAgentIds where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDataCollectionByAgentIds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopDataCollectionByAgentIdsResponse' smart constructor.
data StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse'
  { -- | Information about the agents or connector that were instructed to stop collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
    agentsConfigurationStatus :: Lude.Maybe [AgentConfigurationStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDataCollectionByAgentIdsResponse' with the minimum fields required to make a request.
--
-- * 'agentsConfigurationStatus' - Information about the agents or connector that were instructed to stop collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
-- * 'responseStatus' - The response status code.
mkStopDataCollectionByAgentIdsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDataCollectionByAgentIdsResponse
mkStopDataCollectionByAgentIdsResponse pResponseStatus_ =
  StopDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the agents or connector that were instructed to stop collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
--
-- /Note:/ Consider using 'agentsConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbairsAgentsConfigurationStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse (Lude.Maybe [AgentConfigurationStatus])
sdcbairsAgentsConfigurationStatus = Lens.lens (agentsConfigurationStatus :: StopDataCollectionByAgentIdsResponse -> Lude.Maybe [AgentConfigurationStatus]) (\s a -> s {agentsConfigurationStatus = a} :: StopDataCollectionByAgentIdsResponse)
{-# DEPRECATED sdcbairsAgentsConfigurationStatus "Use generic-lens or generic-optics with 'agentsConfigurationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbairsResponseStatus :: Lens.Lens' StopDataCollectionByAgentIdsResponse Lude.Int
sdcbairsResponseStatus = Lens.lens (responseStatus :: StopDataCollectionByAgentIdsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDataCollectionByAgentIdsResponse)
{-# DEPRECATED sdcbairsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
