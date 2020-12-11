{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartDataCollectionByAgentIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to start collecting data.
module Network.AWS.Discovery.StartDataCollectionByAgentIds
  ( -- * Creating a request
    StartDataCollectionByAgentIds (..),
    mkStartDataCollectionByAgentIds,

    -- ** Request lenses
    sAgentIds,

    -- * Destructuring the response
    StartDataCollectionByAgentIdsResponse (..),
    mkStartDataCollectionByAgentIdsResponse,

    -- ** Response lenses
    sdcbaisrsAgentsConfigurationStatus,
    sdcbaisrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDataCollectionByAgentIds' smart constructor.
newtype StartDataCollectionByAgentIds = StartDataCollectionByAgentIds'
  { agentIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDataCollectionByAgentIds' with the minimum fields required to make a request.
--
-- * 'agentIds' - The IDs of the agents or connectors from which to start collecting data. If you send a request to an agent/connector ID that you do not have permission to contact, according to your AWS account, the service does not throw an exception. Instead, it returns the error in the /Description/ field. If you send a request to multiple agents/connectors and you do not have permission to contact some of those agents/connectors, the system does not throw an exception. Instead, the system shows @Failed@ in the /Description/ field.
mkStartDataCollectionByAgentIds ::
  StartDataCollectionByAgentIds
mkStartDataCollectionByAgentIds =
  StartDataCollectionByAgentIds' {agentIds = Lude.mempty}

-- | The IDs of the agents or connectors from which to start collecting data. If you send a request to an agent/connector ID that you do not have permission to contact, according to your AWS account, the service does not throw an exception. Instead, it returns the error in the /Description/ field. If you send a request to multiple agents/connectors and you do not have permission to contact some of those agents/connectors, the system does not throw an exception. Instead, the system shows @Failed@ in the /Description/ field.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentIds :: Lens.Lens' StartDataCollectionByAgentIds [Lude.Text]
sAgentIds = Lens.lens (agentIds :: StartDataCollectionByAgentIds -> [Lude.Text]) (\s a -> s {agentIds = a} :: StartDataCollectionByAgentIds)
{-# DEPRECATED sAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

instance Lude.AWSRequest StartDataCollectionByAgentIds where
  type
    Rs StartDataCollectionByAgentIds =
      StartDataCollectionByAgentIdsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartDataCollectionByAgentIdsResponse'
            Lude.<$> (x Lude..?> "agentsConfigurationStatus" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDataCollectionByAgentIds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StartDataCollectionByAgentIds" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartDataCollectionByAgentIds where
  toJSON StartDataCollectionByAgentIds' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("agentIds" Lude..= agentIds)])

instance Lude.ToPath StartDataCollectionByAgentIds where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDataCollectionByAgentIds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartDataCollectionByAgentIdsResponse' smart constructor.
data StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse'
  { agentsConfigurationStatus ::
      Lude.Maybe
        [AgentConfigurationStatus],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDataCollectionByAgentIdsResponse' with the minimum fields required to make a request.
--
-- * 'agentsConfigurationStatus' - Information about agents or the connector that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
-- * 'responseStatus' - The response status code.
mkStartDataCollectionByAgentIdsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDataCollectionByAgentIdsResponse
mkStartDataCollectionByAgentIdsResponse pResponseStatus_ =
  StartDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about agents or the connector that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
--
-- /Note:/ Consider using 'agentsConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbaisrsAgentsConfigurationStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse (Lude.Maybe [AgentConfigurationStatus])
sdcbaisrsAgentsConfigurationStatus = Lens.lens (agentsConfigurationStatus :: StartDataCollectionByAgentIdsResponse -> Lude.Maybe [AgentConfigurationStatus]) (\s a -> s {agentsConfigurationStatus = a} :: StartDataCollectionByAgentIdsResponse)
{-# DEPRECATED sdcbaisrsAgentsConfigurationStatus "Use generic-lens or generic-optics with 'agentsConfigurationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbaisrsResponseStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse Lude.Int
sdcbaisrsResponseStatus = Lens.lens (responseStatus :: StartDataCollectionByAgentIdsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDataCollectionByAgentIdsResponse)
{-# DEPRECATED sdcbaisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
