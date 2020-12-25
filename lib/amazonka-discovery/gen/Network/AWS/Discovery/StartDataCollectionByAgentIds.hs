{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sdcbairfrsAgentsConfigurationStatus,
    sdcbairfrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDataCollectionByAgentIds' smart constructor.
newtype StartDataCollectionByAgentIds = StartDataCollectionByAgentIds'
  { -- | The IDs of the agents or connectors from which to start collecting data. If you send a request to an agent/connector ID that you do not have permission to contact, according to your AWS account, the service does not throw an exception. Instead, it returns the error in the /Description/ field. If you send a request to multiple agents/connectors and you do not have permission to contact some of those agents/connectors, the system does not throw an exception. Instead, the system shows @Failed@ in the /Description/ field.
    agentIds :: [Types.AgentId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartDataCollectionByAgentIds' value with any optional fields omitted.
mkStartDataCollectionByAgentIds ::
  StartDataCollectionByAgentIds
mkStartDataCollectionByAgentIds =
  StartDataCollectionByAgentIds' {agentIds = Core.mempty}

-- | The IDs of the agents or connectors from which to start collecting data. If you send a request to an agent/connector ID that you do not have permission to contact, according to your AWS account, the service does not throw an exception. Instead, it returns the error in the /Description/ field. If you send a request to multiple agents/connectors and you do not have permission to contact some of those agents/connectors, the system does not throw an exception. Instead, the system shows @Failed@ in the /Description/ field.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAgentIds :: Lens.Lens' StartDataCollectionByAgentIds [Types.AgentId]
sAgentIds = Lens.field @"agentIds"
{-# DEPRECATED sAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

instance Core.FromJSON StartDataCollectionByAgentIds where
  toJSON StartDataCollectionByAgentIds {..} =
    Core.object
      (Core.catMaybes [Core.Just ("agentIds" Core..= agentIds)])

instance Core.AWSRequest StartDataCollectionByAgentIds where
  type
    Rs StartDataCollectionByAgentIds =
      StartDataCollectionByAgentIdsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.StartDataCollectionByAgentIds"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataCollectionByAgentIdsResponse'
            Core.<$> (x Core..:? "agentsConfigurationStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartDataCollectionByAgentIdsResponse' smart constructor.
data StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse'
  { -- | Information about agents or the connector that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
    agentsConfigurationStatus :: Core.Maybe [Types.AgentConfigurationStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartDataCollectionByAgentIdsResponse' value with any optional fields omitted.
mkStartDataCollectionByAgentIdsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartDataCollectionByAgentIdsResponse
mkStartDataCollectionByAgentIdsResponse responseStatus =
  StartDataCollectionByAgentIdsResponse'
    { agentsConfigurationStatus =
        Core.Nothing,
      responseStatus
    }

-- | Information about agents or the connector that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
--
-- /Note:/ Consider using 'agentsConfigurationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbairfrsAgentsConfigurationStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse (Core.Maybe [Types.AgentConfigurationStatus])
sdcbairfrsAgentsConfigurationStatus = Lens.field @"agentsConfigurationStatus"
{-# DEPRECATED sdcbairfrsAgentsConfigurationStatus "Use generic-lens or generic-optics with 'agentsConfigurationStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcbairfrsResponseStatus :: Lens.Lens' StartDataCollectionByAgentIdsResponse Core.Int
sdcbairfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sdcbairfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
