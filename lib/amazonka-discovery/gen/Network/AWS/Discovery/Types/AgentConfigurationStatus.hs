{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentConfigurationStatus
  ( AgentConfigurationStatus (..),

    -- * Smart constructor
    mkAgentConfigurationStatus,

    -- * Lenses
    acsAgentId,
    acsDescription,
    acsOperationSucceeded,
  )
where

import qualified Network.AWS.Discovery.Types.AgentId as Types
import qualified Network.AWS.Discovery.Types.Description as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.
--
-- /See:/ 'mkAgentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { -- | The agent/connector ID.
    agentId :: Core.Maybe Types.AgentId,
    -- | A description of the operation performed.
    description :: Core.Maybe Types.Description,
    -- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
    operationSucceeded :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AgentConfigurationStatus' value with any optional fields omitted.
mkAgentConfigurationStatus ::
  AgentConfigurationStatus
mkAgentConfigurationStatus =
  AgentConfigurationStatus'
    { agentId = Core.Nothing,
      description = Core.Nothing,
      operationSucceeded = Core.Nothing
    }

-- | The agent/connector ID.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAgentId :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Types.AgentId)
acsAgentId = Lens.field @"agentId"
{-# DEPRECATED acsAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

-- | A description of the operation performed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsDescription :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Types.Description)
acsDescription = Lens.field @"description"
{-# DEPRECATED acsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
--
-- /Note:/ Consider using 'operationSucceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsOperationSucceeded :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Core.Bool)
acsOperationSucceeded = Lens.field @"operationSucceeded"
{-# DEPRECATED acsOperationSucceeded "Use generic-lens or generic-optics with 'operationSucceeded' instead." #-}

instance Core.FromJSON AgentConfigurationStatus where
  parseJSON =
    Core.withObject "AgentConfigurationStatus" Core.$
      \x ->
        AgentConfigurationStatus'
          Core.<$> (x Core..:? "agentId")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "operationSucceeded")
