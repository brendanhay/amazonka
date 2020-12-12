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
    acsOperationSucceeded,
    acsDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.
--
-- /See:/ 'mkAgentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { agentId ::
      Lude.Maybe Lude.Text,
    operationSucceeded ::
      Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AgentConfigurationStatus' with the minimum fields required to make a request.
--
-- * 'agentId' - The agent/connector ID.
-- * 'description' - A description of the operation performed.
-- * 'operationSucceeded' - Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
mkAgentConfigurationStatus ::
  AgentConfigurationStatus
mkAgentConfigurationStatus =
  AgentConfigurationStatus'
    { agentId = Lude.Nothing,
      operationSucceeded = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The agent/connector ID.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAgentId :: Lens.Lens' AgentConfigurationStatus (Lude.Maybe Lude.Text)
acsAgentId = Lens.lens (agentId :: AgentConfigurationStatus -> Lude.Maybe Lude.Text) (\s a -> s {agentId = a} :: AgentConfigurationStatus)
{-# DEPRECATED acsAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

-- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
--
-- /Note:/ Consider using 'operationSucceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsOperationSucceeded :: Lens.Lens' AgentConfigurationStatus (Lude.Maybe Lude.Bool)
acsOperationSucceeded = Lens.lens (operationSucceeded :: AgentConfigurationStatus -> Lude.Maybe Lude.Bool) (\s a -> s {operationSucceeded = a} :: AgentConfigurationStatus)
{-# DEPRECATED acsOperationSucceeded "Use generic-lens or generic-optics with 'operationSucceeded' instead." #-}

-- | A description of the operation performed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsDescription :: Lens.Lens' AgentConfigurationStatus (Lude.Maybe Lude.Text)
acsDescription = Lens.lens (description :: AgentConfigurationStatus -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AgentConfigurationStatus)
{-# DEPRECATED acsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON AgentConfigurationStatus where
  parseJSON =
    Lude.withObject
      "AgentConfigurationStatus"
      ( \x ->
          AgentConfigurationStatus'
            Lude.<$> (x Lude..:? "agentId")
            Lude.<*> (x Lude..:? "operationSucceeded")
            Lude.<*> (x Lude..:? "description")
      )
