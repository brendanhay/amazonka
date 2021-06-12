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
-- Module      : Network.AWS.Discovery.Types.AgentConfigurationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentConfigurationStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about agents or connectors that were instructed to start
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation, and whether the agent\/connector
-- configuration was updated.
--
-- /See:/ 'newAgentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { -- | The agent\/connector ID.
    agentId :: Core.Maybe Core.Text,
    -- | Information about the status of the @StartDataCollection@ and
    -- @StopDataCollection@ operations. The system has recorded the data
    -- collection operation. The agent\/connector receives this command the
    -- next time it polls for a new command.
    operationSucceeded :: Core.Maybe Core.Bool,
    -- | A description of the operation performed.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AgentConfigurationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentId', 'agentConfigurationStatus_agentId' - The agent\/connector ID.
--
-- 'operationSucceeded', 'agentConfigurationStatus_operationSucceeded' - Information about the status of the @StartDataCollection@ and
-- @StopDataCollection@ operations. The system has recorded the data
-- collection operation. The agent\/connector receives this command the
-- next time it polls for a new command.
--
-- 'description', 'agentConfigurationStatus_description' - A description of the operation performed.
newAgentConfigurationStatus ::
  AgentConfigurationStatus
newAgentConfigurationStatus =
  AgentConfigurationStatus'
    { agentId = Core.Nothing,
      operationSucceeded = Core.Nothing,
      description = Core.Nothing
    }

-- | The agent\/connector ID.
agentConfigurationStatus_agentId :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Core.Text)
agentConfigurationStatus_agentId = Lens.lens (\AgentConfigurationStatus' {agentId} -> agentId) (\s@AgentConfigurationStatus' {} a -> s {agentId = a} :: AgentConfigurationStatus)

-- | Information about the status of the @StartDataCollection@ and
-- @StopDataCollection@ operations. The system has recorded the data
-- collection operation. The agent\/connector receives this command the
-- next time it polls for a new command.
agentConfigurationStatus_operationSucceeded :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Core.Bool)
agentConfigurationStatus_operationSucceeded = Lens.lens (\AgentConfigurationStatus' {operationSucceeded} -> operationSucceeded) (\s@AgentConfigurationStatus' {} a -> s {operationSucceeded = a} :: AgentConfigurationStatus)

-- | A description of the operation performed.
agentConfigurationStatus_description :: Lens.Lens' AgentConfigurationStatus (Core.Maybe Core.Text)
agentConfigurationStatus_description = Lens.lens (\AgentConfigurationStatus' {description} -> description) (\s@AgentConfigurationStatus' {} a -> s {description = a} :: AgentConfigurationStatus)

instance Core.FromJSON AgentConfigurationStatus where
  parseJSON =
    Core.withObject
      "AgentConfigurationStatus"
      ( \x ->
          AgentConfigurationStatus'
            Core.<$> (x Core..:? "agentId")
            Core.<*> (x Core..:? "operationSucceeded")
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable AgentConfigurationStatus

instance Core.NFData AgentConfigurationStatus
