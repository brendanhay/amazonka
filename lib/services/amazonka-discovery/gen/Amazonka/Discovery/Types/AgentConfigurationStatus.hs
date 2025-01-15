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
-- Module      : Amazonka.Discovery.Types.AgentConfigurationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.AgentConfigurationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about agents or connectors that were instructed to start
-- collecting data. Information includes the agent\/connector ID, a
-- description of the operation, and whether the agent\/connector
-- configuration was updated.
--
-- /See:/ 'newAgentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { -- | The agent\/connector ID.
    agentId :: Prelude.Maybe Prelude.Text,
    -- | A description of the operation performed.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the @StartDataCollection@ and
    -- @StopDataCollection@ operations. The system has recorded the data
    -- collection operation. The agent\/connector receives this command the
    -- next time it polls for a new command.
    operationSucceeded :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'agentConfigurationStatus_description' - A description of the operation performed.
--
-- 'operationSucceeded', 'agentConfigurationStatus_operationSucceeded' - Information about the status of the @StartDataCollection@ and
-- @StopDataCollection@ operations. The system has recorded the data
-- collection operation. The agent\/connector receives this command the
-- next time it polls for a new command.
newAgentConfigurationStatus ::
  AgentConfigurationStatus
newAgentConfigurationStatus =
  AgentConfigurationStatus'
    { agentId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      operationSucceeded = Prelude.Nothing
    }

-- | The agent\/connector ID.
agentConfigurationStatus_agentId :: Lens.Lens' AgentConfigurationStatus (Prelude.Maybe Prelude.Text)
agentConfigurationStatus_agentId = Lens.lens (\AgentConfigurationStatus' {agentId} -> agentId) (\s@AgentConfigurationStatus' {} a -> s {agentId = a} :: AgentConfigurationStatus)

-- | A description of the operation performed.
agentConfigurationStatus_description :: Lens.Lens' AgentConfigurationStatus (Prelude.Maybe Prelude.Text)
agentConfigurationStatus_description = Lens.lens (\AgentConfigurationStatus' {description} -> description) (\s@AgentConfigurationStatus' {} a -> s {description = a} :: AgentConfigurationStatus)

-- | Information about the status of the @StartDataCollection@ and
-- @StopDataCollection@ operations. The system has recorded the data
-- collection operation. The agent\/connector receives this command the
-- next time it polls for a new command.
agentConfigurationStatus_operationSucceeded :: Lens.Lens' AgentConfigurationStatus (Prelude.Maybe Prelude.Bool)
agentConfigurationStatus_operationSucceeded = Lens.lens (\AgentConfigurationStatus' {operationSucceeded} -> operationSucceeded) (\s@AgentConfigurationStatus' {} a -> s {operationSucceeded = a} :: AgentConfigurationStatus)

instance Data.FromJSON AgentConfigurationStatus where
  parseJSON =
    Data.withObject
      "AgentConfigurationStatus"
      ( \x ->
          AgentConfigurationStatus'
            Prelude.<$> (x Data..:? "agentId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "operationSucceeded")
      )

instance Prelude.Hashable AgentConfigurationStatus where
  hashWithSalt _salt AgentConfigurationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` agentId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` operationSucceeded

instance Prelude.NFData AgentConfigurationStatus where
  rnf AgentConfigurationStatus' {..} =
    Prelude.rnf agentId `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf operationSucceeded
