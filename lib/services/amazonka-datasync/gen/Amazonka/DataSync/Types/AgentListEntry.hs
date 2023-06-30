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
-- Module      : Amazonka.DataSync.Types.AgentListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.AgentListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.AgentStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in a list of agents. @AgentListEntry@ returns
-- an array that contains a list of agents when the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListAgents.html ListAgents>
-- operation is called.
--
-- /See:/ 'newAgentListEntry' smart constructor.
data AgentListEntry = AgentListEntry'
  { -- | The Amazon Resource Name (ARN) of the agent.
    agentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the agent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the agent.
    status :: Prelude.Maybe AgentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArn', 'agentListEntry_agentArn' - The Amazon Resource Name (ARN) of the agent.
--
-- 'name', 'agentListEntry_name' - The name of the agent.
--
-- 'status', 'agentListEntry_status' - The status of the agent.
newAgentListEntry ::
  AgentListEntry
newAgentListEntry =
  AgentListEntry'
    { agentArn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the agent.
agentListEntry_agentArn :: Lens.Lens' AgentListEntry (Prelude.Maybe Prelude.Text)
agentListEntry_agentArn = Lens.lens (\AgentListEntry' {agentArn} -> agentArn) (\s@AgentListEntry' {} a -> s {agentArn = a} :: AgentListEntry)

-- | The name of the agent.
agentListEntry_name :: Lens.Lens' AgentListEntry (Prelude.Maybe Prelude.Text)
agentListEntry_name = Lens.lens (\AgentListEntry' {name} -> name) (\s@AgentListEntry' {} a -> s {name = a} :: AgentListEntry)

-- | The status of the agent.
agentListEntry_status :: Lens.Lens' AgentListEntry (Prelude.Maybe AgentStatus)
agentListEntry_status = Lens.lens (\AgentListEntry' {status} -> status) (\s@AgentListEntry' {} a -> s {status = a} :: AgentListEntry)

instance Data.FromJSON AgentListEntry where
  parseJSON =
    Data.withObject
      "AgentListEntry"
      ( \x ->
          AgentListEntry'
            Prelude.<$> (x Data..:? "AgentArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AgentListEntry where
  hashWithSalt _salt AgentListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` agentArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData AgentListEntry where
  rnf AgentListEntry' {..} =
    Prelude.rnf agentArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
