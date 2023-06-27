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

-- | Represents a single entry in a list (or array) of DataSync agents when
-- you call the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListAgents.html ListAgents>
-- operation.
--
-- /See:/ 'newAgentListEntry' smart constructor.
data AgentListEntry = AgentListEntry'
  { -- | The Amazon Resource Name (ARN) of a DataSync agent.
    agentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of an agent.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of an agent. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/understand-agent-statuses.html DataSync agent statuses>.
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
-- 'agentArn', 'agentListEntry_agentArn' - The Amazon Resource Name (ARN) of a DataSync agent.
--
-- 'name', 'agentListEntry_name' - The name of an agent.
--
-- 'status', 'agentListEntry_status' - The status of an agent. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/understand-agent-statuses.html DataSync agent statuses>.
newAgentListEntry ::
  AgentListEntry
newAgentListEntry =
  AgentListEntry'
    { agentArn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a DataSync agent.
agentListEntry_agentArn :: Lens.Lens' AgentListEntry (Prelude.Maybe Prelude.Text)
agentListEntry_agentArn = Lens.lens (\AgentListEntry' {agentArn} -> agentArn) (\s@AgentListEntry' {} a -> s {agentArn = a} :: AgentListEntry)

-- | The name of an agent.
agentListEntry_name :: Lens.Lens' AgentListEntry (Prelude.Maybe Prelude.Text)
agentListEntry_name = Lens.lens (\AgentListEntry' {name} -> name) (\s@AgentListEntry' {} a -> s {name = a} :: AgentListEntry)

-- | The status of an agent. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/understand-agent-statuses.html DataSync agent statuses>.
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
