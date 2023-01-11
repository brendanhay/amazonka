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
-- Module      : Amazonka.Connect.Types.AgentInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AgentInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the agent who accepted the contact.
--
-- /See:/ 'newAgentInfo' smart constructor.
data AgentInfo = AgentInfo'
  { -- | The timestamp when the contact was connected to the agent.
    connectedToAgentTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the agent who accepted the contact.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectedToAgentTimestamp', 'agentInfo_connectedToAgentTimestamp' - The timestamp when the contact was connected to the agent.
--
-- 'id', 'agentInfo_id' - The identifier of the agent who accepted the contact.
newAgentInfo ::
  AgentInfo
newAgentInfo =
  AgentInfo'
    { connectedToAgentTimestamp =
        Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The timestamp when the contact was connected to the agent.
agentInfo_connectedToAgentTimestamp :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.UTCTime)
agentInfo_connectedToAgentTimestamp = Lens.lens (\AgentInfo' {connectedToAgentTimestamp} -> connectedToAgentTimestamp) (\s@AgentInfo' {} a -> s {connectedToAgentTimestamp = a} :: AgentInfo) Prelude.. Lens.mapping Data._Time

-- | The identifier of the agent who accepted the contact.
agentInfo_id :: Lens.Lens' AgentInfo (Prelude.Maybe Prelude.Text)
agentInfo_id = Lens.lens (\AgentInfo' {id} -> id) (\s@AgentInfo' {} a -> s {id = a} :: AgentInfo)

instance Data.FromJSON AgentInfo where
  parseJSON =
    Data.withObject
      "AgentInfo"
      ( \x ->
          AgentInfo'
            Prelude.<$> (x Data..:? "ConnectedToAgentTimestamp")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable AgentInfo where
  hashWithSalt _salt AgentInfo' {..} =
    _salt
      `Prelude.hashWithSalt` connectedToAgentTimestamp
      `Prelude.hashWithSalt` id

instance Prelude.NFData AgentInfo where
  rnf AgentInfo' {..} =
    Prelude.rnf connectedToAgentTimestamp
      `Prelude.seq` Prelude.rnf id
