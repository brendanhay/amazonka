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
-- Module      : Amazonka.Connect.Types.AgentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AgentStatus where

import Amazonka.Connect.Types.AgentStatusState
import Amazonka.Connect.Types.AgentStatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an agent status.
--
-- /See:/ 'newAgentStatus' smart constructor.
data AgentStatus = AgentStatus'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the agent status.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of agent status.
    type' :: Prelude.Maybe AgentStatusType,
    -- | The display order of the agent status.
    displayOrder :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the agent status.
    agentStatusId :: Prelude.Maybe Prelude.Text,
    -- | The state of the agent status.
    state :: Prelude.Maybe AgentStatusState,
    -- | The description of the agent status.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the agent status.
    agentStatusARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'agentStatus_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'name', 'agentStatus_name' - The name of the agent status.
--
-- 'type'', 'agentStatus_type' - The type of agent status.
--
-- 'displayOrder', 'agentStatus_displayOrder' - The display order of the agent status.
--
-- 'agentStatusId', 'agentStatus_agentStatusId' - The identifier of the agent status.
--
-- 'state', 'agentStatus_state' - The state of the agent status.
--
-- 'description', 'agentStatus_description' - The description of the agent status.
--
-- 'agentStatusARN', 'agentStatus_agentStatusARN' - The Amazon Resource Name (ARN) of the agent status.
newAgentStatus ::
  AgentStatus
newAgentStatus =
  AgentStatus'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      displayOrder = Prelude.Nothing,
      agentStatusId = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      agentStatusARN = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
agentStatus_tags :: Lens.Lens' AgentStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
agentStatus_tags = Lens.lens (\AgentStatus' {tags} -> tags) (\s@AgentStatus' {} a -> s {tags = a} :: AgentStatus) Prelude.. Lens.mapping Lens.coerced

-- | The name of the agent status.
agentStatus_name :: Lens.Lens' AgentStatus (Prelude.Maybe Prelude.Text)
agentStatus_name = Lens.lens (\AgentStatus' {name} -> name) (\s@AgentStatus' {} a -> s {name = a} :: AgentStatus)

-- | The type of agent status.
agentStatus_type :: Lens.Lens' AgentStatus (Prelude.Maybe AgentStatusType)
agentStatus_type = Lens.lens (\AgentStatus' {type'} -> type') (\s@AgentStatus' {} a -> s {type' = a} :: AgentStatus)

-- | The display order of the agent status.
agentStatus_displayOrder :: Lens.Lens' AgentStatus (Prelude.Maybe Prelude.Natural)
agentStatus_displayOrder = Lens.lens (\AgentStatus' {displayOrder} -> displayOrder) (\s@AgentStatus' {} a -> s {displayOrder = a} :: AgentStatus)

-- | The identifier of the agent status.
agentStatus_agentStatusId :: Lens.Lens' AgentStatus (Prelude.Maybe Prelude.Text)
agentStatus_agentStatusId = Lens.lens (\AgentStatus' {agentStatusId} -> agentStatusId) (\s@AgentStatus' {} a -> s {agentStatusId = a} :: AgentStatus)

-- | The state of the agent status.
agentStatus_state :: Lens.Lens' AgentStatus (Prelude.Maybe AgentStatusState)
agentStatus_state = Lens.lens (\AgentStatus' {state} -> state) (\s@AgentStatus' {} a -> s {state = a} :: AgentStatus)

-- | The description of the agent status.
agentStatus_description :: Lens.Lens' AgentStatus (Prelude.Maybe Prelude.Text)
agentStatus_description = Lens.lens (\AgentStatus' {description} -> description) (\s@AgentStatus' {} a -> s {description = a} :: AgentStatus)

-- | The Amazon Resource Name (ARN) of the agent status.
agentStatus_agentStatusARN :: Lens.Lens' AgentStatus (Prelude.Maybe Prelude.Text)
agentStatus_agentStatusARN = Lens.lens (\AgentStatus' {agentStatusARN} -> agentStatusARN) (\s@AgentStatus' {} a -> s {agentStatusARN = a} :: AgentStatus)

instance Data.FromJSON AgentStatus where
  parseJSON =
    Data.withObject
      "AgentStatus"
      ( \x ->
          AgentStatus'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "DisplayOrder")
            Prelude.<*> (x Data..:? "AgentStatusId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "AgentStatusARN")
      )

instance Prelude.Hashable AgentStatus where
  hashWithSalt _salt AgentStatus' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` displayOrder
      `Prelude.hashWithSalt` agentStatusId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` agentStatusARN

instance Prelude.NFData AgentStatus where
  rnf AgentStatus' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf displayOrder
      `Prelude.seq` Prelude.rnf agentStatusId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf agentStatusARN
