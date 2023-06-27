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
-- Module      : Amazonka.LexV2Models.Types.TurnSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TurnSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AgentTurnSpecification
import Amazonka.LexV2Models.Types.UserTurnSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the messages in the turn.
--
-- /See:/ 'newTurnSpecification' smart constructor.
data TurnSpecification = TurnSpecification'
  { -- | Contains information about the agent messages in the turn.
    agentTurn :: Prelude.Maybe AgentTurnSpecification,
    -- | Contains information about the user messages in the turn.
    userTurn :: Prelude.Maybe UserTurnSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TurnSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentTurn', 'turnSpecification_agentTurn' - Contains information about the agent messages in the turn.
--
-- 'userTurn', 'turnSpecification_userTurn' - Contains information about the user messages in the turn.
newTurnSpecification ::
  TurnSpecification
newTurnSpecification =
  TurnSpecification'
    { agentTurn = Prelude.Nothing,
      userTurn = Prelude.Nothing
    }

-- | Contains information about the agent messages in the turn.
turnSpecification_agentTurn :: Lens.Lens' TurnSpecification (Prelude.Maybe AgentTurnSpecification)
turnSpecification_agentTurn = Lens.lens (\TurnSpecification' {agentTurn} -> agentTurn) (\s@TurnSpecification' {} a -> s {agentTurn = a} :: TurnSpecification)

-- | Contains information about the user messages in the turn.
turnSpecification_userTurn :: Lens.Lens' TurnSpecification (Prelude.Maybe UserTurnSpecification)
turnSpecification_userTurn = Lens.lens (\TurnSpecification' {userTurn} -> userTurn) (\s@TurnSpecification' {} a -> s {userTurn = a} :: TurnSpecification)

instance Data.FromJSON TurnSpecification where
  parseJSON =
    Data.withObject
      "TurnSpecification"
      ( \x ->
          TurnSpecification'
            Prelude.<$> (x Data..:? "agentTurn")
            Prelude.<*> (x Data..:? "userTurn")
      )

instance Prelude.Hashable TurnSpecification where
  hashWithSalt _salt TurnSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` agentTurn
      `Prelude.hashWithSalt` userTurn

instance Prelude.NFData TurnSpecification where
  rnf TurnSpecification' {..} =
    Prelude.rnf agentTurn
      `Prelude.seq` Prelude.rnf userTurn
