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
-- Module      : Amazonka.LexV2Models.Types.TestSetTurnResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetTurnResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AgentTurnResult
import Amazonka.LexV2Models.Types.UserTurnResult
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the results of the analysis of a turn in the
-- test set.
--
-- /See:/ 'newTestSetTurnResult' smart constructor.
data TestSetTurnResult = TestSetTurnResult'
  { -- | Contains information about the agent messages in the turn.
    agent :: Prelude.Maybe AgentTurnResult,
    -- | Contains information about the user messages in the turn.
    user :: Prelude.Maybe UserTurnResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetTurnResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agent', 'testSetTurnResult_agent' - Contains information about the agent messages in the turn.
--
-- 'user', 'testSetTurnResult_user' - Contains information about the user messages in the turn.
newTestSetTurnResult ::
  TestSetTurnResult
newTestSetTurnResult =
  TestSetTurnResult'
    { agent = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | Contains information about the agent messages in the turn.
testSetTurnResult_agent :: Lens.Lens' TestSetTurnResult (Prelude.Maybe AgentTurnResult)
testSetTurnResult_agent = Lens.lens (\TestSetTurnResult' {agent} -> agent) (\s@TestSetTurnResult' {} a -> s {agent = a} :: TestSetTurnResult)

-- | Contains information about the user messages in the turn.
testSetTurnResult_user :: Lens.Lens' TestSetTurnResult (Prelude.Maybe UserTurnResult)
testSetTurnResult_user = Lens.lens (\TestSetTurnResult' {user} -> user) (\s@TestSetTurnResult' {} a -> s {user = a} :: TestSetTurnResult)

instance Data.FromJSON TestSetTurnResult where
  parseJSON =
    Data.withObject
      "TestSetTurnResult"
      ( \x ->
          TestSetTurnResult'
            Prelude.<$> (x Data..:? "agent")
            Prelude.<*> (x Data..:? "user")
      )

instance Prelude.Hashable TestSetTurnResult where
  hashWithSalt _salt TestSetTurnResult' {..} =
    _salt
      `Prelude.hashWithSalt` agent
      `Prelude.hashWithSalt` user

instance Prelude.NFData TestSetTurnResult where
  rnf TestSetTurnResult' {..} =
    Prelude.rnf agent `Prelude.seq` Prelude.rnf user
