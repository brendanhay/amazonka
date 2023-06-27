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
-- Module      : Amazonka.LexV2Models.Types.TestExecutionTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestExecutionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.BotAliasTestExecutionTarget
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the bot used for the test execution.
--
-- /See:/ 'newTestExecutionTarget' smart constructor.
data TestExecutionTarget = TestExecutionTarget'
  { -- | Contains information about the bot alias used for the test execution.
    botAliasTarget :: Prelude.Maybe BotAliasTestExecutionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestExecutionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasTarget', 'testExecutionTarget_botAliasTarget' - Contains information about the bot alias used for the test execution.
newTestExecutionTarget ::
  TestExecutionTarget
newTestExecutionTarget =
  TestExecutionTarget'
    { botAliasTarget =
        Prelude.Nothing
    }

-- | Contains information about the bot alias used for the test execution.
testExecutionTarget_botAliasTarget :: Lens.Lens' TestExecutionTarget (Prelude.Maybe BotAliasTestExecutionTarget)
testExecutionTarget_botAliasTarget = Lens.lens (\TestExecutionTarget' {botAliasTarget} -> botAliasTarget) (\s@TestExecutionTarget' {} a -> s {botAliasTarget = a} :: TestExecutionTarget)

instance Data.FromJSON TestExecutionTarget where
  parseJSON =
    Data.withObject
      "TestExecutionTarget"
      ( \x ->
          TestExecutionTarget'
            Prelude.<$> (x Data..:? "botAliasTarget")
      )

instance Prelude.Hashable TestExecutionTarget where
  hashWithSalt _salt TestExecutionTarget' {..} =
    _salt `Prelude.hashWithSalt` botAliasTarget

instance Prelude.NFData TestExecutionTarget where
  rnf TestExecutionTarget' {..} =
    Prelude.rnf botAliasTarget

instance Data.ToJSON TestExecutionTarget where
  toJSON TestExecutionTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botAliasTarget" Data..=)
              Prelude.<$> botAliasTarget
          ]
      )
