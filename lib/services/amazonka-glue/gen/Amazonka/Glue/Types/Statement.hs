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
-- Module      : Amazonka.Glue.Types.Statement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Statement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.StatementOutput
import Amazonka.Glue.Types.StatementState
import qualified Amazonka.Prelude as Prelude

-- | The statement or request for a particular action to occur in a session.
--
-- /See:/ 'newStatement' smart constructor.
data Statement = Statement'
  { -- | The code execution progress.
    progress :: Prelude.Maybe Prelude.Double,
    -- | The execution code of the statement.
    code :: Prelude.Maybe Prelude.Text,
    -- | The unix time and date that the job definition was started.
    startedOn :: Prelude.Maybe Prelude.Integer,
    -- | The state while request is actioned.
    state :: Prelude.Maybe StatementState,
    -- | The ID of the statement.
    id :: Prelude.Maybe Prelude.Int,
    -- | The unix time and date that the job definition was completed.
    completedOn :: Prelude.Maybe Prelude.Integer,
    -- | The output in JSON.
    output :: Prelude.Maybe StatementOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progress', 'statement_progress' - The code execution progress.
--
-- 'code', 'statement_code' - The execution code of the statement.
--
-- 'startedOn', 'statement_startedOn' - The unix time and date that the job definition was started.
--
-- 'state', 'statement_state' - The state while request is actioned.
--
-- 'id', 'statement_id' - The ID of the statement.
--
-- 'completedOn', 'statement_completedOn' - The unix time and date that the job definition was completed.
--
-- 'output', 'statement_output' - The output in JSON.
newStatement ::
  Statement
newStatement =
  Statement'
    { progress = Prelude.Nothing,
      code = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      output = Prelude.Nothing
    }

-- | The code execution progress.
statement_progress :: Lens.Lens' Statement (Prelude.Maybe Prelude.Double)
statement_progress = Lens.lens (\Statement' {progress} -> progress) (\s@Statement' {} a -> s {progress = a} :: Statement)

-- | The execution code of the statement.
statement_code :: Lens.Lens' Statement (Prelude.Maybe Prelude.Text)
statement_code = Lens.lens (\Statement' {code} -> code) (\s@Statement' {} a -> s {code = a} :: Statement)

-- | The unix time and date that the job definition was started.
statement_startedOn :: Lens.Lens' Statement (Prelude.Maybe Prelude.Integer)
statement_startedOn = Lens.lens (\Statement' {startedOn} -> startedOn) (\s@Statement' {} a -> s {startedOn = a} :: Statement)

-- | The state while request is actioned.
statement_state :: Lens.Lens' Statement (Prelude.Maybe StatementState)
statement_state = Lens.lens (\Statement' {state} -> state) (\s@Statement' {} a -> s {state = a} :: Statement)

-- | The ID of the statement.
statement_id :: Lens.Lens' Statement (Prelude.Maybe Prelude.Int)
statement_id = Lens.lens (\Statement' {id} -> id) (\s@Statement' {} a -> s {id = a} :: Statement)

-- | The unix time and date that the job definition was completed.
statement_completedOn :: Lens.Lens' Statement (Prelude.Maybe Prelude.Integer)
statement_completedOn = Lens.lens (\Statement' {completedOn} -> completedOn) (\s@Statement' {} a -> s {completedOn = a} :: Statement)

-- | The output in JSON.
statement_output :: Lens.Lens' Statement (Prelude.Maybe StatementOutput)
statement_output = Lens.lens (\Statement' {output} -> output) (\s@Statement' {} a -> s {output = a} :: Statement)

instance Core.FromJSON Statement where
  parseJSON =
    Core.withObject
      "Statement"
      ( \x ->
          Statement'
            Prelude.<$> (x Core..:? "Progress")
            Prelude.<*> (x Core..:? "Code")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "Output")
      )

instance Prelude.Hashable Statement where
  hashWithSalt _salt Statement' {..} =
    _salt `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` output

instance Prelude.NFData Statement where
  rnf Statement' {..} =
    Prelude.rnf progress
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf output
