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
-- Module      : Amazonka.EMR.Types.Command
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Command where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | Arguments for Amazon EMR to pass to the command for execution.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The name of the command.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the command script.
    scriptPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Command' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'command_args' - Arguments for Amazon EMR to pass to the command for execution.
--
-- 'name', 'command_name' - The name of the command.
--
-- 'scriptPath', 'command_scriptPath' - The Amazon S3 location of the command script.
newCommand ::
  Command
newCommand =
  Command'
    { args = Prelude.Nothing,
      name = Prelude.Nothing,
      scriptPath = Prelude.Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
command_args :: Lens.Lens' Command (Prelude.Maybe [Prelude.Text])
command_args = Lens.lens (\Command' {args} -> args) (\s@Command' {} a -> s {args = a} :: Command) Prelude.. Lens.mapping Lens.coerced

-- | The name of the command.
command_name :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_name = Lens.lens (\Command' {name} -> name) (\s@Command' {} a -> s {name = a} :: Command)

-- | The Amazon S3 location of the command script.
command_scriptPath :: Lens.Lens' Command (Prelude.Maybe Prelude.Text)
command_scriptPath = Lens.lens (\Command' {scriptPath} -> scriptPath) (\s@Command' {} a -> s {scriptPath = a} :: Command)

instance Data.FromJSON Command where
  parseJSON =
    Data.withObject
      "Command"
      ( \x ->
          Command'
            Prelude.<$> (x Data..:? "Args" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ScriptPath")
      )

instance Prelude.Hashable Command where
  hashWithSalt _salt Command' {..} =
    _salt
      `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scriptPath

instance Prelude.NFData Command where
  rnf Command' {..} =
    Prelude.rnf args `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf scriptPath
