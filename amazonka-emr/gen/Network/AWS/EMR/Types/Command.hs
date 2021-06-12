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
-- Module      : Network.AWS.EMR.Types.Command
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Command where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'newCommand' smart constructor.
data Command = Command'
  { -- | Arguments for Amazon EMR to pass to the command for execution.
    args :: Core.Maybe [Core.Text],
    -- | The Amazon S3 location of the command script.
    scriptPath :: Core.Maybe Core.Text,
    -- | The name of the command.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'scriptPath', 'command_scriptPath' - The Amazon S3 location of the command script.
--
-- 'name', 'command_name' - The name of the command.
newCommand ::
  Command
newCommand =
  Command'
    { args = Core.Nothing,
      scriptPath = Core.Nothing,
      name = Core.Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
command_args :: Lens.Lens' Command (Core.Maybe [Core.Text])
command_args = Lens.lens (\Command' {args} -> args) (\s@Command' {} a -> s {args = a} :: Command) Core.. Lens.mapping Lens._Coerce

-- | The Amazon S3 location of the command script.
command_scriptPath :: Lens.Lens' Command (Core.Maybe Core.Text)
command_scriptPath = Lens.lens (\Command' {scriptPath} -> scriptPath) (\s@Command' {} a -> s {scriptPath = a} :: Command)

-- | The name of the command.
command_name :: Lens.Lens' Command (Core.Maybe Core.Text)
command_name = Lens.lens (\Command' {name} -> name) (\s@Command' {} a -> s {name = a} :: Command)

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject
      "Command"
      ( \x ->
          Command'
            Core.<$> (x Core..:? "Args" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ScriptPath")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable Command

instance Core.NFData Command
