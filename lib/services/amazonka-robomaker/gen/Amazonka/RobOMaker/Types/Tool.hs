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
-- Module      : Amazonka.RobOMaker.Types.Tool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.Tool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.ExitBehavior

-- | Information about a tool. Tools are used in a simulation job.
--
-- /See:/ 'newTool' smart constructor.
data Tool = Tool'
  { -- | Boolean indicating whether logs will be recorded in CloudWatch for the
    -- tool. The default is @False@.
    streamOutputToCloudWatch :: Prelude.Maybe Prelude.Bool,
    -- | Boolean indicating whether a streaming session will be configured for
    -- the tool. If @True@, AWS RoboMaker will configure a connection so you
    -- can interact with the tool as it is running in the simulation. It must
    -- have a graphical user interface. The default is @False@.
    streamUI :: Prelude.Maybe Prelude.Bool,
    -- | Exit behavior determines what happens when your tool quits running.
    -- @RESTART@ will cause your tool to be restarted. @FAIL@ will cause your
    -- job to exit. The default is @RESTART@.
    exitBehavior :: Prelude.Maybe ExitBehavior,
    -- | The name of the tool.
    name :: Prelude.Text,
    -- | Command-line arguments for the tool. It must include the tool executable
    -- name.
    command :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamOutputToCloudWatch', 'tool_streamOutputToCloudWatch' - Boolean indicating whether logs will be recorded in CloudWatch for the
-- tool. The default is @False@.
--
-- 'streamUI', 'tool_streamUI' - Boolean indicating whether a streaming session will be configured for
-- the tool. If @True@, AWS RoboMaker will configure a connection so you
-- can interact with the tool as it is running in the simulation. It must
-- have a graphical user interface. The default is @False@.
--
-- 'exitBehavior', 'tool_exitBehavior' - Exit behavior determines what happens when your tool quits running.
-- @RESTART@ will cause your tool to be restarted. @FAIL@ will cause your
-- job to exit. The default is @RESTART@.
--
-- 'name', 'tool_name' - The name of the tool.
--
-- 'command', 'tool_command' - Command-line arguments for the tool. It must include the tool executable
-- name.
newTool ::
  -- | 'name'
  Prelude.Text ->
  -- | 'command'
  Prelude.Text ->
  Tool
newTool pName_ pCommand_ =
  Tool'
    { streamOutputToCloudWatch = Prelude.Nothing,
      streamUI = Prelude.Nothing,
      exitBehavior = Prelude.Nothing,
      name = pName_,
      command = pCommand_
    }

-- | Boolean indicating whether logs will be recorded in CloudWatch for the
-- tool. The default is @False@.
tool_streamOutputToCloudWatch :: Lens.Lens' Tool (Prelude.Maybe Prelude.Bool)
tool_streamOutputToCloudWatch = Lens.lens (\Tool' {streamOutputToCloudWatch} -> streamOutputToCloudWatch) (\s@Tool' {} a -> s {streamOutputToCloudWatch = a} :: Tool)

-- | Boolean indicating whether a streaming session will be configured for
-- the tool. If @True@, AWS RoboMaker will configure a connection so you
-- can interact with the tool as it is running in the simulation. It must
-- have a graphical user interface. The default is @False@.
tool_streamUI :: Lens.Lens' Tool (Prelude.Maybe Prelude.Bool)
tool_streamUI = Lens.lens (\Tool' {streamUI} -> streamUI) (\s@Tool' {} a -> s {streamUI = a} :: Tool)

-- | Exit behavior determines what happens when your tool quits running.
-- @RESTART@ will cause your tool to be restarted. @FAIL@ will cause your
-- job to exit. The default is @RESTART@.
tool_exitBehavior :: Lens.Lens' Tool (Prelude.Maybe ExitBehavior)
tool_exitBehavior = Lens.lens (\Tool' {exitBehavior} -> exitBehavior) (\s@Tool' {} a -> s {exitBehavior = a} :: Tool)

-- | The name of the tool.
tool_name :: Lens.Lens' Tool Prelude.Text
tool_name = Lens.lens (\Tool' {name} -> name) (\s@Tool' {} a -> s {name = a} :: Tool)

-- | Command-line arguments for the tool. It must include the tool executable
-- name.
tool_command :: Lens.Lens' Tool Prelude.Text
tool_command = Lens.lens (\Tool' {command} -> command) (\s@Tool' {} a -> s {command = a} :: Tool)

instance Core.FromJSON Tool where
  parseJSON =
    Core.withObject
      "Tool"
      ( \x ->
          Tool'
            Prelude.<$> (x Core..:? "streamOutputToCloudWatch")
            Prelude.<*> (x Core..:? "streamUI")
            Prelude.<*> (x Core..:? "exitBehavior")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "command")
      )

instance Prelude.Hashable Tool where
  hashWithSalt _salt Tool' {..} =
    _salt
      `Prelude.hashWithSalt` streamOutputToCloudWatch
      `Prelude.hashWithSalt` streamUI
      `Prelude.hashWithSalt` exitBehavior
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` command

instance Prelude.NFData Tool where
  rnf Tool' {..} =
    Prelude.rnf streamOutputToCloudWatch
      `Prelude.seq` Prelude.rnf streamUI
      `Prelude.seq` Prelude.rnf exitBehavior
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf command

instance Core.ToJSON Tool where
  toJSON Tool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("streamOutputToCloudWatch" Core..=)
              Prelude.<$> streamOutputToCloudWatch,
            ("streamUI" Core..=) Prelude.<$> streamUI,
            ("exitBehavior" Core..=) Prelude.<$> exitBehavior,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("command" Core..= command)
          ]
      )
