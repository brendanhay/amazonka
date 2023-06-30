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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.PlatformCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.PlatformCommand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Command to be run on a particular operating system.
--
-- /See:/ 'newPlatformCommand' smart constructor.
data PlatformCommand = PlatformCommand'
  { -- | Command for Linux.
    linux :: Prelude.Maybe Prelude.Text,
    -- | Command for Windows.
    windows :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linux', 'platformCommand_linux' - Command for Linux.
--
-- 'windows', 'platformCommand_windows' - Command for Windows.
newPlatformCommand ::
  PlatformCommand
newPlatformCommand =
  PlatformCommand'
    { linux = Prelude.Nothing,
      windows = Prelude.Nothing
    }

-- | Command for Linux.
platformCommand_linux :: Lens.Lens' PlatformCommand (Prelude.Maybe Prelude.Text)
platformCommand_linux = Lens.lens (\PlatformCommand' {linux} -> linux) (\s@PlatformCommand' {} a -> s {linux = a} :: PlatformCommand)

-- | Command for Windows.
platformCommand_windows :: Lens.Lens' PlatformCommand (Prelude.Maybe Prelude.Text)
platformCommand_windows = Lens.lens (\PlatformCommand' {windows} -> windows) (\s@PlatformCommand' {} a -> s {windows = a} :: PlatformCommand)

instance Data.FromJSON PlatformCommand where
  parseJSON =
    Data.withObject
      "PlatformCommand"
      ( \x ->
          PlatformCommand'
            Prelude.<$> (x Data..:? "linux")
            Prelude.<*> (x Data..:? "windows")
      )

instance Prelude.Hashable PlatformCommand where
  hashWithSalt _salt PlatformCommand' {..} =
    _salt
      `Prelude.hashWithSalt` linux
      `Prelude.hashWithSalt` windows

instance Prelude.NFData PlatformCommand where
  rnf PlatformCommand' {..} =
    Prelude.rnf linux `Prelude.seq` Prelude.rnf windows

instance Data.ToJSON PlatformCommand where
  toJSON PlatformCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("linux" Data..=) Prelude.<$> linux,
            ("windows" Data..=) Prelude.<$> windows
          ]
      )
