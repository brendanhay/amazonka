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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.PlatformScriptKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.PlatformScriptKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The script location for a particular operating system.
--
-- /See:/ 'newPlatformScriptKey' smart constructor.
data PlatformScriptKey = PlatformScriptKey'
  { -- | The script location for Windows.
    windows :: Prelude.Maybe Prelude.Text,
    -- | The script location for Linux.
    linux :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformScriptKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windows', 'platformScriptKey_windows' - The script location for Windows.
--
-- 'linux', 'platformScriptKey_linux' - The script location for Linux.
newPlatformScriptKey ::
  PlatformScriptKey
newPlatformScriptKey =
  PlatformScriptKey'
    { windows = Prelude.Nothing,
      linux = Prelude.Nothing
    }

-- | The script location for Windows.
platformScriptKey_windows :: Lens.Lens' PlatformScriptKey (Prelude.Maybe Prelude.Text)
platformScriptKey_windows = Lens.lens (\PlatformScriptKey' {windows} -> windows) (\s@PlatformScriptKey' {} a -> s {windows = a} :: PlatformScriptKey)

-- | The script location for Linux.
platformScriptKey_linux :: Lens.Lens' PlatformScriptKey (Prelude.Maybe Prelude.Text)
platformScriptKey_linux = Lens.lens (\PlatformScriptKey' {linux} -> linux) (\s@PlatformScriptKey' {} a -> s {linux = a} :: PlatformScriptKey)

instance Data.FromJSON PlatformScriptKey where
  parseJSON =
    Data.withObject
      "PlatformScriptKey"
      ( \x ->
          PlatformScriptKey'
            Prelude.<$> (x Data..:? "windows")
            Prelude.<*> (x Data..:? "linux")
      )

instance Prelude.Hashable PlatformScriptKey where
  hashWithSalt _salt PlatformScriptKey' {..} =
    _salt `Prelude.hashWithSalt` windows
      `Prelude.hashWithSalt` linux

instance Prelude.NFData PlatformScriptKey where
  rnf PlatformScriptKey' {..} =
    Prelude.rnf windows `Prelude.seq` Prelude.rnf linux

instance Data.ToJSON PlatformScriptKey where
  toJSON PlatformScriptKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("windows" Data..=) Prelude.<$> windows,
            ("linux" Data..=) Prelude.<$> linux
          ]
      )
