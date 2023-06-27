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
-- Module      : Amazonka.Nimble.Types.StreamingSessionStorageRoot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamingSessionStorageRoot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The upload storage root location (folder) on streaming workstations
-- where files are uploaded.
--
-- /See:/ 'newStreamingSessionStorageRoot' smart constructor.
data StreamingSessionStorageRoot = StreamingSessionStorageRoot'
  { -- | The folder path in Linux workstations where files are uploaded.
    linux :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The folder path in Windows workstations where files are uploaded.
    windows :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingSessionStorageRoot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linux', 'streamingSessionStorageRoot_linux' - The folder path in Linux workstations where files are uploaded.
--
-- 'windows', 'streamingSessionStorageRoot_windows' - The folder path in Windows workstations where files are uploaded.
newStreamingSessionStorageRoot ::
  StreamingSessionStorageRoot
newStreamingSessionStorageRoot =
  StreamingSessionStorageRoot'
    { linux =
        Prelude.Nothing,
      windows = Prelude.Nothing
    }

-- | The folder path in Linux workstations where files are uploaded.
streamingSessionStorageRoot_linux :: Lens.Lens' StreamingSessionStorageRoot (Prelude.Maybe Prelude.Text)
streamingSessionStorageRoot_linux = Lens.lens (\StreamingSessionStorageRoot' {linux} -> linux) (\s@StreamingSessionStorageRoot' {} a -> s {linux = a} :: StreamingSessionStorageRoot) Prelude.. Lens.mapping Data._Sensitive

-- | The folder path in Windows workstations where files are uploaded.
streamingSessionStorageRoot_windows :: Lens.Lens' StreamingSessionStorageRoot (Prelude.Maybe Prelude.Text)
streamingSessionStorageRoot_windows = Lens.lens (\StreamingSessionStorageRoot' {windows} -> windows) (\s@StreamingSessionStorageRoot' {} a -> s {windows = a} :: StreamingSessionStorageRoot) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON StreamingSessionStorageRoot where
  parseJSON =
    Data.withObject
      "StreamingSessionStorageRoot"
      ( \x ->
          StreamingSessionStorageRoot'
            Prelude.<$> (x Data..:? "linux")
            Prelude.<*> (x Data..:? "windows")
      )

instance Prelude.Hashable StreamingSessionStorageRoot where
  hashWithSalt _salt StreamingSessionStorageRoot' {..} =
    _salt
      `Prelude.hashWithSalt` linux
      `Prelude.hashWithSalt` windows

instance Prelude.NFData StreamingSessionStorageRoot where
  rnf StreamingSessionStorageRoot' {..} =
    Prelude.rnf linux `Prelude.seq` Prelude.rnf windows

instance Data.ToJSON StreamingSessionStorageRoot where
  toJSON StreamingSessionStorageRoot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("linux" Data..=) Prelude.<$> linux,
            ("windows" Data..=) Prelude.<$> windows
          ]
      )
