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
-- Module      : Amazonka.Nimble.Types.StreamConfigurationSessionStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfigurationSessionStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StreamingSessionStorageMode
import Amazonka.Nimble.Types.StreamingSessionStorageRoot
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a streaming session’s upload storage.
--
-- /See:/ 'newStreamConfigurationSessionStorage' smart constructor.
data StreamConfigurationSessionStorage = StreamConfigurationSessionStorage'
  { -- | The configuration for the upload storage root of the streaming session.
    root :: Prelude.Maybe StreamingSessionStorageRoot,
    -- | Allows artists to upload files to their workstations. The only valid
    -- option is @UPLOAD@.
    mode :: Prelude.NonEmpty StreamingSessionStorageMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamConfigurationSessionStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'root', 'streamConfigurationSessionStorage_root' - The configuration for the upload storage root of the streaming session.
--
-- 'mode', 'streamConfigurationSessionStorage_mode' - Allows artists to upload files to their workstations. The only valid
-- option is @UPLOAD@.
newStreamConfigurationSessionStorage ::
  -- | 'mode'
  Prelude.NonEmpty StreamingSessionStorageMode ->
  StreamConfigurationSessionStorage
newStreamConfigurationSessionStorage pMode_ =
  StreamConfigurationSessionStorage'
    { root =
        Prelude.Nothing,
      mode = Lens.coerced Lens.# pMode_
    }

-- | The configuration for the upload storage root of the streaming session.
streamConfigurationSessionStorage_root :: Lens.Lens' StreamConfigurationSessionStorage (Prelude.Maybe StreamingSessionStorageRoot)
streamConfigurationSessionStorage_root = Lens.lens (\StreamConfigurationSessionStorage' {root} -> root) (\s@StreamConfigurationSessionStorage' {} a -> s {root = a} :: StreamConfigurationSessionStorage)

-- | Allows artists to upload files to their workstations. The only valid
-- option is @UPLOAD@.
streamConfigurationSessionStorage_mode :: Lens.Lens' StreamConfigurationSessionStorage (Prelude.NonEmpty StreamingSessionStorageMode)
streamConfigurationSessionStorage_mode = Lens.lens (\StreamConfigurationSessionStorage' {mode} -> mode) (\s@StreamConfigurationSessionStorage' {} a -> s {mode = a} :: StreamConfigurationSessionStorage) Prelude.. Lens.coerced

instance
  Data.FromJSON
    StreamConfigurationSessionStorage
  where
  parseJSON =
    Data.withObject
      "StreamConfigurationSessionStorage"
      ( \x ->
          StreamConfigurationSessionStorage'
            Prelude.<$> (x Data..:? "root")
            Prelude.<*> (x Data..: "mode")
      )

instance
  Prelude.Hashable
    StreamConfigurationSessionStorage
  where
  hashWithSalt
    _salt
    StreamConfigurationSessionStorage' {..} =
      _salt
        `Prelude.hashWithSalt` root
        `Prelude.hashWithSalt` mode

instance
  Prelude.NFData
    StreamConfigurationSessionStorage
  where
  rnf StreamConfigurationSessionStorage' {..} =
    Prelude.rnf root `Prelude.seq` Prelude.rnf mode

instance
  Data.ToJSON
    StreamConfigurationSessionStorage
  where
  toJSON StreamConfigurationSessionStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("root" Data..=) Prelude.<$> root,
            Prelude.Just ("mode" Data..= mode)
          ]
      )
