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
-- Module      : Amazonka.Transfer.Types.HomeDirectoryMapEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.HomeDirectoryMapEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an object that contains entries and targets for
-- @HomeDirectoryMappings@.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- /See:/ 'newHomeDirectoryMapEntry' smart constructor.
data HomeDirectoryMapEntry = HomeDirectoryMapEntry'
  { -- | Represents an entry for @HomeDirectoryMappings@.
    entry :: Prelude.Text,
    -- | Represents the map target that is used in a @HomeDirectorymapEntry@.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HomeDirectoryMapEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entry', 'homeDirectoryMapEntry_entry' - Represents an entry for @HomeDirectoryMappings@.
--
-- 'target', 'homeDirectoryMapEntry_target' - Represents the map target that is used in a @HomeDirectorymapEntry@.
newHomeDirectoryMapEntry ::
  -- | 'entry'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  HomeDirectoryMapEntry
newHomeDirectoryMapEntry pEntry_ pTarget_ =
  HomeDirectoryMapEntry'
    { entry = pEntry_,
      target = pTarget_
    }

-- | Represents an entry for @HomeDirectoryMappings@.
homeDirectoryMapEntry_entry :: Lens.Lens' HomeDirectoryMapEntry Prelude.Text
homeDirectoryMapEntry_entry = Lens.lens (\HomeDirectoryMapEntry' {entry} -> entry) (\s@HomeDirectoryMapEntry' {} a -> s {entry = a} :: HomeDirectoryMapEntry)

-- | Represents the map target that is used in a @HomeDirectorymapEntry@.
homeDirectoryMapEntry_target :: Lens.Lens' HomeDirectoryMapEntry Prelude.Text
homeDirectoryMapEntry_target = Lens.lens (\HomeDirectoryMapEntry' {target} -> target) (\s@HomeDirectoryMapEntry' {} a -> s {target = a} :: HomeDirectoryMapEntry)

instance Data.FromJSON HomeDirectoryMapEntry where
  parseJSON =
    Data.withObject
      "HomeDirectoryMapEntry"
      ( \x ->
          HomeDirectoryMapEntry'
            Prelude.<$> (x Data..: "Entry") Prelude.<*> (x Data..: "Target")
      )

instance Prelude.Hashable HomeDirectoryMapEntry where
  hashWithSalt _salt HomeDirectoryMapEntry' {..} =
    _salt `Prelude.hashWithSalt` entry
      `Prelude.hashWithSalt` target

instance Prelude.NFData HomeDirectoryMapEntry where
  rnf HomeDirectoryMapEntry' {..} =
    Prelude.rnf entry `Prelude.seq` Prelude.rnf target

instance Data.ToJSON HomeDirectoryMapEntry where
  toJSON HomeDirectoryMapEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Entry" Data..= entry),
            Prelude.Just ("Target" Data..= target)
          ]
      )
