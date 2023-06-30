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
-- Module      : Amazonka.TimeStreamWrite.Types.RecordsIngested
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.RecordsIngested where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information on the records ingested by this request.
--
-- /See:/ 'newRecordsIngested' smart constructor.
data RecordsIngested = RecordsIngested'
  { -- | Count of records ingested into the magnetic store.
    magneticStore :: Prelude.Maybe Prelude.Int,
    -- | Count of records ingested into the memory store.
    memoryStore :: Prelude.Maybe Prelude.Int,
    -- | Total count of successfully ingested records.
    total :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordsIngested' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'magneticStore', 'recordsIngested_magneticStore' - Count of records ingested into the magnetic store.
--
-- 'memoryStore', 'recordsIngested_memoryStore' - Count of records ingested into the memory store.
--
-- 'total', 'recordsIngested_total' - Total count of successfully ingested records.
newRecordsIngested ::
  RecordsIngested
newRecordsIngested =
  RecordsIngested'
    { magneticStore = Prelude.Nothing,
      memoryStore = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | Count of records ingested into the magnetic store.
recordsIngested_magneticStore :: Lens.Lens' RecordsIngested (Prelude.Maybe Prelude.Int)
recordsIngested_magneticStore = Lens.lens (\RecordsIngested' {magneticStore} -> magneticStore) (\s@RecordsIngested' {} a -> s {magneticStore = a} :: RecordsIngested)

-- | Count of records ingested into the memory store.
recordsIngested_memoryStore :: Lens.Lens' RecordsIngested (Prelude.Maybe Prelude.Int)
recordsIngested_memoryStore = Lens.lens (\RecordsIngested' {memoryStore} -> memoryStore) (\s@RecordsIngested' {} a -> s {memoryStore = a} :: RecordsIngested)

-- | Total count of successfully ingested records.
recordsIngested_total :: Lens.Lens' RecordsIngested (Prelude.Maybe Prelude.Int)
recordsIngested_total = Lens.lens (\RecordsIngested' {total} -> total) (\s@RecordsIngested' {} a -> s {total = a} :: RecordsIngested)

instance Data.FromJSON RecordsIngested where
  parseJSON =
    Data.withObject
      "RecordsIngested"
      ( \x ->
          RecordsIngested'
            Prelude.<$> (x Data..:? "MagneticStore")
            Prelude.<*> (x Data..:? "MemoryStore")
            Prelude.<*> (x Data..:? "Total")
      )

instance Prelude.Hashable RecordsIngested where
  hashWithSalt _salt RecordsIngested' {..} =
    _salt
      `Prelude.hashWithSalt` magneticStore
      `Prelude.hashWithSalt` memoryStore
      `Prelude.hashWithSalt` total

instance Prelude.NFData RecordsIngested where
  rnf RecordsIngested' {..} =
    Prelude.rnf magneticStore
      `Prelude.seq` Prelude.rnf memoryStore
      `Prelude.seq` Prelude.rnf total
