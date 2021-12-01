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
-- Module      : Amazonka.CognitoSync.Types.Record
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The basic data structure of a dataset.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The server sync count for this record.
    syncCount :: Prelude.Maybe Prelude.Integer,
    -- | The last modified date of the client device.
    deviceLastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The date on which the record was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The value for the record.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key for the record.
    key :: Prelude.Maybe Prelude.Text,
    -- | The user\/device that made the last change to this record.
    lastModifiedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncCount', 'record_syncCount' - The server sync count for this record.
--
-- 'deviceLastModifiedDate', 'record_deviceLastModifiedDate' - The last modified date of the client device.
--
-- 'lastModifiedDate', 'record_lastModifiedDate' - The date on which the record was last modified.
--
-- 'value', 'record_value' - The value for the record.
--
-- 'key', 'record_key' - The key for the record.
--
-- 'lastModifiedBy', 'record_lastModifiedBy' - The user\/device that made the last change to this record.
newRecord ::
  Record
newRecord =
  Record'
    { syncCount = Prelude.Nothing,
      deviceLastModifiedDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      value = Prelude.Nothing,
      key = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing
    }

-- | The server sync count for this record.
record_syncCount :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_syncCount = Lens.lens (\Record' {syncCount} -> syncCount) (\s@Record' {} a -> s {syncCount = a} :: Record)

-- | The last modified date of the client device.
record_deviceLastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_deviceLastModifiedDate = Lens.lens (\Record' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@Record' {} a -> s {deviceLastModifiedDate = a} :: Record) Prelude.. Lens.mapping Core._Time

-- | The date on which the record was last modified.
record_lastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_lastModifiedDate = Lens.lens (\Record' {lastModifiedDate} -> lastModifiedDate) (\s@Record' {} a -> s {lastModifiedDate = a} :: Record) Prelude.. Lens.mapping Core._Time

-- | The value for the record.
record_value :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_value = Lens.lens (\Record' {value} -> value) (\s@Record' {} a -> s {value = a} :: Record)

-- | The key for the record.
record_key :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_key = Lens.lens (\Record' {key} -> key) (\s@Record' {} a -> s {key = a} :: Record)

-- | The user\/device that made the last change to this record.
record_lastModifiedBy :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_lastModifiedBy = Lens.lens (\Record' {lastModifiedBy} -> lastModifiedBy) (\s@Record' {} a -> s {lastModifiedBy = a} :: Record)

instance Core.FromJSON Record where
  parseJSON =
    Core.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Core..:? "SyncCount")
            Prelude.<*> (x Core..:? "DeviceLastModifiedDate")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "LastModifiedBy")
      )

instance Prelude.Hashable Record where
  hashWithSalt salt' Record' {..} =
    salt' `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` deviceLastModifiedDate
      `Prelude.hashWithSalt` syncCount

instance Prelude.NFData Record where
  rnf Record' {..} =
    Prelude.rnf syncCount
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf deviceLastModifiedDate
