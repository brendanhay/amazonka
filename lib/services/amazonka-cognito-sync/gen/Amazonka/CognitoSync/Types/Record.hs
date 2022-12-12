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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The basic data structure of a dataset.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The last modified date of the client device.
    deviceLastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The key for the record.
    key :: Prelude.Maybe Prelude.Text,
    -- | The user\/device that made the last change to this record.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date on which the record was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The server sync count for this record.
    syncCount :: Prelude.Maybe Prelude.Integer,
    -- | The value for the record.
    value :: Prelude.Maybe Prelude.Text
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
-- 'deviceLastModifiedDate', 'record_deviceLastModifiedDate' - The last modified date of the client device.
--
-- 'key', 'record_key' - The key for the record.
--
-- 'lastModifiedBy', 'record_lastModifiedBy' - The user\/device that made the last change to this record.
--
-- 'lastModifiedDate', 'record_lastModifiedDate' - The date on which the record was last modified.
--
-- 'syncCount', 'record_syncCount' - The server sync count for this record.
--
-- 'value', 'record_value' - The value for the record.
newRecord ::
  Record
newRecord =
  Record'
    { deviceLastModifiedDate = Prelude.Nothing,
      key = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      syncCount = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The last modified date of the client device.
record_deviceLastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_deviceLastModifiedDate = Lens.lens (\Record' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@Record' {} a -> s {deviceLastModifiedDate = a} :: Record) Prelude.. Lens.mapping Data._Time

-- | The key for the record.
record_key :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_key = Lens.lens (\Record' {key} -> key) (\s@Record' {} a -> s {key = a} :: Record)

-- | The user\/device that made the last change to this record.
record_lastModifiedBy :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_lastModifiedBy = Lens.lens (\Record' {lastModifiedBy} -> lastModifiedBy) (\s@Record' {} a -> s {lastModifiedBy = a} :: Record)

-- | The date on which the record was last modified.
record_lastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_lastModifiedDate = Lens.lens (\Record' {lastModifiedDate} -> lastModifiedDate) (\s@Record' {} a -> s {lastModifiedDate = a} :: Record) Prelude.. Lens.mapping Data._Time

-- | The server sync count for this record.
record_syncCount :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_syncCount = Lens.lens (\Record' {syncCount} -> syncCount) (\s@Record' {} a -> s {syncCount = a} :: Record)

-- | The value for the record.
record_value :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_value = Lens.lens (\Record' {value} -> value) (\s@Record' {} a -> s {value = a} :: Record)

instance Data.FromJSON Record where
  parseJSON =
    Data.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Data..:? "DeviceLastModifiedDate")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "SyncCount")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Record where
  hashWithSalt _salt Record' {..} =
    _salt `Prelude.hashWithSalt` deviceLastModifiedDate
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` syncCount
      `Prelude.hashWithSalt` value

instance Prelude.NFData Record where
  rnf Record' {..} =
    Prelude.rnf deviceLastModifiedDate
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf syncCount
      `Prelude.seq` Prelude.rnf value
