{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoSync.Types.Record
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Record where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The basic data structure of a dataset.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The last modified date of the client device.
    deviceLastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date on which the record was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The key for the record.
    key :: Prelude.Maybe Prelude.Text,
    -- | The server sync count for this record.
    syncCount :: Prelude.Maybe Prelude.Integer,
    -- | The value for the record.
    value :: Prelude.Maybe Prelude.Text,
    -- | The user\/device that made the last change to this record.
    lastModifiedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'lastModifiedDate', 'record_lastModifiedDate' - The date on which the record was last modified.
--
-- 'key', 'record_key' - The key for the record.
--
-- 'syncCount', 'record_syncCount' - The server sync count for this record.
--
-- 'value', 'record_value' - The value for the record.
--
-- 'lastModifiedBy', 'record_lastModifiedBy' - The user\/device that made the last change to this record.
newRecord ::
  Record
newRecord =
  Record'
    { deviceLastModifiedDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      key = Prelude.Nothing,
      syncCount = Prelude.Nothing,
      value = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing
    }

-- | The last modified date of the client device.
record_deviceLastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_deviceLastModifiedDate = Lens.lens (\Record' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@Record' {} a -> s {deviceLastModifiedDate = a} :: Record) Prelude.. Lens.mapping Prelude._Time

-- | The date on which the record was last modified.
record_lastModifiedDate :: Lens.Lens' Record (Prelude.Maybe Prelude.UTCTime)
record_lastModifiedDate = Lens.lens (\Record' {lastModifiedDate} -> lastModifiedDate) (\s@Record' {} a -> s {lastModifiedDate = a} :: Record) Prelude.. Lens.mapping Prelude._Time

-- | The key for the record.
record_key :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_key = Lens.lens (\Record' {key} -> key) (\s@Record' {} a -> s {key = a} :: Record)

-- | The server sync count for this record.
record_syncCount :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_syncCount = Lens.lens (\Record' {syncCount} -> syncCount) (\s@Record' {} a -> s {syncCount = a} :: Record)

-- | The value for the record.
record_value :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_value = Lens.lens (\Record' {value} -> value) (\s@Record' {} a -> s {value = a} :: Record)

-- | The user\/device that made the last change to this record.
record_lastModifiedBy :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_lastModifiedBy = Lens.lens (\Record' {lastModifiedBy} -> lastModifiedBy) (\s@Record' {} a -> s {lastModifiedBy = a} :: Record)

instance Prelude.FromJSON Record where
  parseJSON =
    Prelude.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Prelude..:? "DeviceLastModifiedDate")
            Prelude.<*> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "SyncCount")
            Prelude.<*> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
      )

instance Prelude.Hashable Record

instance Prelude.NFData Record
