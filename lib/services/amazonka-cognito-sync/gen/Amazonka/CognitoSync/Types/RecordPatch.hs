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
-- Module      : Amazonka.CognitoSync.Types.RecordPatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.RecordPatch where

import Amazonka.CognitoSync.Types.Operation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An update operation for a record.
--
-- /See:/ 'newRecordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { -- | The last modified date of the client device.
    deviceLastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The value associated with the record patch.
    value :: Prelude.Maybe Prelude.Text,
    -- | An operation, either replace or remove.
    op :: Operation,
    -- | The key associated with the record patch.
    key :: Prelude.Text,
    -- | Last known server sync count for this record. Set to 0 if unknown.
    syncCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordPatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceLastModifiedDate', 'recordPatch_deviceLastModifiedDate' - The last modified date of the client device.
--
-- 'value', 'recordPatch_value' - The value associated with the record patch.
--
-- 'op', 'recordPatch_op' - An operation, either replace or remove.
--
-- 'key', 'recordPatch_key' - The key associated with the record patch.
--
-- 'syncCount', 'recordPatch_syncCount' - Last known server sync count for this record. Set to 0 if unknown.
newRecordPatch ::
  -- | 'op'
  Operation ->
  -- | 'key'
  Prelude.Text ->
  -- | 'syncCount'
  Prelude.Integer ->
  RecordPatch
newRecordPatch pOp_ pKey_ pSyncCount_ =
  RecordPatch'
    { deviceLastModifiedDate =
        Prelude.Nothing,
      value = Prelude.Nothing,
      op = pOp_,
      key = pKey_,
      syncCount = pSyncCount_
    }

-- | The last modified date of the client device.
recordPatch_deviceLastModifiedDate :: Lens.Lens' RecordPatch (Prelude.Maybe Prelude.UTCTime)
recordPatch_deviceLastModifiedDate = Lens.lens (\RecordPatch' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@RecordPatch' {} a -> s {deviceLastModifiedDate = a} :: RecordPatch) Prelude.. Lens.mapping Data._Time

-- | The value associated with the record patch.
recordPatch_value :: Lens.Lens' RecordPatch (Prelude.Maybe Prelude.Text)
recordPatch_value = Lens.lens (\RecordPatch' {value} -> value) (\s@RecordPatch' {} a -> s {value = a} :: RecordPatch)

-- | An operation, either replace or remove.
recordPatch_op :: Lens.Lens' RecordPatch Operation
recordPatch_op = Lens.lens (\RecordPatch' {op} -> op) (\s@RecordPatch' {} a -> s {op = a} :: RecordPatch)

-- | The key associated with the record patch.
recordPatch_key :: Lens.Lens' RecordPatch Prelude.Text
recordPatch_key = Lens.lens (\RecordPatch' {key} -> key) (\s@RecordPatch' {} a -> s {key = a} :: RecordPatch)

-- | Last known server sync count for this record. Set to 0 if unknown.
recordPatch_syncCount :: Lens.Lens' RecordPatch Prelude.Integer
recordPatch_syncCount = Lens.lens (\RecordPatch' {syncCount} -> syncCount) (\s@RecordPatch' {} a -> s {syncCount = a} :: RecordPatch)

instance Prelude.Hashable RecordPatch where
  hashWithSalt _salt RecordPatch' {..} =
    _salt
      `Prelude.hashWithSalt` deviceLastModifiedDate
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` op
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` syncCount

instance Prelude.NFData RecordPatch where
  rnf RecordPatch' {..} =
    Prelude.rnf deviceLastModifiedDate
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf op
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf syncCount

instance Data.ToJSON RecordPatch where
  toJSON RecordPatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceLastModifiedDate" Data..=)
              Prelude.<$> deviceLastModifiedDate,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Op" Data..= op),
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("SyncCount" Data..= syncCount)
          ]
      )
