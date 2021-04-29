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
-- Module      : Network.AWS.CognitoSync.Types.RecordPatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.RecordPatch where

import Network.AWS.CognitoSync.Types.Operation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An update operation for a record.
--
-- /See:/ 'newRecordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { -- | The last modified date of the client device.
    deviceLastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The value associated with the record patch.
    value :: Prelude.Maybe Prelude.Text,
    -- | An operation, either replace or remove.
    op :: Operation,
    -- | The key associated with the record patch.
    key :: Prelude.Text,
    -- | Last known server sync count for this record. Set to 0 if unknown.
    syncCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
recordPatch_deviceLastModifiedDate = Lens.lens (\RecordPatch' {deviceLastModifiedDate} -> deviceLastModifiedDate) (\s@RecordPatch' {} a -> s {deviceLastModifiedDate = a} :: RecordPatch) Prelude.. Lens.mapping Prelude._Time

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

instance Prelude.Hashable RecordPatch

instance Prelude.NFData RecordPatch

instance Prelude.ToJSON RecordPatch where
  toJSON RecordPatch' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceLastModifiedDate" Prelude..=)
              Prelude.<$> deviceLastModifiedDate,
            ("Value" Prelude..=) Prelude.<$> value,
            Prelude.Just ("Op" Prelude..= op),
            Prelude.Just ("Key" Prelude..= key),
            Prelude.Just ("SyncCount" Prelude..= syncCount)
          ]
      )
