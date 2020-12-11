-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.RecordPatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.RecordPatch
  ( RecordPatch (..),

    -- * Smart constructor
    mkRecordPatch,

    -- * Lenses
    rpDeviceLastModifiedDate,
    rpValue,
    rpOp,
    rpKey,
    rpSyncCount,
  )
where

import Network.AWS.CognitoSync.Types.Operation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An update operation for a record.
--
-- /See:/ 'mkRecordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { deviceLastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    value :: Lude.Maybe Lude.Text,
    op :: Operation,
    key :: Lude.Text,
    syncCount :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordPatch' with the minimum fields required to make a request.
--
-- * 'deviceLastModifiedDate' - The last modified date of the client device.
-- * 'key' - The key associated with the record patch.
-- * 'op' - An operation, either replace or remove.
-- * 'syncCount' - Last known server sync count for this record. Set to 0 if unknown.
-- * 'value' - The value associated with the record patch.
mkRecordPatch ::
  -- | 'op'
  Operation ->
  -- | 'key'
  Lude.Text ->
  -- | 'syncCount'
  Lude.Integer ->
  RecordPatch
mkRecordPatch pOp_ pKey_ pSyncCount_ =
  RecordPatch'
    { deviceLastModifiedDate = Lude.Nothing,
      value = Lude.Nothing,
      op = pOp_,
      key = pKey_,
      syncCount = pSyncCount_
    }

-- | The last modified date of the client device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDeviceLastModifiedDate :: Lens.Lens' RecordPatch (Lude.Maybe Lude.Timestamp)
rpDeviceLastModifiedDate = Lens.lens (deviceLastModifiedDate :: RecordPatch -> Lude.Maybe Lude.Timestamp) (\s a -> s {deviceLastModifiedDate = a} :: RecordPatch)
{-# DEPRECATED rpDeviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead." #-}

-- | The value associated with the record patch.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpValue :: Lens.Lens' RecordPatch (Lude.Maybe Lude.Text)
rpValue = Lens.lens (value :: RecordPatch -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: RecordPatch)
{-# DEPRECATED rpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An operation, either replace or remove.
--
-- /Note:/ Consider using 'op' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpOp :: Lens.Lens' RecordPatch Operation
rpOp = Lens.lens (op :: RecordPatch -> Operation) (\s a -> s {op = a} :: RecordPatch)
{-# DEPRECATED rpOp "Use generic-lens or generic-optics with 'op' instead." #-}

-- | The key associated with the record patch.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpKey :: Lens.Lens' RecordPatch Lude.Text
rpKey = Lens.lens (key :: RecordPatch -> Lude.Text) (\s a -> s {key = a} :: RecordPatch)
{-# DEPRECATED rpKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Last known server sync count for this record. Set to 0 if unknown.
--
-- /Note:/ Consider using 'syncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpSyncCount :: Lens.Lens' RecordPatch Lude.Integer
rpSyncCount = Lens.lens (syncCount :: RecordPatch -> Lude.Integer) (\s a -> s {syncCount = a} :: RecordPatch)
{-# DEPRECATED rpSyncCount "Use generic-lens or generic-optics with 'syncCount' instead." #-}

instance Lude.ToJSON RecordPatch where
  toJSON RecordPatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeviceLastModifiedDate" Lude..=)
              Lude.<$> deviceLastModifiedDate,
            ("Value" Lude..=) Lude.<$> value,
            Lude.Just ("Op" Lude..= op),
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("SyncCount" Lude..= syncCount)
          ]
      )
