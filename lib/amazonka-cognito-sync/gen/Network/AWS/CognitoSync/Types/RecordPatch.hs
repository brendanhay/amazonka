{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.RecordPatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.RecordPatch
  ( RecordPatch (..)
  -- * Smart constructor
  , mkRecordPatch
  -- * Lenses
  , rpOp
  , rpKey
  , rpSyncCount
  , rpDeviceLastModifiedDate
  , rpValue
  ) where

import qualified Network.AWS.CognitoSync.Types.Operation as Types
import qualified Network.AWS.CognitoSync.Types.RecordKey as Types
import qualified Network.AWS.CognitoSync.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An update operation for a record.
--
-- /See:/ 'mkRecordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { op :: Types.Operation
    -- ^ An operation, either replace or remove.
  , key :: Types.RecordKey
    -- ^ The key associated with the record patch.
  , syncCount :: Core.Integer
    -- ^ Last known server sync count for this record. Set to 0 if unknown.
  , deviceLastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified date of the client device.
  , value :: Core.Maybe Types.Value
    -- ^ The value associated with the record patch.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RecordPatch' value with any optional fields omitted.
mkRecordPatch
    :: Types.Operation -- ^ 'op'
    -> Types.RecordKey -- ^ 'key'
    -> Core.Integer -- ^ 'syncCount'
    -> RecordPatch
mkRecordPatch op key syncCount
  = RecordPatch'{op, key, syncCount,
                 deviceLastModifiedDate = Core.Nothing, value = Core.Nothing}

-- | An operation, either replace or remove.
--
-- /Note:/ Consider using 'op' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpOp :: Lens.Lens' RecordPatch Types.Operation
rpOp = Lens.field @"op"
{-# INLINEABLE rpOp #-}
{-# DEPRECATED op "Use generic-lens or generic-optics with 'op' instead"  #-}

-- | The key associated with the record patch.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpKey :: Lens.Lens' RecordPatch Types.RecordKey
rpKey = Lens.field @"key"
{-# INLINEABLE rpKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Last known server sync count for this record. Set to 0 if unknown.
--
-- /Note:/ Consider using 'syncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpSyncCount :: Lens.Lens' RecordPatch Core.Integer
rpSyncCount = Lens.field @"syncCount"
{-# INLINEABLE rpSyncCount #-}
{-# DEPRECATED syncCount "Use generic-lens or generic-optics with 'syncCount' instead"  #-}

-- | The last modified date of the client device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDeviceLastModifiedDate :: Lens.Lens' RecordPatch (Core.Maybe Core.NominalDiffTime)
rpDeviceLastModifiedDate = Lens.field @"deviceLastModifiedDate"
{-# INLINEABLE rpDeviceLastModifiedDate #-}
{-# DEPRECATED deviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead"  #-}

-- | The value associated with the record patch.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpValue :: Lens.Lens' RecordPatch (Core.Maybe Types.Value)
rpValue = Lens.field @"value"
{-# INLINEABLE rpValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON RecordPatch where
        toJSON RecordPatch{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Op" Core..= op), Core.Just ("Key" Core..= key),
                  Core.Just ("SyncCount" Core..= syncCount),
                  ("DeviceLastModifiedDate" Core..=) Core.<$> deviceLastModifiedDate,
                  ("Value" Core..=) Core.<$> value])
