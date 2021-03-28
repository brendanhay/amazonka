{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.Record
  ( Record (..)
  -- * Smart constructor
  , mkRecord
  -- * Lenses
  , rDeviceLastModifiedDate
  , rKey
  , rLastModifiedBy
  , rLastModifiedDate
  , rSyncCount
  , rValue
  ) where

import qualified Network.AWS.CognitoSync.Types.RecordKey as Types
import qualified Network.AWS.CognitoSync.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The basic data structure of a dataset.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { deviceLastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified date of the client device.
  , key :: Core.Maybe Types.RecordKey
    -- ^ The key for the record.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user/device that made the last change to this record.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date on which the record was last modified.
  , syncCount :: Core.Maybe Core.Integer
    -- ^ The server sync count for this record.
  , value :: Core.Maybe Types.Value
    -- ^ The value for the record.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Record' value with any optional fields omitted.
mkRecord
    :: Record
mkRecord
  = Record'{deviceLastModifiedDate = Core.Nothing,
            key = Core.Nothing, lastModifiedBy = Core.Nothing,
            lastModifiedDate = Core.Nothing, syncCount = Core.Nothing,
            value = Core.Nothing}

-- | The last modified date of the client device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceLastModifiedDate :: Lens.Lens' Record (Core.Maybe Core.NominalDiffTime)
rDeviceLastModifiedDate = Lens.field @"deviceLastModifiedDate"
{-# INLINEABLE rDeviceLastModifiedDate #-}
{-# DEPRECATED deviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead"  #-}

-- | The key for the record.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKey :: Lens.Lens' Record (Core.Maybe Types.RecordKey)
rKey = Lens.field @"key"
{-# INLINEABLE rKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The user/device that made the last change to this record.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastModifiedBy :: Lens.Lens' Record (Core.Maybe Core.Text)
rLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE rLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date on which the record was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastModifiedDate :: Lens.Lens' Record (Core.Maybe Core.NominalDiffTime)
rLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE rLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The server sync count for this record.
--
-- /Note:/ Consider using 'syncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSyncCount :: Lens.Lens' Record (Core.Maybe Core.Integer)
rSyncCount = Lens.field @"syncCount"
{-# INLINEABLE rSyncCount #-}
{-# DEPRECATED syncCount "Use generic-lens or generic-optics with 'syncCount' instead"  #-}

-- | The value for the record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rValue :: Lens.Lens' Record (Core.Maybe Types.Value)
rValue = Lens.field @"value"
{-# INLINEABLE rValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Record where
        parseJSON
          = Core.withObject "Record" Core.$
              \ x ->
                Record' Core.<$>
                  (x Core..:? "DeviceLastModifiedDate") Core.<*> x Core..:? "Key"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "SyncCount"
                    Core.<*> x Core..:? "Value"
