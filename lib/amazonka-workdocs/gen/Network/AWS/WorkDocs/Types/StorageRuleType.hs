{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageRuleType
  ( StorageRuleType (..),

    -- * Smart constructor
    mkStorageRuleType,

    -- * Lenses
    srtStorageAllocatedInBytes,
    srtStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.StorageType as Types

-- | Describes the storage for a user.
--
-- /See:/ 'mkStorageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { -- | The amount of storage allocated, in bytes.
    storageAllocatedInBytes :: Core.Maybe Core.Natural,
    -- | The type of storage.
    storageType :: Core.Maybe Types.StorageType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageRuleType' value with any optional fields omitted.
mkStorageRuleType ::
  StorageRuleType
mkStorageRuleType =
  StorageRuleType'
    { storageAllocatedInBytes = Core.Nothing,
      storageType = Core.Nothing
    }

-- | The amount of storage allocated, in bytes.
--
-- /Note:/ Consider using 'storageAllocatedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStorageAllocatedInBytes :: Lens.Lens' StorageRuleType (Core.Maybe Core.Natural)
srtStorageAllocatedInBytes = Lens.field @"storageAllocatedInBytes"
{-# DEPRECATED srtStorageAllocatedInBytes "Use generic-lens or generic-optics with 'storageAllocatedInBytes' instead." #-}

-- | The type of storage.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStorageType :: Lens.Lens' StorageRuleType (Core.Maybe Types.StorageType)
srtStorageType = Lens.field @"storageType"
{-# DEPRECATED srtStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Core.FromJSON StorageRuleType where
  toJSON StorageRuleType {..} =
    Core.object
      ( Core.catMaybes
          [ ("StorageAllocatedInBytes" Core..=)
              Core.<$> storageAllocatedInBytes,
            ("StorageType" Core..=) Core.<$> storageType
          ]
      )

instance Core.FromJSON StorageRuleType where
  parseJSON =
    Core.withObject "StorageRuleType" Core.$
      \x ->
        StorageRuleType'
          Core.<$> (x Core..:? "StorageAllocatedInBytes")
          Core.<*> (x Core..:? "StorageType")
