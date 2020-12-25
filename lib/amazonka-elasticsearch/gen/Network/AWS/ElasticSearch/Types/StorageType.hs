{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.StorageType
  ( StorageType (..),

    -- * Smart constructor
    mkStorageType,

    -- * Lenses
    stStorageSubTypeName,
    stStorageTypeLimits,
    stStorageTypeName,
  )
where

import qualified Network.AWS.ElasticSearch.Types.StorageSubTypeName as Types
import qualified Network.AWS.ElasticSearch.Types.StorageTypeLimit as Types
import qualified Network.AWS.ElasticSearch.Types.StorageTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType.
--
-- /See:/ 'mkStorageType' smart constructor.
data StorageType = StorageType'
  { storageSubTypeName :: Core.Maybe Types.StorageSubTypeName,
    -- | List of limits that are applicable for given storage type.
    storageTypeLimits :: Core.Maybe [Types.StorageTypeLimit],
    storageTypeName :: Core.Maybe Types.StorageTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageType' value with any optional fields omitted.
mkStorageType ::
  StorageType
mkStorageType =
  StorageType'
    { storageSubTypeName = Core.Nothing,
      storageTypeLimits = Core.Nothing,
      storageTypeName = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageSubTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageSubTypeName :: Lens.Lens' StorageType (Core.Maybe Types.StorageSubTypeName)
stStorageSubTypeName = Lens.field @"storageSubTypeName"
{-# DEPRECATED stStorageSubTypeName "Use generic-lens or generic-optics with 'storageSubTypeName' instead." #-}

-- | List of limits that are applicable for given storage type.
--
-- /Note:/ Consider using 'storageTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageTypeLimits :: Lens.Lens' StorageType (Core.Maybe [Types.StorageTypeLimit])
stStorageTypeLimits = Lens.field @"storageTypeLimits"
{-# DEPRECATED stStorageTypeLimits "Use generic-lens or generic-optics with 'storageTypeLimits' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStorageTypeName :: Lens.Lens' StorageType (Core.Maybe Types.StorageTypeName)
stStorageTypeName = Lens.field @"storageTypeName"
{-# DEPRECATED stStorageTypeName "Use generic-lens or generic-optics with 'storageTypeName' instead." #-}

instance Core.FromJSON StorageType where
  parseJSON =
    Core.withObject "StorageType" Core.$
      \x ->
        StorageType'
          Core.<$> (x Core..:? "StorageSubTypeName")
          Core.<*> (x Core..:? "StorageTypeLimits")
          Core.<*> (x Core..:? "StorageTypeName")
