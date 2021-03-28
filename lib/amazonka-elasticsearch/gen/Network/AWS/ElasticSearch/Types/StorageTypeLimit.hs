{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.StorageTypeLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.StorageTypeLimit
  ( StorageTypeLimit (..)
  -- * Smart constructor
  , mkStorageTypeLimit
  -- * Lenses
  , stlLimitName
  , stlLimitValues
  ) where

import qualified Network.AWS.ElasticSearch.Types.LimitName as Types
import qualified Network.AWS.ElasticSearch.Types.LimitValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Limits that are applicable for given storage type. 
--
-- /See:/ 'mkStorageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { limitName :: Core.Maybe Types.LimitName
    -- ^ Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable 
--
--     * MinimumVolumeSize
-- Minimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MaximumVolumeSize
-- Maximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MaximumIops
-- Maximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MinimumIops
-- Minimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable. 
--
  , limitValues :: Core.Maybe [Types.LimitValue]
    -- ^ Values for the @'StorageTypeLimit$LimitName' @ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StorageTypeLimit' value with any optional fields omitted.
mkStorageTypeLimit
    :: StorageTypeLimit
mkStorageTypeLimit
  = StorageTypeLimit'{limitName = Core.Nothing,
                      limitValues = Core.Nothing}

-- | Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable 
--
--     * MinimumVolumeSize
-- Minimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MaximumVolumeSize
-- Maximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MaximumIops
-- Maximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable. 
--     * MinimumIops
-- Minimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable. 
--
--
-- /Note:/ Consider using 'limitName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stlLimitName :: Lens.Lens' StorageTypeLimit (Core.Maybe Types.LimitName)
stlLimitName = Lens.field @"limitName"
{-# INLINEABLE stlLimitName #-}
{-# DEPRECATED limitName "Use generic-lens or generic-optics with 'limitName' instead"  #-}

-- | Values for the @'StorageTypeLimit$LimitName' @ . 
--
-- /Note:/ Consider using 'limitValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stlLimitValues :: Lens.Lens' StorageTypeLimit (Core.Maybe [Types.LimitValue])
stlLimitValues = Lens.field @"limitValues"
{-# INLINEABLE stlLimitValues #-}
{-# DEPRECATED limitValues "Use generic-lens or generic-optics with 'limitValues' instead"  #-}

instance Core.FromJSON StorageTypeLimit where
        parseJSON
          = Core.withObject "StorageTypeLimit" Core.$
              \ x ->
                StorageTypeLimit' Core.<$>
                  (x Core..:? "LimitName") Core.<*> x Core..:? "LimitValues"
