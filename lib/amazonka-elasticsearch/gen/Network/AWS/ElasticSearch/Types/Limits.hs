{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Limits
  ( Limits (..)
  -- * Smart constructor
  , mkLimits
  -- * Lenses
  , lAdditionalLimits
  , lInstanceLimits
  , lStorageTypes
  ) where

import qualified Network.AWS.ElasticSearch.Types.AdditionalLimit as Types
import qualified Network.AWS.ElasticSearch.Types.InstanceLimits as Types
import qualified Network.AWS.ElasticSearch.Types.StorageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Limits for given InstanceType and for each of it's role. 
--
-- Limits contains following @'StorageTypes,' @ @'InstanceLimits' @ and @'AdditionalLimits' @ 
--
-- /See:/ 'mkLimits' smart constructor.
data Limits = Limits'
  { additionalLimits :: Core.Maybe [Types.AdditionalLimit]
    -- ^ List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ . 
  , instanceLimits :: Core.Maybe Types.InstanceLimits
  , storageTypes :: Core.Maybe [Types.StorageType]
    -- ^ StorageType represents the list of storage related types and attributes that are available for given InstanceType. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Limits' value with any optional fields omitted.
mkLimits
    :: Limits
mkLimits
  = Limits'{additionalLimits = Core.Nothing,
            instanceLimits = Core.Nothing, storageTypes = Core.Nothing}

-- | List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ . 
--
-- /Note:/ Consider using 'additionalLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAdditionalLimits :: Lens.Lens' Limits (Core.Maybe [Types.AdditionalLimit])
lAdditionalLimits = Lens.field @"additionalLimits"
{-# INLINEABLE lAdditionalLimits #-}
{-# DEPRECATED additionalLimits "Use generic-lens or generic-optics with 'additionalLimits' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'instanceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstanceLimits :: Lens.Lens' Limits (Core.Maybe Types.InstanceLimits)
lInstanceLimits = Lens.field @"instanceLimits"
{-# INLINEABLE lInstanceLimits #-}
{-# DEPRECATED instanceLimits "Use generic-lens or generic-optics with 'instanceLimits' instead"  #-}

-- | StorageType represents the list of storage related types and attributes that are available for given InstanceType. 
--
-- /Note:/ Consider using 'storageTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lStorageTypes :: Lens.Lens' Limits (Core.Maybe [Types.StorageType])
lStorageTypes = Lens.field @"storageTypes"
{-# INLINEABLE lStorageTypes #-}
{-# DEPRECATED storageTypes "Use generic-lens or generic-optics with 'storageTypes' instead"  #-}

instance Core.FromJSON Limits where
        parseJSON
          = Core.withObject "Limits" Core.$
              \ x ->
                Limits' Core.<$>
                  (x Core..:? "AdditionalLimits") Core.<*>
                    x Core..:? "InstanceLimits"
                    Core.<*> x Core..:? "StorageTypes"
