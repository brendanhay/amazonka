{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchFilter
  ( PatchFilter (..)
  -- * Smart constructor
  , mkPatchFilter
  -- * Lenses
  , pfKey
  , pfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchFilterKey as Types
import qualified Network.AWS.SSM.Types.PatchFilterValue as Types

-- | Defines which patches should be included in a patch baseline.
--
-- A patch filter consists of a key and a set of values. The filter key is a patch property. For example, the available filter keys for WINDOWS are PATCH_SET, PRODUCT, PRODUCT_FAMILY, CLASSIFICATION, and MSRC_SEVERITY. The filter values define a matching criterion for the patch property indicated by the key. For example, if the filter key is PRODUCT and the filter values are ["Office 2013", "Office 2016"], then the filter accepts all patches where product name is either "Office 2013" or "Office 2016". The filter values can be exact values for the patch property given as a key, or a wildcard (*), which matches all values.
-- You can view lists of valid values for the patch properties by running the @DescribePatchProperties@ command. For information about which patch properties can be used with each major operating system, see 'DescribePatchProperties' .
--
-- /See:/ 'mkPatchFilter' smart constructor.
data PatchFilter = PatchFilter'
  { key :: Types.PatchFilterKey
    -- ^ The key for the filter.
--
-- Run the 'DescribePatchProperties' command to view lists of valid keys for each operating system type.
  , values :: Core.NonEmpty Types.PatchFilterValue
    -- ^ The value for the filter key.
--
-- Run the 'DescribePatchProperties' command to view lists of valid values for each key based on operating system type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchFilter' value with any optional fields omitted.
mkPatchFilter
    :: Types.PatchFilterKey -- ^ 'key'
    -> Core.NonEmpty Types.PatchFilterValue -- ^ 'values'
    -> PatchFilter
mkPatchFilter key values = PatchFilter'{key, values}

-- | The key for the filter.
--
-- Run the 'DescribePatchProperties' command to view lists of valid keys for each operating system type.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfKey :: Lens.Lens' PatchFilter Types.PatchFilterKey
pfKey = Lens.field @"key"
{-# INLINEABLE pfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value for the filter key.
--
-- Run the 'DescribePatchProperties' command to view lists of valid values for each key based on operating system type.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfValues :: Lens.Lens' PatchFilter (Core.NonEmpty Types.PatchFilterValue)
pfValues = Lens.field @"values"
{-# INLINEABLE pfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON PatchFilter where
        toJSON PatchFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values)])

instance Core.FromJSON PatchFilter where
        parseJSON
          = Core.withObject "PatchFilter" Core.$
              \ x ->
                PatchFilter' Core.<$> (x Core..: "Key") Core.<*> x Core..: "Values"
