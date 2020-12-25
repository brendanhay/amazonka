{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
  ( ListRecordHistorySearchFilter (..),

    -- * Smart constructor
    mkListRecordHistorySearchFilter,

    -- * Lenses
    lrhsfKey,
    lrhsfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.SearchFilterKey as Types
import qualified Network.AWS.ServiceCatalog.Types.SearchFilterValue as Types

-- | The search filter to use when listing history records.
--
-- /See:/ 'mkListRecordHistorySearchFilter' smart constructor.
data ListRecordHistorySearchFilter = ListRecordHistorySearchFilter'
  { -- | The filter key.
    --
    --
    --     * @product@ - Filter results based on the specified product identifier.
    --
    --
    --     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
    key :: Core.Maybe Types.SearchFilterKey,
    -- | The filter value.
    value :: Core.Maybe Types.SearchFilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRecordHistorySearchFilter' value with any optional fields omitted.
mkListRecordHistorySearchFilter ::
  ListRecordHistorySearchFilter
mkListRecordHistorySearchFilter =
  ListRecordHistorySearchFilter'
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The filter key.
--
--
--     * @product@ - Filter results based on the specified product identifier.
--
--
--     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
--
--
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhsfKey :: Lens.Lens' ListRecordHistorySearchFilter (Core.Maybe Types.SearchFilterKey)
lrhsfKey = Lens.field @"key"
{-# DEPRECATED lrhsfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhsfValue :: Lens.Lens' ListRecordHistorySearchFilter (Core.Maybe Types.SearchFilterValue)
lrhsfValue = Lens.field @"value"
{-# DEPRECATED lrhsfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ListRecordHistorySearchFilter where
  toJSON ListRecordHistorySearchFilter {..} =
    Core.object
      ( Core.catMaybes
          [("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value]
      )
