{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.DateOptions
  ( DateOptions (..)
  -- * Smart constructor
  , mkDateOptions
  -- * Lenses
  , doDefaultValue
  , doFacetEnabled
  , doReturnEnabled
  , doSearchEnabled
  , doSortEnabled
  , doSourceField
  ) where

import qualified Network.AWS.CloudSearch.Types.DefaultValue as Types
import qualified Network.AWS.CloudSearch.Types.SourceField as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a date field. Dates and times are specified in UTC (Coordinated Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is of type @date@ . All options are enabled by default.
--
-- /See:/ 'mkDateOptions' smart constructor.
data DateOptions = DateOptions'
  { defaultValue :: Core.Maybe Types.DefaultValue
    -- ^ A value to use for the field if the field isn't specified for a document.
  , facetEnabled :: Core.Maybe Core.Bool
    -- ^ Whether facet information can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , searchEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field are searchable.
  , sortEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the field can be used to sort the search results.
  , sourceField :: Core.Maybe Types.SourceField
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DateOptions' value with any optional fields omitted.
mkDateOptions
    :: DateOptions
mkDateOptions
  = DateOptions'{defaultValue = Core.Nothing,
                 facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                 searchEnabled = Core.Nothing, sortEnabled = Core.Nothing,
                 sourceField = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDefaultValue :: Lens.Lens' DateOptions (Core.Maybe Types.DefaultValue)
doDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE doDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doFacetEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
doFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE doFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doReturnEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
doReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE doReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSearchEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
doSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE doSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSortEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
doSortEnabled = Lens.field @"sortEnabled"
{-# INLINEABLE doSortEnabled #-}
{-# DEPRECATED sortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSourceField :: Lens.Lens' DateOptions (Core.Maybe Types.SourceField)
doSourceField = Lens.field @"sourceField"
{-# INLINEABLE doSourceField #-}
{-# DEPRECATED sourceField "Use generic-lens or generic-optics with 'sourceField' instead"  #-}

instance Core.ToQuery DateOptions where
        toQuery DateOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "DefaultValue")
              defaultValue
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FacetEnabled")
                facetEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnEnabled")
                returnEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SearchEnabled")
                searchEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SortEnabled") sortEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceField") sourceField

instance Core.FromXML DateOptions where
        parseXML x
          = DateOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SortEnabled"
                Core.<*> x Core..@? "SourceField"
