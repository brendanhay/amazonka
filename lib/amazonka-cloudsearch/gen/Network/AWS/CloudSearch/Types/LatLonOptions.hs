{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LatLonOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.LatLonOptions
  ( LatLonOptions (..)
  -- * Smart constructor
  , mkLatLonOptions
  -- * Lenses
  , lloDefaultValue
  , lloFacetEnabled
  , lloReturnEnabled
  , lloSearchEnabled
  , lloSortEnabled
  , lloSourceField
  ) where

import qualified Network.AWS.CloudSearch.Types.FieldValue as Types
import qualified Network.AWS.CloudSearch.Types.SourceField as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a latlon field. A latlon field contains a location stored as a latitude and longitude value pair. Present if @IndexFieldType@ specifies the field is of type @latlon@ . All options are enabled by default.
--
-- /See:/ 'mkLatLonOptions' smart constructor.
data LatLonOptions = LatLonOptions'
  { defaultValue :: Core.Maybe Types.FieldValue
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

-- | Creates a 'LatLonOptions' value with any optional fields omitted.
mkLatLonOptions
    :: LatLonOptions
mkLatLonOptions
  = LatLonOptions'{defaultValue = Core.Nothing,
                   facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                   searchEnabled = Core.Nothing, sortEnabled = Core.Nothing,
                   sourceField = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloDefaultValue :: Lens.Lens' LatLonOptions (Core.Maybe Types.FieldValue)
lloDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE lloDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloFacetEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
lloFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE lloFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloReturnEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
lloReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE lloReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloSearchEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
lloSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE lloSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloSortEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
lloSortEnabled = Lens.field @"sortEnabled"
{-# INLINEABLE lloSortEnabled #-}
{-# DEPRECATED sortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloSourceField :: Lens.Lens' LatLonOptions (Core.Maybe Types.SourceField)
lloSourceField = Lens.field @"sourceField"
{-# INLINEABLE lloSourceField #-}
{-# DEPRECATED sourceField "Use generic-lens or generic-optics with 'sourceField' instead"  #-}

instance Core.ToQuery LatLonOptions where
        toQuery LatLonOptions{..}
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

instance Core.FromXML LatLonOptions where
        parseXML x
          = LatLonOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SortEnabled"
                Core.<*> x Core..@? "SourceField"
