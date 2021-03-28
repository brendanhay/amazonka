{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.LiteralOptions
  ( LiteralOptions (..)
  -- * Smart constructor
  , mkLiteralOptions
  -- * Lenses
  , loDefaultValue
  , loFacetEnabled
  , loReturnEnabled
  , loSearchEnabled
  , loSortEnabled
  , loSourceField
  ) where

import qualified Network.AWS.CloudSearch.Types.FieldName as Types
import qualified Network.AWS.CloudSearch.Types.FieldValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for literal field. Present if @IndexFieldType@ specifies the field is of type @literal@ . All options are enabled by default.
--
-- /See:/ 'mkLiteralOptions' smart constructor.
data LiteralOptions = LiteralOptions'
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
  , sourceField :: Core.Maybe Types.FieldName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LiteralOptions' value with any optional fields omitted.
mkLiteralOptions
    :: LiteralOptions
mkLiteralOptions
  = LiteralOptions'{defaultValue = Core.Nothing,
                    facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                    searchEnabled = Core.Nothing, sortEnabled = Core.Nothing,
                    sourceField = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDefaultValue :: Lens.Lens' LiteralOptions (Core.Maybe Types.FieldValue)
loDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE loDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loFacetEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE loFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loReturnEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE loReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSearchEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE loSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSortEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loSortEnabled = Lens.field @"sortEnabled"
{-# INLINEABLE loSortEnabled #-}
{-# DEPRECATED sortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSourceField :: Lens.Lens' LiteralOptions (Core.Maybe Types.FieldName)
loSourceField = Lens.field @"sourceField"
{-# INLINEABLE loSourceField #-}
{-# DEPRECATED sourceField "Use generic-lens or generic-optics with 'sourceField' instead"  #-}

instance Core.ToQuery LiteralOptions where
        toQuery LiteralOptions{..}
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

instance Core.FromXML LiteralOptions where
        parseXML x
          = LiteralOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SortEnabled"
                Core.<*> x Core..@? "SourceField"
