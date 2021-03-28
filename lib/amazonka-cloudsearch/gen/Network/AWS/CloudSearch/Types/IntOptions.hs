{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.IntOptions
  ( IntOptions (..)
  -- * Smart constructor
  , mkIntOptions
  -- * Lenses
  , ioDefaultValue
  , ioFacetEnabled
  , ioReturnEnabled
  , ioSearchEnabled
  , ioSortEnabled
  , ioSourceField
  ) where

import qualified Network.AWS.CloudSearch.Types.FieldName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@ specifies the field is of type @int@ . All options are enabled by default.
--
-- /See:/ 'mkIntOptions' smart constructor.
data IntOptions = IntOptions'
  { defaultValue :: Core.Maybe Core.Integer
    -- ^ A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
  , facetEnabled :: Core.Maybe Core.Bool
    -- ^ Whether facet information can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , searchEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field are searchable.
  , sortEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the field can be used to sort the search results.
  , sourceField :: Core.Maybe Types.FieldName
    -- ^ The name of the source field to map to the field. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntOptions' value with any optional fields omitted.
mkIntOptions
    :: IntOptions
mkIntOptions
  = IntOptions'{defaultValue = Core.Nothing,
                facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                searchEnabled = Core.Nothing, sortEnabled = Core.Nothing,
                sourceField = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioDefaultValue :: Lens.Lens' IntOptions (Core.Maybe Core.Integer)
ioDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE ioDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioFacetEnabled :: Lens.Lens' IntOptions (Core.Maybe Core.Bool)
ioFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE ioFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioReturnEnabled :: Lens.Lens' IntOptions (Core.Maybe Core.Bool)
ioReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE ioReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioSearchEnabled :: Lens.Lens' IntOptions (Core.Maybe Core.Bool)
ioSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE ioSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioSortEnabled :: Lens.Lens' IntOptions (Core.Maybe Core.Bool)
ioSortEnabled = Lens.field @"sortEnabled"
{-# INLINEABLE ioSortEnabled #-}
{-# DEPRECATED sortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead"  #-}

-- | The name of the source field to map to the field. 
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioSourceField :: Lens.Lens' IntOptions (Core.Maybe Types.FieldName)
ioSourceField = Lens.field @"sourceField"
{-# INLINEABLE ioSourceField #-}
{-# DEPRECATED sourceField "Use generic-lens or generic-optics with 'sourceField' instead"  #-}

instance Core.ToQuery IntOptions where
        toQuery IntOptions{..}
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

instance Core.FromXML IntOptions where
        parseXML x
          = IntOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SortEnabled"
                Core.<*> x Core..@? "SourceField"
