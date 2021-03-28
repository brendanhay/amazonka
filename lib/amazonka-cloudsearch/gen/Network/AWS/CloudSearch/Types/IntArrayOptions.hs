{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.IntArrayOptions
  ( IntArrayOptions (..)
  -- * Smart constructor
  , mkIntArrayOptions
  -- * Lenses
  , iaoDefaultValue
  , iaoFacetEnabled
  , iaoReturnEnabled
  , iaoSearchEnabled
  , iaoSourceFields
  ) where

import qualified Network.AWS.CloudSearch.Types.FieldNameCommaList as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of 64-bit signed integers. Present if @IndexFieldType@ specifies the field is of type @int-array@ . All options are enabled by default.
--
-- /See:/ 'mkIntArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
  { defaultValue :: Core.Maybe Core.Integer
    -- ^ A value to use for the field if the field isn't specified for a document.
  , facetEnabled :: Core.Maybe Core.Bool
    -- ^ Whether facet information can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , searchEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field are searchable.
  , sourceFields :: Core.Maybe Types.FieldNameCommaList
    -- ^ A list of source fields to map to the field. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntArrayOptions' value with any optional fields omitted.
mkIntArrayOptions
    :: IntArrayOptions
mkIntArrayOptions
  = IntArrayOptions'{defaultValue = Core.Nothing,
                     facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                     searchEnabled = Core.Nothing, sourceFields = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDefaultValue :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Integer)
iaoDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE iaoDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoFacetEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE iaoFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoReturnEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE iaoReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSearchEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE iaoSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | A list of source fields to map to the field. 
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSourceFields :: Lens.Lens' IntArrayOptions (Core.Maybe Types.FieldNameCommaList)
iaoSourceFields = Lens.field @"sourceFields"
{-# INLINEABLE iaoSourceFields #-}
{-# DEPRECATED sourceFields "Use generic-lens or generic-optics with 'sourceFields' instead"  #-}

instance Core.ToQuery IntArrayOptions where
        toQuery IntArrayOptions{..}
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
              Core.maybe Core.mempty (Core.toQueryPair "SourceFields")
                sourceFields

instance Core.FromXML IntArrayOptions where
        parseXML x
          = IntArrayOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SourceFields"
