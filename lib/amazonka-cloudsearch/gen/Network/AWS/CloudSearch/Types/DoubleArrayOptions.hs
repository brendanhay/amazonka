{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DoubleArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.DoubleArrayOptions
  ( DoubleArrayOptions (..)
  -- * Smart constructor
  , mkDoubleArrayOptions
  -- * Lenses
  , daoDefaultValue
  , daoFacetEnabled
  , daoReturnEnabled
  , daoSearchEnabled
  , daoSourceFields
  ) where

import qualified Network.AWS.CloudSearch.Types.SourceFields as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of double-precision 64-bit floating point values. Present if @IndexFieldType@ specifies the field is of type @double-array@ . All options are enabled by default.
--
-- /See:/ 'mkDoubleArrayOptions' smart constructor.
data DoubleArrayOptions = DoubleArrayOptions'
  { defaultValue :: Core.Maybe Core.Double
    -- ^ A value to use for the field if the field isn't specified for a document.
  , facetEnabled :: Core.Maybe Core.Bool
    -- ^ Whether facet information can be returned for the field.
  , returnEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field can be returned in the search results.
  , searchEnabled :: Core.Maybe Core.Bool
    -- ^ Whether the contents of the field are searchable.
  , sourceFields :: Core.Maybe Types.SourceFields
    -- ^ A list of source fields to map to the field. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DoubleArrayOptions' value with any optional fields omitted.
mkDoubleArrayOptions
    :: DoubleArrayOptions
mkDoubleArrayOptions
  = DoubleArrayOptions'{defaultValue = Core.Nothing,
                        facetEnabled = Core.Nothing, returnEnabled = Core.Nothing,
                        searchEnabled = Core.Nothing, sourceFields = Core.Nothing}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDefaultValue :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Double)
daoDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE daoDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoFacetEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
daoFacetEnabled = Lens.field @"facetEnabled"
{-# INLINEABLE daoFacetEnabled #-}
{-# DEPRECATED facetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead"  #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoReturnEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
daoReturnEnabled = Lens.field @"returnEnabled"
{-# INLINEABLE daoReturnEnabled #-}
{-# DEPRECATED returnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead"  #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoSearchEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
daoSearchEnabled = Lens.field @"searchEnabled"
{-# INLINEABLE daoSearchEnabled #-}
{-# DEPRECATED searchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead"  #-}

-- | A list of source fields to map to the field. 
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoSourceFields :: Lens.Lens' DoubleArrayOptions (Core.Maybe Types.SourceFields)
daoSourceFields = Lens.field @"sourceFields"
{-# INLINEABLE daoSourceFields #-}
{-# DEPRECATED sourceFields "Use generic-lens or generic-optics with 'sourceFields' instead"  #-}

instance Core.ToQuery DoubleArrayOptions where
        toQuery DoubleArrayOptions{..}
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

instance Core.FromXML DoubleArrayOptions where
        parseXML x
          = DoubleArrayOptions' Core.<$>
              (x Core..@? "DefaultValue") Core.<*> x Core..@? "FacetEnabled"
                Core.<*> x Core..@? "ReturnEnabled"
                Core.<*> x Core..@? "SearchEnabled"
                Core.<*> x Core..@? "SourceFields"
