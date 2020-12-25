{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntArrayOptions
  ( IntArrayOptions (..),

    -- * Smart constructor
    mkIntArrayOptions,

    -- * Lenses
    iaoDefaultValue,
    iaoFacetEnabled,
    iaoReturnEnabled,
    iaoSearchEnabled,
    iaoSourceFields,
  )
where

import qualified Network.AWS.CloudSearch.Types.FieldNameCommaList as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of 64-bit signed integers. Present if @IndexFieldType@ specifies the field is of type @int-array@ . All options are enabled by default.
--
-- /See:/ 'mkIntArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
  { -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Core.Maybe Core.Integer,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | A list of source fields to map to the field.
    sourceFields :: Core.Maybe Types.FieldNameCommaList
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntArrayOptions' value with any optional fields omitted.
mkIntArrayOptions ::
  IntArrayOptions
mkIntArrayOptions =
  IntArrayOptions'
    { defaultValue = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      sourceFields = Core.Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDefaultValue :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Integer)
iaoDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED iaoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoFacetEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoFacetEnabled = Lens.field @"facetEnabled"
{-# DEPRECATED iaoFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoReturnEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoReturnEnabled = Lens.field @"returnEnabled"
{-# DEPRECATED iaoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSearchEnabled :: Lens.Lens' IntArrayOptions (Core.Maybe Core.Bool)
iaoSearchEnabled = Lens.field @"searchEnabled"
{-# DEPRECATED iaoSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSourceFields :: Lens.Lens' IntArrayOptions (Core.Maybe Types.FieldNameCommaList)
iaoSourceFields = Lens.field @"sourceFields"
{-# DEPRECATED iaoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

instance Core.FromXML IntArrayOptions where
  parseXML x =
    IntArrayOptions'
      Core.<$> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "SourceFields")
