{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateArrayOptions
  ( DateArrayOptions (..),

    -- * Smart constructor
    mkDateArrayOptions,

    -- * Lenses
    dDefaultValue,
    dFacetEnabled,
    dReturnEnabled,
    dSearchEnabled,
    dSourceFields,
  )
where

import qualified Network.AWS.CloudSearch.Types.FieldNameCommaList as Types
import qualified Network.AWS.CloudSearch.Types.FieldValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of dates. Present if @IndexFieldType@ specifies the field is of type @date-array@ . All options are enabled by default.
--
-- /See:/ 'mkDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Core.Maybe Types.FieldValue,
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

-- | Creates a 'DateArrayOptions' value with any optional fields omitted.
mkDateArrayOptions ::
  DateArrayOptions
mkDateArrayOptions =
  DateArrayOptions'
    { defaultValue = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      sourceFields = Core.Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDefaultValue :: Lens.Lens' DateArrayOptions (Core.Maybe Types.FieldValue)
dDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED dDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFacetEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dFacetEnabled = Lens.field @"facetEnabled"
{-# DEPRECATED dFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReturnEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dReturnEnabled = Lens.field @"returnEnabled"
{-# DEPRECATED dReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSearchEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dSearchEnabled = Lens.field @"searchEnabled"
{-# DEPRECATED dSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSourceFields :: Lens.Lens' DateArrayOptions (Core.Maybe Types.FieldNameCommaList)
dSourceFields = Lens.field @"sourceFields"
{-# DEPRECATED dSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

instance Core.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Core.<$> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "SourceFields")
