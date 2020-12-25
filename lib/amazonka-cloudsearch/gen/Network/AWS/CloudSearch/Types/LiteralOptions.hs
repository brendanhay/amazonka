{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralOptions
  ( LiteralOptions (..),

    -- * Smart constructor
    mkLiteralOptions,

    -- * Lenses
    loDefaultValue,
    loFacetEnabled,
    loReturnEnabled,
    loSearchEnabled,
    loSortEnabled,
    loSourceField,
  )
where

import qualified Network.AWS.CloudSearch.Types.FieldName as Types
import qualified Network.AWS.CloudSearch.Types.FieldValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for literal field. Present if @IndexFieldType@ specifies the field is of type @literal@ . All options are enabled by default.
--
-- /See:/ 'mkLiteralOptions' smart constructor.
data LiteralOptions = LiteralOptions'
  { -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Core.Maybe Types.FieldValue,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | Whether the field can be used to sort the search results.
    sortEnabled :: Core.Maybe Core.Bool,
    sourceField :: Core.Maybe Types.FieldName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LiteralOptions' value with any optional fields omitted.
mkLiteralOptions ::
  LiteralOptions
mkLiteralOptions =
  LiteralOptions'
    { defaultValue = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      sortEnabled = Core.Nothing,
      sourceField = Core.Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDefaultValue :: Lens.Lens' LiteralOptions (Core.Maybe Types.FieldValue)
loDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED loDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loFacetEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loFacetEnabled = Lens.field @"facetEnabled"
{-# DEPRECATED loFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loReturnEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loReturnEnabled = Lens.field @"returnEnabled"
{-# DEPRECATED loReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSearchEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loSearchEnabled = Lens.field @"searchEnabled"
{-# DEPRECATED loSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSortEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
loSortEnabled = Lens.field @"sortEnabled"
{-# DEPRECATED loSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSourceField :: Lens.Lens' LiteralOptions (Core.Maybe Types.FieldName)
loSourceField = Lens.field @"sourceField"
{-# DEPRECATED loSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

instance Core.FromXML LiteralOptions where
  parseXML x =
    LiteralOptions'
      Core.<$> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "SourceField")
