{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralArrayOptions
  ( LiteralArrayOptions (..),

    -- * Smart constructor
    mkLiteralArrayOptions,

    -- * Lenses
    laoDefaultValue,
    laoFacetEnabled,
    laoReturnEnabled,
    laoSearchEnabled,
    laoSourceFields,
  )
where

import qualified Network.AWS.CloudSearch.Types.FieldNameCommaList as Types
import qualified Network.AWS.CloudSearch.Types.FieldValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options for a field that contains an array of literal strings. Present if @IndexFieldType@ specifies the field is of type @literal-array@ . All options are enabled by default.
--
-- /See:/ 'mkLiteralArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
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

-- | Creates a 'LiteralArrayOptions' value with any optional fields omitted.
mkLiteralArrayOptions ::
  LiteralArrayOptions
mkLiteralArrayOptions =
  LiteralArrayOptions'
    { defaultValue = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      sourceFields = Core.Nothing
    }

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoDefaultValue :: Lens.Lens' LiteralArrayOptions (Core.Maybe Types.FieldValue)
laoDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED laoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoFacetEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
laoFacetEnabled = Lens.field @"facetEnabled"
{-# DEPRECATED laoFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoReturnEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
laoReturnEnabled = Lens.field @"returnEnabled"
{-# DEPRECATED laoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoSearchEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
laoSearchEnabled = Lens.field @"searchEnabled"
{-# DEPRECATED laoSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoSourceFields :: Lens.Lens' LiteralArrayOptions (Core.Maybe Types.FieldNameCommaList)
laoSourceFields = Lens.field @"sourceFields"
{-# DEPRECATED laoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

instance Core.FromXML LiteralArrayOptions where
  parseXML x =
    LiteralArrayOptions'
      Core.<$> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "SourceFields")
