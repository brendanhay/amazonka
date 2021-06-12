{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for literal field. Present if @IndexFieldType@ specifies the
-- field is of type @literal@. All options are enabled by default.
--
-- /See:/ 'newLiteralOptions' smart constructor.
data LiteralOptions = LiteralOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Core.Maybe Core.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    sourceField :: Core.Maybe Core.Text,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LiteralOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'literalOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'facetEnabled', 'literalOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'literalOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'literalOptions_sourceField' - Undocumented member.
--
-- 'searchEnabled', 'literalOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'literalOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newLiteralOptions ::
  LiteralOptions
newLiteralOptions =
  LiteralOptions'
    { sortEnabled = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceField = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | Whether the field can be used to sort the search results.
literalOptions_sortEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
literalOptions_sortEnabled = Lens.lens (\LiteralOptions' {sortEnabled} -> sortEnabled) (\s@LiteralOptions' {} a -> s {sortEnabled = a} :: LiteralOptions)

-- | Whether facet information can be returned for the field.
literalOptions_facetEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
literalOptions_facetEnabled = Lens.lens (\LiteralOptions' {facetEnabled} -> facetEnabled) (\s@LiteralOptions' {} a -> s {facetEnabled = a} :: LiteralOptions)

-- | Whether the contents of the field can be returned in the search results.
literalOptions_returnEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
literalOptions_returnEnabled = Lens.lens (\LiteralOptions' {returnEnabled} -> returnEnabled) (\s@LiteralOptions' {} a -> s {returnEnabled = a} :: LiteralOptions)

-- | Undocumented member.
literalOptions_sourceField :: Lens.Lens' LiteralOptions (Core.Maybe Core.Text)
literalOptions_sourceField = Lens.lens (\LiteralOptions' {sourceField} -> sourceField) (\s@LiteralOptions' {} a -> s {sourceField = a} :: LiteralOptions)

-- | Whether the contents of the field are searchable.
literalOptions_searchEnabled :: Lens.Lens' LiteralOptions (Core.Maybe Core.Bool)
literalOptions_searchEnabled = Lens.lens (\LiteralOptions' {searchEnabled} -> searchEnabled) (\s@LiteralOptions' {} a -> s {searchEnabled = a} :: LiteralOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
literalOptions_defaultValue :: Lens.Lens' LiteralOptions (Core.Maybe Core.Text)
literalOptions_defaultValue = Lens.lens (\LiteralOptions' {defaultValue} -> defaultValue) (\s@LiteralOptions' {} a -> s {defaultValue = a} :: LiteralOptions)

instance Core.FromXML LiteralOptions where
  parseXML x =
    LiteralOptions'
      Core.<$> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceField")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable LiteralOptions

instance Core.NFData LiteralOptions

instance Core.ToQuery LiteralOptions where
  toQuery LiteralOptions' {..} =
    Core.mconcat
      [ "SortEnabled" Core.=: sortEnabled,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SourceField" Core.=: sourceField,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
