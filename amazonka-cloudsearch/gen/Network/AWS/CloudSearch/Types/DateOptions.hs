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
-- Module      : Network.AWS.CloudSearch.Types.DateOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a date field. Dates and times are specified in UTC
-- (Coordinated Universal Time) according to IETF RFC3339:
-- yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is
-- of type @date@. All options are enabled by default.
--
-- /See:/ 'newDateOptions' smart constructor.
data DateOptions = DateOptions'
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
-- Create a value of 'DateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'dateOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'facetEnabled', 'dateOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'dateOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'dateOptions_sourceField' - Undocumented member.
--
-- 'searchEnabled', 'dateOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'dateOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newDateOptions ::
  DateOptions
newDateOptions =
  DateOptions'
    { sortEnabled = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceField = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | Whether the field can be used to sort the search results.
dateOptions_sortEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
dateOptions_sortEnabled = Lens.lens (\DateOptions' {sortEnabled} -> sortEnabled) (\s@DateOptions' {} a -> s {sortEnabled = a} :: DateOptions)

-- | Whether facet information can be returned for the field.
dateOptions_facetEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
dateOptions_facetEnabled = Lens.lens (\DateOptions' {facetEnabled} -> facetEnabled) (\s@DateOptions' {} a -> s {facetEnabled = a} :: DateOptions)

-- | Whether the contents of the field can be returned in the search results.
dateOptions_returnEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
dateOptions_returnEnabled = Lens.lens (\DateOptions' {returnEnabled} -> returnEnabled) (\s@DateOptions' {} a -> s {returnEnabled = a} :: DateOptions)

-- | Undocumented member.
dateOptions_sourceField :: Lens.Lens' DateOptions (Core.Maybe Core.Text)
dateOptions_sourceField = Lens.lens (\DateOptions' {sourceField} -> sourceField) (\s@DateOptions' {} a -> s {sourceField = a} :: DateOptions)

-- | Whether the contents of the field are searchable.
dateOptions_searchEnabled :: Lens.Lens' DateOptions (Core.Maybe Core.Bool)
dateOptions_searchEnabled = Lens.lens (\DateOptions' {searchEnabled} -> searchEnabled) (\s@DateOptions' {} a -> s {searchEnabled = a} :: DateOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateOptions_defaultValue :: Lens.Lens' DateOptions (Core.Maybe Core.Text)
dateOptions_defaultValue = Lens.lens (\DateOptions' {defaultValue} -> defaultValue) (\s@DateOptions' {} a -> s {defaultValue = a} :: DateOptions)

instance Core.FromXML DateOptions where
  parseXML x =
    DateOptions'
      Core.<$> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceField")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable DateOptions

instance Core.NFData DateOptions

instance Core.ToQuery DateOptions where
  toQuery DateOptions' {..} =
    Core.mconcat
      [ "SortEnabled" Core.=: sortEnabled,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SourceField" Core.=: sourceField,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
