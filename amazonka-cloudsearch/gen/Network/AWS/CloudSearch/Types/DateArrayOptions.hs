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
-- Module      : Network.AWS.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateArrayOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a field that contains an array of dates. Present if
-- @IndexFieldType@ specifies the field is of type @date-array@. All
-- options are enabled by default.
--
-- /See:/ 'newDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Core.Maybe Core.Text,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DateArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'dateArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'facetEnabled', 'dateArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'dateArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'dateArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'dateArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newDateArrayOptions ::
  DateArrayOptions
newDateArrayOptions =
  DateArrayOptions'
    { sourceFields = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | A list of source fields to map to the field.
dateArrayOptions_sourceFields :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Text)
dateArrayOptions_sourceFields = Lens.lens (\DateArrayOptions' {sourceFields} -> sourceFields) (\s@DateArrayOptions' {} a -> s {sourceFields = a} :: DateArrayOptions)

-- | Whether facet information can be returned for the field.
dateArrayOptions_facetEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dateArrayOptions_facetEnabled = Lens.lens (\DateArrayOptions' {facetEnabled} -> facetEnabled) (\s@DateArrayOptions' {} a -> s {facetEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
dateArrayOptions_returnEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dateArrayOptions_returnEnabled = Lens.lens (\DateArrayOptions' {returnEnabled} -> returnEnabled) (\s@DateArrayOptions' {} a -> s {returnEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field are searchable.
dateArrayOptions_searchEnabled :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Bool)
dateArrayOptions_searchEnabled = Lens.lens (\DateArrayOptions' {searchEnabled} -> searchEnabled) (\s@DateArrayOptions' {} a -> s {searchEnabled = a} :: DateArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateArrayOptions_defaultValue :: Lens.Lens' DateArrayOptions (Core.Maybe Core.Text)
dateArrayOptions_defaultValue = Lens.lens (\DateArrayOptions' {defaultValue} -> defaultValue) (\s@DateArrayOptions' {} a -> s {defaultValue = a} :: DateArrayOptions)

instance Core.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Core.<$> (x Core..@? "SourceFields")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable DateArrayOptions

instance Core.NFData DateArrayOptions

instance Core.ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    Core.mconcat
      [ "SourceFields" Core.=: sourceFields,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
