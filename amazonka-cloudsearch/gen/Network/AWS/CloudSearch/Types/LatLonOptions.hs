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
-- Module      : Network.AWS.CloudSearch.Types.LatLonOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LatLonOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a latlon field. A latlon field contains a location stored as
-- a latitude and longitude value pair. Present if @IndexFieldType@
-- specifies the field is of type @latlon@. All options are enabled by
-- default.
--
-- /See:/ 'newLatLonOptions' smart constructor.
data LatLonOptions = LatLonOptions'
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
-- Create a value of 'LatLonOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'latLonOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'facetEnabled', 'latLonOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'latLonOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'latLonOptions_sourceField' - Undocumented member.
--
-- 'searchEnabled', 'latLonOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'latLonOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newLatLonOptions ::
  LatLonOptions
newLatLonOptions =
  LatLonOptions'
    { sortEnabled = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceField = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | Whether the field can be used to sort the search results.
latLonOptions_sortEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
latLonOptions_sortEnabled = Lens.lens (\LatLonOptions' {sortEnabled} -> sortEnabled) (\s@LatLonOptions' {} a -> s {sortEnabled = a} :: LatLonOptions)

-- | Whether facet information can be returned for the field.
latLonOptions_facetEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
latLonOptions_facetEnabled = Lens.lens (\LatLonOptions' {facetEnabled} -> facetEnabled) (\s@LatLonOptions' {} a -> s {facetEnabled = a} :: LatLonOptions)

-- | Whether the contents of the field can be returned in the search results.
latLonOptions_returnEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
latLonOptions_returnEnabled = Lens.lens (\LatLonOptions' {returnEnabled} -> returnEnabled) (\s@LatLonOptions' {} a -> s {returnEnabled = a} :: LatLonOptions)

-- | Undocumented member.
latLonOptions_sourceField :: Lens.Lens' LatLonOptions (Core.Maybe Core.Text)
latLonOptions_sourceField = Lens.lens (\LatLonOptions' {sourceField} -> sourceField) (\s@LatLonOptions' {} a -> s {sourceField = a} :: LatLonOptions)

-- | Whether the contents of the field are searchable.
latLonOptions_searchEnabled :: Lens.Lens' LatLonOptions (Core.Maybe Core.Bool)
latLonOptions_searchEnabled = Lens.lens (\LatLonOptions' {searchEnabled} -> searchEnabled) (\s@LatLonOptions' {} a -> s {searchEnabled = a} :: LatLonOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
latLonOptions_defaultValue :: Lens.Lens' LatLonOptions (Core.Maybe Core.Text)
latLonOptions_defaultValue = Lens.lens (\LatLonOptions' {defaultValue} -> defaultValue) (\s@LatLonOptions' {} a -> s {defaultValue = a} :: LatLonOptions)

instance Core.FromXML LatLonOptions where
  parseXML x =
    LatLonOptions'
      Core.<$> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceField")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable LatLonOptions

instance Core.NFData LatLonOptions

instance Core.ToQuery LatLonOptions where
  toQuery LatLonOptions' {..} =
    Core.mconcat
      [ "SortEnabled" Core.=: sortEnabled,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SourceField" Core.=: sourceField,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
