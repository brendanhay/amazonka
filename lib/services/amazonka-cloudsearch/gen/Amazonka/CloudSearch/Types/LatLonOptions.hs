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
-- Module      : Amazonka.CloudSearch.Types.LatLonOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.LatLonOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for a latlon field. A latlon field contains a location stored as
-- a latitude and longitude value pair. Present if @IndexFieldType@
-- specifies the field is of type @latlon@. All options are enabled by
-- default.
--
-- /See:/ 'newLatLonOptions' smart constructor.
data LatLonOptions = LatLonOptions'
  { -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    sourceField :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LatLonOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'latLonOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'facetEnabled', 'latLonOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'latLonOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'latLonOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sortEnabled', 'latLonOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'sourceField', 'latLonOptions_sourceField' - Undocumented member.
newLatLonOptions ::
  LatLonOptions
newLatLonOptions =
  LatLonOptions'
    { defaultValue = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sortEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document.
latLonOptions_defaultValue :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Text)
latLonOptions_defaultValue = Lens.lens (\LatLonOptions' {defaultValue} -> defaultValue) (\s@LatLonOptions' {} a -> s {defaultValue = a} :: LatLonOptions)

-- | Whether facet information can be returned for the field.
latLonOptions_facetEnabled :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Bool)
latLonOptions_facetEnabled = Lens.lens (\LatLonOptions' {facetEnabled} -> facetEnabled) (\s@LatLonOptions' {} a -> s {facetEnabled = a} :: LatLonOptions)

-- | Whether the contents of the field can be returned in the search results.
latLonOptions_returnEnabled :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Bool)
latLonOptions_returnEnabled = Lens.lens (\LatLonOptions' {returnEnabled} -> returnEnabled) (\s@LatLonOptions' {} a -> s {returnEnabled = a} :: LatLonOptions)

-- | Whether the contents of the field are searchable.
latLonOptions_searchEnabled :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Bool)
latLonOptions_searchEnabled = Lens.lens (\LatLonOptions' {searchEnabled} -> searchEnabled) (\s@LatLonOptions' {} a -> s {searchEnabled = a} :: LatLonOptions)

-- | Whether the field can be used to sort the search results.
latLonOptions_sortEnabled :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Bool)
latLonOptions_sortEnabled = Lens.lens (\LatLonOptions' {sortEnabled} -> sortEnabled) (\s@LatLonOptions' {} a -> s {sortEnabled = a} :: LatLonOptions)

-- | Undocumented member.
latLonOptions_sourceField :: Lens.Lens' LatLonOptions (Prelude.Maybe Prelude.Text)
latLonOptions_sourceField = Lens.lens (\LatLonOptions' {sourceField} -> sourceField) (\s@LatLonOptions' {} a -> s {sourceField = a} :: LatLonOptions)

instance Data.FromXML LatLonOptions where
  parseXML x =
    LatLonOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SortEnabled")
      Prelude.<*> (x Data..@? "SourceField")

instance Prelude.Hashable LatLonOptions where
  hashWithSalt _salt LatLonOptions' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sortEnabled
      `Prelude.hashWithSalt` sourceField

instance Prelude.NFData LatLonOptions where
  rnf LatLonOptions' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sortEnabled
      `Prelude.seq` Prelude.rnf sourceField

instance Data.ToQuery LatLonOptions where
  toQuery LatLonOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SortEnabled" Data.=: sortEnabled,
        "SourceField" Data.=: sourceField
      ]
