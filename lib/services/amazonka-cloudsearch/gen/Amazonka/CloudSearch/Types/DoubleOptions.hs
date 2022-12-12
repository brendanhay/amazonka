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
-- Module      : Amazonka.CloudSearch.Types.DoubleOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DoubleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for a double-precision 64-bit floating point field. Present if
-- @IndexFieldType@ specifies the field is of type @double@. All options
-- are enabled by default.
--
-- /See:/ 'newDoubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { -- | A value to use for the field if the field isn\'t specified for a
    -- document. This can be important if you are using the field in an
    -- expression and that field is not present in every document.
    defaultValue :: Prelude.Maybe Prelude.Double,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source field to map to the field.
    sourceField :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DoubleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'doubleOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
--
-- 'facetEnabled', 'doubleOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'doubleOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'doubleOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sortEnabled', 'doubleOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'sourceField', 'doubleOptions_sourceField' - The name of the source field to map to the field.
newDoubleOptions ::
  DoubleOptions
newDoubleOptions =
  DoubleOptions'
    { defaultValue = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sortEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
doubleOptions_defaultValue :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Double)
doubleOptions_defaultValue = Lens.lens (\DoubleOptions' {defaultValue} -> defaultValue) (\s@DoubleOptions' {} a -> s {defaultValue = a} :: DoubleOptions)

-- | Whether facet information can be returned for the field.
doubleOptions_facetEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_facetEnabled = Lens.lens (\DoubleOptions' {facetEnabled} -> facetEnabled) (\s@DoubleOptions' {} a -> s {facetEnabled = a} :: DoubleOptions)

-- | Whether the contents of the field can be returned in the search results.
doubleOptions_returnEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_returnEnabled = Lens.lens (\DoubleOptions' {returnEnabled} -> returnEnabled) (\s@DoubleOptions' {} a -> s {returnEnabled = a} :: DoubleOptions)

-- | Whether the contents of the field are searchable.
doubleOptions_searchEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_searchEnabled = Lens.lens (\DoubleOptions' {searchEnabled} -> searchEnabled) (\s@DoubleOptions' {} a -> s {searchEnabled = a} :: DoubleOptions)

-- | Whether the field can be used to sort the search results.
doubleOptions_sortEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_sortEnabled = Lens.lens (\DoubleOptions' {sortEnabled} -> sortEnabled) (\s@DoubleOptions' {} a -> s {sortEnabled = a} :: DoubleOptions)

-- | The name of the source field to map to the field.
doubleOptions_sourceField :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Text)
doubleOptions_sourceField = Lens.lens (\DoubleOptions' {sourceField} -> sourceField) (\s@DoubleOptions' {} a -> s {sourceField = a} :: DoubleOptions)

instance Data.FromXML DoubleOptions where
  parseXML x =
    DoubleOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SortEnabled")
      Prelude.<*> (x Data..@? "SourceField")

instance Prelude.Hashable DoubleOptions where
  hashWithSalt _salt DoubleOptions' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sortEnabled
      `Prelude.hashWithSalt` sourceField

instance Prelude.NFData DoubleOptions where
  rnf DoubleOptions' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sortEnabled
      `Prelude.seq` Prelude.rnf sourceField

instance Data.ToQuery DoubleOptions where
  toQuery DoubleOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SortEnabled" Data.=: sortEnabled,
        "SourceField" Data.=: sourceField
      ]
