{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudSearch.Types.DoubleOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DoubleOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for a double-precision 64-bit floating point field. Present if
-- @IndexFieldType@ specifies the field is of type @double@. All options
-- are enabled by default.
--
-- /See:/ 'newDoubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source field to map to the field.
    sourceField :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document. This can be important if you are using the field in an
    -- expression and that field is not present in every document.
    defaultValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DoubleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'doubleOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'facetEnabled', 'doubleOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'doubleOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'doubleOptions_sourceField' - The name of the source field to map to the field.
--
-- 'searchEnabled', 'doubleOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'doubleOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
newDoubleOptions ::
  DoubleOptions
newDoubleOptions =
  DoubleOptions'
    { sortEnabled = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | Whether the field can be used to sort the search results.
doubleOptions_sortEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_sortEnabled = Lens.lens (\DoubleOptions' {sortEnabled} -> sortEnabled) (\s@DoubleOptions' {} a -> s {sortEnabled = a} :: DoubleOptions)

-- | Whether facet information can be returned for the field.
doubleOptions_facetEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_facetEnabled = Lens.lens (\DoubleOptions' {facetEnabled} -> facetEnabled) (\s@DoubleOptions' {} a -> s {facetEnabled = a} :: DoubleOptions)

-- | Whether the contents of the field can be returned in the search results.
doubleOptions_returnEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_returnEnabled = Lens.lens (\DoubleOptions' {returnEnabled} -> returnEnabled) (\s@DoubleOptions' {} a -> s {returnEnabled = a} :: DoubleOptions)

-- | The name of the source field to map to the field.
doubleOptions_sourceField :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Text)
doubleOptions_sourceField = Lens.lens (\DoubleOptions' {sourceField} -> sourceField) (\s@DoubleOptions' {} a -> s {sourceField = a} :: DoubleOptions)

-- | Whether the contents of the field are searchable.
doubleOptions_searchEnabled :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Bool)
doubleOptions_searchEnabled = Lens.lens (\DoubleOptions' {searchEnabled} -> searchEnabled) (\s@DoubleOptions' {} a -> s {searchEnabled = a} :: DoubleOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
doubleOptions_defaultValue :: Lens.Lens' DoubleOptions (Prelude.Maybe Prelude.Double)
doubleOptions_defaultValue = Lens.lens (\DoubleOptions' {defaultValue} -> defaultValue) (\s@DoubleOptions' {} a -> s {defaultValue = a} :: DoubleOptions)

instance Prelude.FromXML DoubleOptions where
  parseXML x =
    DoubleOptions'
      Prelude.<$> (x Prelude..@? "SortEnabled")
      Prelude.<*> (x Prelude..@? "FacetEnabled")
      Prelude.<*> (x Prelude..@? "ReturnEnabled")
      Prelude.<*> (x Prelude..@? "SourceField")
      Prelude.<*> (x Prelude..@? "SearchEnabled")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance Prelude.Hashable DoubleOptions

instance Prelude.NFData DoubleOptions

instance Prelude.ToQuery DoubleOptions where
  toQuery DoubleOptions' {..} =
    Prelude.mconcat
      [ "SortEnabled" Prelude.=: sortEnabled,
        "FacetEnabled" Prelude.=: facetEnabled,
        "ReturnEnabled" Prelude.=: returnEnabled,
        "SourceField" Prelude.=: sourceField,
        "SearchEnabled" Prelude.=: searchEnabled,
        "DefaultValue" Prelude.=: defaultValue
      ]
