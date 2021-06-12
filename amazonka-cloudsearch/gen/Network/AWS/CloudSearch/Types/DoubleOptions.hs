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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a double-precision 64-bit floating point field. Present if
-- @IndexFieldType@ specifies the field is of type @double@. All options
-- are enabled by default.
--
-- /See:/ 'newDoubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Core.Maybe Core.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | The name of the source field to map to the field.
    sourceField :: Core.Maybe Core.Text,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document. This can be important if you are using the field in an
    -- expression and that field is not present in every document.
    defaultValue :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { sortEnabled = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceField = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | Whether the field can be used to sort the search results.
doubleOptions_sortEnabled :: Lens.Lens' DoubleOptions (Core.Maybe Core.Bool)
doubleOptions_sortEnabled = Lens.lens (\DoubleOptions' {sortEnabled} -> sortEnabled) (\s@DoubleOptions' {} a -> s {sortEnabled = a} :: DoubleOptions)

-- | Whether facet information can be returned for the field.
doubleOptions_facetEnabled :: Lens.Lens' DoubleOptions (Core.Maybe Core.Bool)
doubleOptions_facetEnabled = Lens.lens (\DoubleOptions' {facetEnabled} -> facetEnabled) (\s@DoubleOptions' {} a -> s {facetEnabled = a} :: DoubleOptions)

-- | Whether the contents of the field can be returned in the search results.
doubleOptions_returnEnabled :: Lens.Lens' DoubleOptions (Core.Maybe Core.Bool)
doubleOptions_returnEnabled = Lens.lens (\DoubleOptions' {returnEnabled} -> returnEnabled) (\s@DoubleOptions' {} a -> s {returnEnabled = a} :: DoubleOptions)

-- | The name of the source field to map to the field.
doubleOptions_sourceField :: Lens.Lens' DoubleOptions (Core.Maybe Core.Text)
doubleOptions_sourceField = Lens.lens (\DoubleOptions' {sourceField} -> sourceField) (\s@DoubleOptions' {} a -> s {sourceField = a} :: DoubleOptions)

-- | Whether the contents of the field are searchable.
doubleOptions_searchEnabled :: Lens.Lens' DoubleOptions (Core.Maybe Core.Bool)
doubleOptions_searchEnabled = Lens.lens (\DoubleOptions' {searchEnabled} -> searchEnabled) (\s@DoubleOptions' {} a -> s {searchEnabled = a} :: DoubleOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
doubleOptions_defaultValue :: Lens.Lens' DoubleOptions (Core.Maybe Core.Double)
doubleOptions_defaultValue = Lens.lens (\DoubleOptions' {defaultValue} -> defaultValue) (\s@DoubleOptions' {} a -> s {defaultValue = a} :: DoubleOptions)

instance Core.FromXML DoubleOptions where
  parseXML x =
    DoubleOptions'
      Core.<$> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceField")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable DoubleOptions

instance Core.NFData DoubleOptions

instance Core.ToQuery DoubleOptions where
  toQuery DoubleOptions' {..} =
    Core.mconcat
      [ "SortEnabled" Core.=: sortEnabled,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SourceField" Core.=: sourceField,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
