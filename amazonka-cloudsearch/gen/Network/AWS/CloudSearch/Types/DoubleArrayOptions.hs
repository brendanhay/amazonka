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
-- Module      : Network.AWS.CloudSearch.Types.DoubleArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DoubleArrayOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if @IndexFieldType@ specifies the field
-- is of type @double-array@. All options are enabled by default.
--
-- /See:/ 'newDoubleArrayOptions' smart constructor.
data DoubleArrayOptions = DoubleArrayOptions'
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
    defaultValue :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DoubleArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'doubleArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'facetEnabled', 'doubleArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'doubleArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'doubleArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'doubleArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newDoubleArrayOptions ::
  DoubleArrayOptions
newDoubleArrayOptions =
  DoubleArrayOptions'
    { sourceFields = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | A list of source fields to map to the field.
doubleArrayOptions_sourceFields :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Text)
doubleArrayOptions_sourceFields = Lens.lens (\DoubleArrayOptions' {sourceFields} -> sourceFields) (\s@DoubleArrayOptions' {} a -> s {sourceFields = a} :: DoubleArrayOptions)

-- | Whether facet information can be returned for the field.
doubleArrayOptions_facetEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
doubleArrayOptions_facetEnabled = Lens.lens (\DoubleArrayOptions' {facetEnabled} -> facetEnabled) (\s@DoubleArrayOptions' {} a -> s {facetEnabled = a} :: DoubleArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
doubleArrayOptions_returnEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
doubleArrayOptions_returnEnabled = Lens.lens (\DoubleArrayOptions' {returnEnabled} -> returnEnabled) (\s@DoubleArrayOptions' {} a -> s {returnEnabled = a} :: DoubleArrayOptions)

-- | Whether the contents of the field are searchable.
doubleArrayOptions_searchEnabled :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Bool)
doubleArrayOptions_searchEnabled = Lens.lens (\DoubleArrayOptions' {searchEnabled} -> searchEnabled) (\s@DoubleArrayOptions' {} a -> s {searchEnabled = a} :: DoubleArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
doubleArrayOptions_defaultValue :: Lens.Lens' DoubleArrayOptions (Core.Maybe Core.Double)
doubleArrayOptions_defaultValue = Lens.lens (\DoubleArrayOptions' {defaultValue} -> defaultValue) (\s@DoubleArrayOptions' {} a -> s {defaultValue = a} :: DoubleArrayOptions)

instance Core.FromXML DoubleArrayOptions where
  parseXML x =
    DoubleArrayOptions'
      Core.<$> (x Core..@? "SourceFields")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable DoubleArrayOptions

instance Core.NFData DoubleArrayOptions

instance Core.ToQuery DoubleArrayOptions where
  toQuery DoubleArrayOptions' {..} =
    Core.mconcat
      [ "SourceFields" Core.=: sourceFields,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
