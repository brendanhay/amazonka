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
-- Module      : Amazonka.CloudSearch.Types.DoubleArrayOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DoubleArrayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options for a field that contains an array of double-precision 64-bit
-- floating point values. Present if @IndexFieldType@ specifies the field
-- is of type @double-array@. All options are enabled by default.
--
-- /See:/ 'newDoubleArrayOptions' smart constructor.
data DoubleArrayOptions = DoubleArrayOptions'
  { -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of source fields to map to the field.
    sourceFields :: Prelude.Maybe Prelude.Text,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Double,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DoubleArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetEnabled', 'doubleArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'searchEnabled', 'doubleArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sourceFields', 'doubleArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'defaultValue', 'doubleArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'returnEnabled', 'doubleArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
newDoubleArrayOptions ::
  DoubleArrayOptions
newDoubleArrayOptions =
  DoubleArrayOptions'
    { facetEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sourceFields = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      returnEnabled = Prelude.Nothing
    }

-- | Whether facet information can be returned for the field.
doubleArrayOptions_facetEnabled :: Lens.Lens' DoubleArrayOptions (Prelude.Maybe Prelude.Bool)
doubleArrayOptions_facetEnabled = Lens.lens (\DoubleArrayOptions' {facetEnabled} -> facetEnabled) (\s@DoubleArrayOptions' {} a -> s {facetEnabled = a} :: DoubleArrayOptions)

-- | Whether the contents of the field are searchable.
doubleArrayOptions_searchEnabled :: Lens.Lens' DoubleArrayOptions (Prelude.Maybe Prelude.Bool)
doubleArrayOptions_searchEnabled = Lens.lens (\DoubleArrayOptions' {searchEnabled} -> searchEnabled) (\s@DoubleArrayOptions' {} a -> s {searchEnabled = a} :: DoubleArrayOptions)

-- | A list of source fields to map to the field.
doubleArrayOptions_sourceFields :: Lens.Lens' DoubleArrayOptions (Prelude.Maybe Prelude.Text)
doubleArrayOptions_sourceFields = Lens.lens (\DoubleArrayOptions' {sourceFields} -> sourceFields) (\s@DoubleArrayOptions' {} a -> s {sourceFields = a} :: DoubleArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
doubleArrayOptions_defaultValue :: Lens.Lens' DoubleArrayOptions (Prelude.Maybe Prelude.Double)
doubleArrayOptions_defaultValue = Lens.lens (\DoubleArrayOptions' {defaultValue} -> defaultValue) (\s@DoubleArrayOptions' {} a -> s {defaultValue = a} :: DoubleArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
doubleArrayOptions_returnEnabled :: Lens.Lens' DoubleArrayOptions (Prelude.Maybe Prelude.Bool)
doubleArrayOptions_returnEnabled = Lens.lens (\DoubleArrayOptions' {returnEnabled} -> returnEnabled) (\s@DoubleArrayOptions' {} a -> s {returnEnabled = a} :: DoubleArrayOptions)

instance Core.FromXML DoubleArrayOptions where
  parseXML x =
    DoubleArrayOptions'
      Prelude.<$> (x Core..@? "FacetEnabled")
      Prelude.<*> (x Core..@? "SearchEnabled")
      Prelude.<*> (x Core..@? "SourceFields")
      Prelude.<*> (x Core..@? "DefaultValue")
      Prelude.<*> (x Core..@? "ReturnEnabled")

instance Prelude.Hashable DoubleArrayOptions where
  hashWithSalt _salt DoubleArrayOptions' {..} =
    _salt `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sourceFields
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` returnEnabled

instance Prelude.NFData DoubleArrayOptions where
  rnf DoubleArrayOptions' {..} =
    Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sourceFields
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf returnEnabled

instance Core.ToQuery DoubleArrayOptions where
  toQuery DoubleArrayOptions' {..} =
    Prelude.mconcat
      [ "FacetEnabled" Core.=: facetEnabled,
        "SearchEnabled" Core.=: searchEnabled,
        "SourceFields" Core.=: sourceFields,
        "DefaultValue" Core.=: defaultValue,
        "ReturnEnabled" Core.=: returnEnabled
      ]
