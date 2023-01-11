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
-- Module      : Amazonka.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DateArrayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for a field that contains an array of dates. Present if
-- @IndexFieldType@ specifies the field is of type @date-array@. All
-- options are enabled by default.
--
-- /See:/ 'newDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of source fields to map to the field.
    sourceFields :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'dateArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'facetEnabled', 'dateArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'dateArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'dateArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sourceFields', 'dateArrayOptions_sourceFields' - A list of source fields to map to the field.
newDateArrayOptions ::
  DateArrayOptions
newDateArrayOptions =
  DateArrayOptions'
    { defaultValue = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sourceFields = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateArrayOptions_defaultValue :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_defaultValue = Lens.lens (\DateArrayOptions' {defaultValue} -> defaultValue) (\s@DateArrayOptions' {} a -> s {defaultValue = a} :: DateArrayOptions)

-- | Whether facet information can be returned for the field.
dateArrayOptions_facetEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_facetEnabled = Lens.lens (\DateArrayOptions' {facetEnabled} -> facetEnabled) (\s@DateArrayOptions' {} a -> s {facetEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
dateArrayOptions_returnEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_returnEnabled = Lens.lens (\DateArrayOptions' {returnEnabled} -> returnEnabled) (\s@DateArrayOptions' {} a -> s {returnEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field are searchable.
dateArrayOptions_searchEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_searchEnabled = Lens.lens (\DateArrayOptions' {searchEnabled} -> searchEnabled) (\s@DateArrayOptions' {} a -> s {searchEnabled = a} :: DateArrayOptions)

-- | A list of source fields to map to the field.
dateArrayOptions_sourceFields :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_sourceFields = Lens.lens (\DateArrayOptions' {sourceFields} -> sourceFields) (\s@DateArrayOptions' {} a -> s {sourceFields = a} :: DateArrayOptions)

instance Data.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SourceFields")

instance Prelude.Hashable DateArrayOptions where
  hashWithSalt _salt DateArrayOptions' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sourceFields

instance Prelude.NFData DateArrayOptions where
  rnf DateArrayOptions' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sourceFields

instance Data.ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SourceFields" Data.=: sourceFields
      ]
