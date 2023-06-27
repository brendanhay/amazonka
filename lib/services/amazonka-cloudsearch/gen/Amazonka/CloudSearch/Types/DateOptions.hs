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
-- Module      : Amazonka.CloudSearch.Types.DateOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DateOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for a date field. Dates and times are specified in UTC
-- (Coordinated Universal Time) according to IETF RFC3339:
-- yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is
-- of type @date@. All options are enabled by default.
--
-- /See:/ 'newDateOptions' smart constructor.
data DateOptions = DateOptions'
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
-- Create a value of 'DateOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'dateOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'facetEnabled', 'dateOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'dateOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'dateOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sortEnabled', 'dateOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'sourceField', 'dateOptions_sourceField' - Undocumented member.
newDateOptions ::
  DateOptions
newDateOptions =
  DateOptions'
    { defaultValue = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sortEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateOptions_defaultValue :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Text)
dateOptions_defaultValue = Lens.lens (\DateOptions' {defaultValue} -> defaultValue) (\s@DateOptions' {} a -> s {defaultValue = a} :: DateOptions)

-- | Whether facet information can be returned for the field.
dateOptions_facetEnabled :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Bool)
dateOptions_facetEnabled = Lens.lens (\DateOptions' {facetEnabled} -> facetEnabled) (\s@DateOptions' {} a -> s {facetEnabled = a} :: DateOptions)

-- | Whether the contents of the field can be returned in the search results.
dateOptions_returnEnabled :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Bool)
dateOptions_returnEnabled = Lens.lens (\DateOptions' {returnEnabled} -> returnEnabled) (\s@DateOptions' {} a -> s {returnEnabled = a} :: DateOptions)

-- | Whether the contents of the field are searchable.
dateOptions_searchEnabled :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Bool)
dateOptions_searchEnabled = Lens.lens (\DateOptions' {searchEnabled} -> searchEnabled) (\s@DateOptions' {} a -> s {searchEnabled = a} :: DateOptions)

-- | Whether the field can be used to sort the search results.
dateOptions_sortEnabled :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Bool)
dateOptions_sortEnabled = Lens.lens (\DateOptions' {sortEnabled} -> sortEnabled) (\s@DateOptions' {} a -> s {sortEnabled = a} :: DateOptions)

-- | Undocumented member.
dateOptions_sourceField :: Lens.Lens' DateOptions (Prelude.Maybe Prelude.Text)
dateOptions_sourceField = Lens.lens (\DateOptions' {sourceField} -> sourceField) (\s@DateOptions' {} a -> s {sourceField = a} :: DateOptions)

instance Data.FromXML DateOptions where
  parseXML x =
    DateOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SortEnabled")
      Prelude.<*> (x Data..@? "SourceField")

instance Prelude.Hashable DateOptions where
  hashWithSalt _salt DateOptions' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sortEnabled
      `Prelude.hashWithSalt` sourceField

instance Prelude.NFData DateOptions where
  rnf DateOptions' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sortEnabled
      `Prelude.seq` Prelude.rnf sourceField

instance Data.ToQuery DateOptions where
  toQuery DateOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SortEnabled" Data.=: sortEnabled,
        "SourceField" Data.=: sourceField
      ]
