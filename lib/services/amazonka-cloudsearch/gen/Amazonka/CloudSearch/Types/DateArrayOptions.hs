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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DateArrayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Options for a field that contains an array of dates. Present if
-- @IndexFieldType@ specifies the field is of type @date-array@. All
-- options are enabled by default.
--
-- /See:/ 'newDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of source fields to map to the field.
    sourceFields :: Prelude.Maybe Prelude.Text,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'facetEnabled', 'dateArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'searchEnabled', 'dateArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sourceFields', 'dateArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'defaultValue', 'dateArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'returnEnabled', 'dateArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
newDateArrayOptions ::
  DateArrayOptions
newDateArrayOptions =
  DateArrayOptions'
    { facetEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sourceFields = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      returnEnabled = Prelude.Nothing
    }

-- | Whether facet information can be returned for the field.
dateArrayOptions_facetEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_facetEnabled = Lens.lens (\DateArrayOptions' {facetEnabled} -> facetEnabled) (\s@DateArrayOptions' {} a -> s {facetEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field are searchable.
dateArrayOptions_searchEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_searchEnabled = Lens.lens (\DateArrayOptions' {searchEnabled} -> searchEnabled) (\s@DateArrayOptions' {} a -> s {searchEnabled = a} :: DateArrayOptions)

-- | A list of source fields to map to the field.
dateArrayOptions_sourceFields :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_sourceFields = Lens.lens (\DateArrayOptions' {sourceFields} -> sourceFields) (\s@DateArrayOptions' {} a -> s {sourceFields = a} :: DateArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateArrayOptions_defaultValue :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_defaultValue = Lens.lens (\DateArrayOptions' {defaultValue} -> defaultValue) (\s@DateArrayOptions' {} a -> s {defaultValue = a} :: DateArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
dateArrayOptions_returnEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_returnEnabled = Lens.lens (\DateArrayOptions' {returnEnabled} -> returnEnabled) (\s@DateArrayOptions' {} a -> s {returnEnabled = a} :: DateArrayOptions)

instance Core.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Prelude.<$> (x Core..@? "FacetEnabled")
      Prelude.<*> (x Core..@? "SearchEnabled")
      Prelude.<*> (x Core..@? "SourceFields")
      Prelude.<*> (x Core..@? "DefaultValue")
      Prelude.<*> (x Core..@? "ReturnEnabled")

instance Prelude.Hashable DateArrayOptions where
  hashWithSalt _salt DateArrayOptions' {..} =
    _salt `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sourceFields
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` returnEnabled

instance Prelude.NFData DateArrayOptions where
  rnf DateArrayOptions' {..} =
    Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sourceFields
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf returnEnabled

instance Core.ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    Prelude.mconcat
      [ "FacetEnabled" Core.=: facetEnabled,
        "SearchEnabled" Core.=: searchEnabled,
        "SourceFields" Core.=: sourceFields,
        "DefaultValue" Core.=: defaultValue,
        "ReturnEnabled" Core.=: returnEnabled
      ]
