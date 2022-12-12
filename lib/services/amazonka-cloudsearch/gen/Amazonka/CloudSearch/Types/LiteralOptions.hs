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
-- Module      : Amazonka.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.LiteralOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for literal field. Present if @IndexFieldType@ specifies the
-- field is of type @literal@. All options are enabled by default.
--
-- /See:/ 'newLiteralOptions' smart constructor.
data LiteralOptions = LiteralOptions'
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
-- Create a value of 'LiteralOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'literalOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'facetEnabled', 'literalOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'literalOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'literalOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sortEnabled', 'literalOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'sourceField', 'literalOptions_sourceField' - Undocumented member.
newLiteralOptions ::
  LiteralOptions
newLiteralOptions =
  LiteralOptions'
    { defaultValue = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sortEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document.
literalOptions_defaultValue :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Text)
literalOptions_defaultValue = Lens.lens (\LiteralOptions' {defaultValue} -> defaultValue) (\s@LiteralOptions' {} a -> s {defaultValue = a} :: LiteralOptions)

-- | Whether facet information can be returned for the field.
literalOptions_facetEnabled :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Bool)
literalOptions_facetEnabled = Lens.lens (\LiteralOptions' {facetEnabled} -> facetEnabled) (\s@LiteralOptions' {} a -> s {facetEnabled = a} :: LiteralOptions)

-- | Whether the contents of the field can be returned in the search results.
literalOptions_returnEnabled :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Bool)
literalOptions_returnEnabled = Lens.lens (\LiteralOptions' {returnEnabled} -> returnEnabled) (\s@LiteralOptions' {} a -> s {returnEnabled = a} :: LiteralOptions)

-- | Whether the contents of the field are searchable.
literalOptions_searchEnabled :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Bool)
literalOptions_searchEnabled = Lens.lens (\LiteralOptions' {searchEnabled} -> searchEnabled) (\s@LiteralOptions' {} a -> s {searchEnabled = a} :: LiteralOptions)

-- | Whether the field can be used to sort the search results.
literalOptions_sortEnabled :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Bool)
literalOptions_sortEnabled = Lens.lens (\LiteralOptions' {sortEnabled} -> sortEnabled) (\s@LiteralOptions' {} a -> s {sortEnabled = a} :: LiteralOptions)

-- | Undocumented member.
literalOptions_sourceField :: Lens.Lens' LiteralOptions (Prelude.Maybe Prelude.Text)
literalOptions_sourceField = Lens.lens (\LiteralOptions' {sourceField} -> sourceField) (\s@LiteralOptions' {} a -> s {sourceField = a} :: LiteralOptions)

instance Data.FromXML LiteralOptions where
  parseXML x =
    LiteralOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SortEnabled")
      Prelude.<*> (x Data..@? "SourceField")

instance Prelude.Hashable LiteralOptions where
  hashWithSalt _salt LiteralOptions' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sortEnabled
      `Prelude.hashWithSalt` sourceField

instance Prelude.NFData LiteralOptions where
  rnf LiteralOptions' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf facetEnabled
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf searchEnabled
      `Prelude.seq` Prelude.rnf sortEnabled
      `Prelude.seq` Prelude.rnf sourceField

instance Data.ToQuery LiteralOptions where
  toQuery LiteralOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SortEnabled" Data.=: sortEnabled,
        "SourceField" Data.=: sourceField
      ]
