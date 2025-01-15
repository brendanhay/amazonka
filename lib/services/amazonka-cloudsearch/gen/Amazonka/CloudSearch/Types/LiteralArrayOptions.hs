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
-- Module      : Amazonka.CloudSearch.Types.LiteralArrayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.LiteralArrayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for a field that contains an array of literal strings. Present
-- if @IndexFieldType@ specifies the field is of type @literal-array@. All
-- options are enabled by default.
--
-- /See:/ 'newLiteralArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
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
-- Create a value of 'LiteralArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'literalArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'facetEnabled', 'literalArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'literalArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'literalArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'sourceFields', 'literalArrayOptions_sourceFields' - A list of source fields to map to the field.
newLiteralArrayOptions ::
  LiteralArrayOptions
newLiteralArrayOptions =
  LiteralArrayOptions'
    { defaultValue =
        Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      sourceFields = Prelude.Nothing
    }

-- | A value to use for the field if the field isn\'t specified for a
-- document.
literalArrayOptions_defaultValue :: Lens.Lens' LiteralArrayOptions (Prelude.Maybe Prelude.Text)
literalArrayOptions_defaultValue = Lens.lens (\LiteralArrayOptions' {defaultValue} -> defaultValue) (\s@LiteralArrayOptions' {} a -> s {defaultValue = a} :: LiteralArrayOptions)

-- | Whether facet information can be returned for the field.
literalArrayOptions_facetEnabled :: Lens.Lens' LiteralArrayOptions (Prelude.Maybe Prelude.Bool)
literalArrayOptions_facetEnabled = Lens.lens (\LiteralArrayOptions' {facetEnabled} -> facetEnabled) (\s@LiteralArrayOptions' {} a -> s {facetEnabled = a} :: LiteralArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
literalArrayOptions_returnEnabled :: Lens.Lens' LiteralArrayOptions (Prelude.Maybe Prelude.Bool)
literalArrayOptions_returnEnabled = Lens.lens (\LiteralArrayOptions' {returnEnabled} -> returnEnabled) (\s@LiteralArrayOptions' {} a -> s {returnEnabled = a} :: LiteralArrayOptions)

-- | Whether the contents of the field are searchable.
literalArrayOptions_searchEnabled :: Lens.Lens' LiteralArrayOptions (Prelude.Maybe Prelude.Bool)
literalArrayOptions_searchEnabled = Lens.lens (\LiteralArrayOptions' {searchEnabled} -> searchEnabled) (\s@LiteralArrayOptions' {} a -> s {searchEnabled = a} :: LiteralArrayOptions)

-- | A list of source fields to map to the field.
literalArrayOptions_sourceFields :: Lens.Lens' LiteralArrayOptions (Prelude.Maybe Prelude.Text)
literalArrayOptions_sourceFields = Lens.lens (\LiteralArrayOptions' {sourceFields} -> sourceFields) (\s@LiteralArrayOptions' {} a -> s {sourceFields = a} :: LiteralArrayOptions)

instance Data.FromXML LiteralArrayOptions where
  parseXML x =
    LiteralArrayOptions'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "FacetEnabled")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "SearchEnabled")
      Prelude.<*> (x Data..@? "SourceFields")

instance Prelude.Hashable LiteralArrayOptions where
  hashWithSalt _salt LiteralArrayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` facetEnabled
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` searchEnabled
      `Prelude.hashWithSalt` sourceFields

instance Prelude.NFData LiteralArrayOptions where
  rnf LiteralArrayOptions' {..} =
    Prelude.rnf defaultValue `Prelude.seq`
      Prelude.rnf facetEnabled `Prelude.seq`
        Prelude.rnf returnEnabled `Prelude.seq`
          Prelude.rnf searchEnabled `Prelude.seq`
            Prelude.rnf sourceFields

instance Data.ToQuery LiteralArrayOptions where
  toQuery LiteralArrayOptions' {..} =
    Prelude.mconcat
      [ "DefaultValue" Data.=: defaultValue,
        "FacetEnabled" Data.=: facetEnabled,
        "ReturnEnabled" Data.=: returnEnabled,
        "SearchEnabled" Data.=: searchEnabled,
        "SourceFields" Data.=: sourceFields
      ]
