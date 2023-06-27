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
-- Module      : Amazonka.Kendra.Types.AttributeSuggestionsGetConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AttributeSuggestionsGetConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AttributeFilter
import Amazonka.Kendra.Types.UserContext
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for the document
-- fields\/attributes that you want to base query suggestions on.
--
-- /See:/ 'newAttributeSuggestionsGetConfig' smart constructor.
data AttributeSuggestionsGetConfig = AttributeSuggestionsGetConfig'
  { -- | The list of additional document field\/attribute keys or field names to
    -- include in the response. You can use additional fields to provide extra
    -- information in the response. Additional fields are not used to based
    -- suggestions on.
    additionalResponseAttributes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Filters the search results based on document fields\/attributes.
    attributeFilter :: Prelude.Maybe AttributeFilter,
    -- | The list of document field\/attribute keys or field names to use for
    -- query suggestions. If the content within any of the fields match what
    -- your user starts typing as their query, then the field content is
    -- returned as a query suggestion.
    suggestionAttributes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Applies user context filtering so that only users who are given access
    -- to certain documents see these document in their search results.
    userContext :: Prelude.Maybe UserContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeSuggestionsGetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalResponseAttributes', 'attributeSuggestionsGetConfig_additionalResponseAttributes' - The list of additional document field\/attribute keys or field names to
-- include in the response. You can use additional fields to provide extra
-- information in the response. Additional fields are not used to based
-- suggestions on.
--
-- 'attributeFilter', 'attributeSuggestionsGetConfig_attributeFilter' - Filters the search results based on document fields\/attributes.
--
-- 'suggestionAttributes', 'attributeSuggestionsGetConfig_suggestionAttributes' - The list of document field\/attribute keys or field names to use for
-- query suggestions. If the content within any of the fields match what
-- your user starts typing as their query, then the field content is
-- returned as a query suggestion.
--
-- 'userContext', 'attributeSuggestionsGetConfig_userContext' - Applies user context filtering so that only users who are given access
-- to certain documents see these document in their search results.
newAttributeSuggestionsGetConfig ::
  AttributeSuggestionsGetConfig
newAttributeSuggestionsGetConfig =
  AttributeSuggestionsGetConfig'
    { additionalResponseAttributes =
        Prelude.Nothing,
      attributeFilter = Prelude.Nothing,
      suggestionAttributes = Prelude.Nothing,
      userContext = Prelude.Nothing
    }

-- | The list of additional document field\/attribute keys or field names to
-- include in the response. You can use additional fields to provide extra
-- information in the response. Additional fields are not used to based
-- suggestions on.
attributeSuggestionsGetConfig_additionalResponseAttributes :: Lens.Lens' AttributeSuggestionsGetConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
attributeSuggestionsGetConfig_additionalResponseAttributes = Lens.lens (\AttributeSuggestionsGetConfig' {additionalResponseAttributes} -> additionalResponseAttributes) (\s@AttributeSuggestionsGetConfig' {} a -> s {additionalResponseAttributes = a} :: AttributeSuggestionsGetConfig) Prelude.. Lens.mapping Lens.coerced

-- | Filters the search results based on document fields\/attributes.
attributeSuggestionsGetConfig_attributeFilter :: Lens.Lens' AttributeSuggestionsGetConfig (Prelude.Maybe AttributeFilter)
attributeSuggestionsGetConfig_attributeFilter = Lens.lens (\AttributeSuggestionsGetConfig' {attributeFilter} -> attributeFilter) (\s@AttributeSuggestionsGetConfig' {} a -> s {attributeFilter = a} :: AttributeSuggestionsGetConfig)

-- | The list of document field\/attribute keys or field names to use for
-- query suggestions. If the content within any of the fields match what
-- your user starts typing as their query, then the field content is
-- returned as a query suggestion.
attributeSuggestionsGetConfig_suggestionAttributes :: Lens.Lens' AttributeSuggestionsGetConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
attributeSuggestionsGetConfig_suggestionAttributes = Lens.lens (\AttributeSuggestionsGetConfig' {suggestionAttributes} -> suggestionAttributes) (\s@AttributeSuggestionsGetConfig' {} a -> s {suggestionAttributes = a} :: AttributeSuggestionsGetConfig) Prelude.. Lens.mapping Lens.coerced

-- | Applies user context filtering so that only users who are given access
-- to certain documents see these document in their search results.
attributeSuggestionsGetConfig_userContext :: Lens.Lens' AttributeSuggestionsGetConfig (Prelude.Maybe UserContext)
attributeSuggestionsGetConfig_userContext = Lens.lens (\AttributeSuggestionsGetConfig' {userContext} -> userContext) (\s@AttributeSuggestionsGetConfig' {} a -> s {userContext = a} :: AttributeSuggestionsGetConfig)

instance
  Prelude.Hashable
    AttributeSuggestionsGetConfig
  where
  hashWithSalt _salt AttributeSuggestionsGetConfig' {..} =
    _salt
      `Prelude.hashWithSalt` additionalResponseAttributes
      `Prelude.hashWithSalt` attributeFilter
      `Prelude.hashWithSalt` suggestionAttributes
      `Prelude.hashWithSalt` userContext

instance Prelude.NFData AttributeSuggestionsGetConfig where
  rnf AttributeSuggestionsGetConfig' {..} =
    Prelude.rnf additionalResponseAttributes
      `Prelude.seq` Prelude.rnf attributeFilter
      `Prelude.seq` Prelude.rnf suggestionAttributes
      `Prelude.seq` Prelude.rnf userContext

instance Data.ToJSON AttributeSuggestionsGetConfig where
  toJSON AttributeSuggestionsGetConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalResponseAttributes" Data..=)
              Prelude.<$> additionalResponseAttributes,
            ("AttributeFilter" Data..=)
              Prelude.<$> attributeFilter,
            ("SuggestionAttributes" Data..=)
              Prelude.<$> suggestionAttributes,
            ("UserContext" Data..=) Prelude.<$> userContext
          ]
      )
