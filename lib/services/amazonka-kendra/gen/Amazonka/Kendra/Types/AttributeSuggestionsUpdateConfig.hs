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
-- Module      : Amazonka.Kendra.Types.AttributeSuggestionsUpdateConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AttributeSuggestionsUpdateConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AttributeSuggestionsMode
import Amazonka.Kendra.Types.SuggestableConfig
import qualified Amazonka.Prelude as Prelude

-- | Updates the configuration information for the document
-- fields\/attributes that you want to base query suggestions on.
--
-- To deactivate using documents fields for query suggestions, set the mode
-- to @INACTIVE@. You must also set @SuggestionTypes@ as either @QUERY@ or
-- @DOCUMENT_ATTRIBUTES@ and then call
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>.
-- If you set to @QUERY@, then Amazon Kendra uses the query history to base
-- suggestions on. If you set to @DOCUMENT_ATTRIBUTES@, then Amazon Kendra
-- uses the contents of document fields to base suggestions on.
--
-- /See:/ 'newAttributeSuggestionsUpdateConfig' smart constructor.
data AttributeSuggestionsUpdateConfig = AttributeSuggestionsUpdateConfig'
  { -- | You can set the mode to @ACTIVE@ or @INACTIVE@. You must also set
    -- @SuggestionTypes@ as either @QUERY@ or @DOCUMENT_ATTRIBUTES@ and then
    -- call
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>.
    -- If @Mode@ to use query history is set to @ENABLED@ when calling
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
    -- and @AttributeSuggestionsMode@ to use fields\/attributes is set to
    -- @ACTIVE@, and you haven\'t set your @SuggestionTypes@ preference to
    -- @DOCUMENT_ATTRIBUTES@, then Amazon Kendra uses the query history.
    attributeSuggestionsMode :: Prelude.Maybe AttributeSuggestionsMode,
    -- | The list of fields\/attributes that you want to set as suggestible for
    -- query suggestions.
    suggestableConfigList :: Prelude.Maybe [SuggestableConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeSuggestionsUpdateConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeSuggestionsMode', 'attributeSuggestionsUpdateConfig_attributeSuggestionsMode' - You can set the mode to @ACTIVE@ or @INACTIVE@. You must also set
-- @SuggestionTypes@ as either @QUERY@ or @DOCUMENT_ATTRIBUTES@ and then
-- call
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>.
-- If @Mode@ to use query history is set to @ENABLED@ when calling
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- and @AttributeSuggestionsMode@ to use fields\/attributes is set to
-- @ACTIVE@, and you haven\'t set your @SuggestionTypes@ preference to
-- @DOCUMENT_ATTRIBUTES@, then Amazon Kendra uses the query history.
--
-- 'suggestableConfigList', 'attributeSuggestionsUpdateConfig_suggestableConfigList' - The list of fields\/attributes that you want to set as suggestible for
-- query suggestions.
newAttributeSuggestionsUpdateConfig ::
  AttributeSuggestionsUpdateConfig
newAttributeSuggestionsUpdateConfig =
  AttributeSuggestionsUpdateConfig'
    { attributeSuggestionsMode =
        Prelude.Nothing,
      suggestableConfigList = Prelude.Nothing
    }

-- | You can set the mode to @ACTIVE@ or @INACTIVE@. You must also set
-- @SuggestionTypes@ as either @QUERY@ or @DOCUMENT_ATTRIBUTES@ and then
-- call
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>.
-- If @Mode@ to use query history is set to @ENABLED@ when calling
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- and @AttributeSuggestionsMode@ to use fields\/attributes is set to
-- @ACTIVE@, and you haven\'t set your @SuggestionTypes@ preference to
-- @DOCUMENT_ATTRIBUTES@, then Amazon Kendra uses the query history.
attributeSuggestionsUpdateConfig_attributeSuggestionsMode :: Lens.Lens' AttributeSuggestionsUpdateConfig (Prelude.Maybe AttributeSuggestionsMode)
attributeSuggestionsUpdateConfig_attributeSuggestionsMode = Lens.lens (\AttributeSuggestionsUpdateConfig' {attributeSuggestionsMode} -> attributeSuggestionsMode) (\s@AttributeSuggestionsUpdateConfig' {} a -> s {attributeSuggestionsMode = a} :: AttributeSuggestionsUpdateConfig)

-- | The list of fields\/attributes that you want to set as suggestible for
-- query suggestions.
attributeSuggestionsUpdateConfig_suggestableConfigList :: Lens.Lens' AttributeSuggestionsUpdateConfig (Prelude.Maybe [SuggestableConfig])
attributeSuggestionsUpdateConfig_suggestableConfigList = Lens.lens (\AttributeSuggestionsUpdateConfig' {suggestableConfigList} -> suggestableConfigList) (\s@AttributeSuggestionsUpdateConfig' {} a -> s {suggestableConfigList = a} :: AttributeSuggestionsUpdateConfig) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    AttributeSuggestionsUpdateConfig
  where
  hashWithSalt
    _salt
    AttributeSuggestionsUpdateConfig' {..} =
      _salt
        `Prelude.hashWithSalt` attributeSuggestionsMode
        `Prelude.hashWithSalt` suggestableConfigList

instance
  Prelude.NFData
    AttributeSuggestionsUpdateConfig
  where
  rnf AttributeSuggestionsUpdateConfig' {..} =
    Prelude.rnf attributeSuggestionsMode
      `Prelude.seq` Prelude.rnf suggestableConfigList

instance Data.ToJSON AttributeSuggestionsUpdateConfig where
  toJSON AttributeSuggestionsUpdateConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeSuggestionsMode" Data..=)
              Prelude.<$> attributeSuggestionsMode,
            ("SuggestableConfigList" Data..=)
              Prelude.<$> suggestableConfigList
          ]
      )
