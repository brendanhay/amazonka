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
-- Module      : Amazonka.Kendra.Types.AttributeSuggestionsDescribeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AttributeSuggestionsDescribeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.AttributeSuggestionsMode
import Amazonka.Kendra.Types.SuggestableConfig
import qualified Amazonka.Prelude as Prelude

-- | Gets information on the configuration of document fields\/attributes
-- that you want to base query suggestions on. To change your
-- configuration, use
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_AttributeSuggestionsUpdateConfig.html AttributeSuggestionsUpdateConfig>
-- and then call
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>.
--
-- /See:/ 'newAttributeSuggestionsDescribeConfig' smart constructor.
data AttributeSuggestionsDescribeConfig = AttributeSuggestionsDescribeConfig'
  { -- | The mode is set to either @ACTIVE@ or @INACTIVE@. If the @Mode@ for
    -- query history is set to @ENABLED@ when calling
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
-- Create a value of 'AttributeSuggestionsDescribeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeSuggestionsMode', 'attributeSuggestionsDescribeConfig_attributeSuggestionsMode' - The mode is set to either @ACTIVE@ or @INACTIVE@. If the @Mode@ for
-- query history is set to @ENABLED@ when calling
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- and @AttributeSuggestionsMode@ to use fields\/attributes is set to
-- @ACTIVE@, and you haven\'t set your @SuggestionTypes@ preference to
-- @DOCUMENT_ATTRIBUTES@, then Amazon Kendra uses the query history.
--
-- 'suggestableConfigList', 'attributeSuggestionsDescribeConfig_suggestableConfigList' - The list of fields\/attributes that you want to set as suggestible for
-- query suggestions.
newAttributeSuggestionsDescribeConfig ::
  AttributeSuggestionsDescribeConfig
newAttributeSuggestionsDescribeConfig =
  AttributeSuggestionsDescribeConfig'
    { attributeSuggestionsMode =
        Prelude.Nothing,
      suggestableConfigList = Prelude.Nothing
    }

-- | The mode is set to either @ACTIVE@ or @INACTIVE@. If the @Mode@ for
-- query history is set to @ENABLED@ when calling
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateQuerySuggestionsConfig.html UpdateQuerySuggestionsConfig>
-- and @AttributeSuggestionsMode@ to use fields\/attributes is set to
-- @ACTIVE@, and you haven\'t set your @SuggestionTypes@ preference to
-- @DOCUMENT_ATTRIBUTES@, then Amazon Kendra uses the query history.
attributeSuggestionsDescribeConfig_attributeSuggestionsMode :: Lens.Lens' AttributeSuggestionsDescribeConfig (Prelude.Maybe AttributeSuggestionsMode)
attributeSuggestionsDescribeConfig_attributeSuggestionsMode = Lens.lens (\AttributeSuggestionsDescribeConfig' {attributeSuggestionsMode} -> attributeSuggestionsMode) (\s@AttributeSuggestionsDescribeConfig' {} a -> s {attributeSuggestionsMode = a} :: AttributeSuggestionsDescribeConfig)

-- | The list of fields\/attributes that you want to set as suggestible for
-- query suggestions.
attributeSuggestionsDescribeConfig_suggestableConfigList :: Lens.Lens' AttributeSuggestionsDescribeConfig (Prelude.Maybe [SuggestableConfig])
attributeSuggestionsDescribeConfig_suggestableConfigList = Lens.lens (\AttributeSuggestionsDescribeConfig' {suggestableConfigList} -> suggestableConfigList) (\s@AttributeSuggestionsDescribeConfig' {} a -> s {suggestableConfigList = a} :: AttributeSuggestionsDescribeConfig) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AttributeSuggestionsDescribeConfig
  where
  parseJSON =
    Data.withObject
      "AttributeSuggestionsDescribeConfig"
      ( \x ->
          AttributeSuggestionsDescribeConfig'
            Prelude.<$> (x Data..:? "AttributeSuggestionsMode")
            Prelude.<*> ( x
                            Data..:? "SuggestableConfigList"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AttributeSuggestionsDescribeConfig
  where
  hashWithSalt
    _salt
    AttributeSuggestionsDescribeConfig' {..} =
      _salt
        `Prelude.hashWithSalt` attributeSuggestionsMode
        `Prelude.hashWithSalt` suggestableConfigList

instance
  Prelude.NFData
    AttributeSuggestionsDescribeConfig
  where
  rnf AttributeSuggestionsDescribeConfig' {..} =
    Prelude.rnf attributeSuggestionsMode
      `Prelude.seq` Prelude.rnf suggestableConfigList
