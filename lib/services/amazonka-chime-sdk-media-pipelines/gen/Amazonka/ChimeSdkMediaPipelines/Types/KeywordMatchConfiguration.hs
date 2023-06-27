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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.KeywordMatchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.KeywordMatchConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the settings for a keyword match task.
--
-- /See:/ 'newKeywordMatchConfiguration' smart constructor.
data KeywordMatchConfiguration = KeywordMatchConfiguration'
  { -- | Matches keywords or phrases on their presence or absence. If set to
    -- @TRUE@, the rule matches when all the specified keywords or phrases are
    -- absent. Default: @FALSE@.
    negate :: Prelude.Maybe Prelude.Bool,
    -- | The name of the keyword match rule.
    ruleName :: Prelude.Text,
    -- | The keywords or phrases that you want to match.
    keywords :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeywordMatchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negate', 'keywordMatchConfiguration_negate' - Matches keywords or phrases on their presence or absence. If set to
-- @TRUE@, the rule matches when all the specified keywords or phrases are
-- absent. Default: @FALSE@.
--
-- 'ruleName', 'keywordMatchConfiguration_ruleName' - The name of the keyword match rule.
--
-- 'keywords', 'keywordMatchConfiguration_keywords' - The keywords or phrases that you want to match.
newKeywordMatchConfiguration ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'keywords'
  Prelude.NonEmpty Prelude.Text ->
  KeywordMatchConfiguration
newKeywordMatchConfiguration pRuleName_ pKeywords_ =
  KeywordMatchConfiguration'
    { negate =
        Prelude.Nothing,
      ruleName = pRuleName_,
      keywords = Lens.coerced Lens.# pKeywords_
    }

-- | Matches keywords or phrases on their presence or absence. If set to
-- @TRUE@, the rule matches when all the specified keywords or phrases are
-- absent. Default: @FALSE@.
keywordMatchConfiguration_negate :: Lens.Lens' KeywordMatchConfiguration (Prelude.Maybe Prelude.Bool)
keywordMatchConfiguration_negate = Lens.lens (\KeywordMatchConfiguration' {negate} -> negate) (\s@KeywordMatchConfiguration' {} a -> s {negate = a} :: KeywordMatchConfiguration)

-- | The name of the keyword match rule.
keywordMatchConfiguration_ruleName :: Lens.Lens' KeywordMatchConfiguration Prelude.Text
keywordMatchConfiguration_ruleName = Lens.lens (\KeywordMatchConfiguration' {ruleName} -> ruleName) (\s@KeywordMatchConfiguration' {} a -> s {ruleName = a} :: KeywordMatchConfiguration)

-- | The keywords or phrases that you want to match.
keywordMatchConfiguration_keywords :: Lens.Lens' KeywordMatchConfiguration (Prelude.NonEmpty Prelude.Text)
keywordMatchConfiguration_keywords = Lens.lens (\KeywordMatchConfiguration' {keywords} -> keywords) (\s@KeywordMatchConfiguration' {} a -> s {keywords = a} :: KeywordMatchConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON KeywordMatchConfiguration where
  parseJSON =
    Data.withObject
      "KeywordMatchConfiguration"
      ( \x ->
          KeywordMatchConfiguration'
            Prelude.<$> (x Data..:? "Negate")
            Prelude.<*> (x Data..: "RuleName")
            Prelude.<*> (x Data..: "Keywords")
      )

instance Prelude.Hashable KeywordMatchConfiguration where
  hashWithSalt _salt KeywordMatchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` negate
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` keywords

instance Prelude.NFData KeywordMatchConfiguration where
  rnf KeywordMatchConfiguration' {..} =
    Prelude.rnf negate
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf keywords

instance Data.ToJSON KeywordMatchConfiguration where
  toJSON KeywordMatchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Negate" Data..=) Prelude.<$> negate,
            Prelude.Just ("RuleName" Data..= ruleName),
            Prelude.Just ("Keywords" Data..= keywords)
          ]
      )
