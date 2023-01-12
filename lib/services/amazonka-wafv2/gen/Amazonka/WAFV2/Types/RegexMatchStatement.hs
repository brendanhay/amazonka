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
-- Module      : Amazonka.WAFV2.Types.RegexMatchStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RegexMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement used to search web request components for a match
-- against a single regular expression.
--
-- /See:/ 'newRegexMatchStatement' smart constructor.
data RegexMatchStatement = RegexMatchStatement'
  { -- | The string representing the regular expression.
    regexString :: Prelude.Text,
    -- | The part of the web request that you want WAF to inspect.
    fieldToMatch :: FieldToMatch,
    -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass detection. If you
    -- specify one or more transformations in a rule statement, WAF performs
    -- all transformations on the content of the request component identified
    -- by @FieldToMatch@, starting from the lowest priority setting, before
    -- inspecting the content for a match.
    textTransformations :: Prelude.NonEmpty TextTransformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegexMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexString', 'regexMatchStatement_regexString' - The string representing the regular expression.
--
-- 'fieldToMatch', 'regexMatchStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'textTransformations', 'regexMatchStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
newRegexMatchStatement ::
  -- | 'regexString'
  Prelude.Text ->
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RegexMatchStatement
newRegexMatchStatement
  pRegexString_
  pFieldToMatch_
  pTextTransformations_ =
    RegexMatchStatement'
      { regexString = pRegexString_,
        fieldToMatch = pFieldToMatch_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_
      }

-- | The string representing the regular expression.
regexMatchStatement_regexString :: Lens.Lens' RegexMatchStatement Prelude.Text
regexMatchStatement_regexString = Lens.lens (\RegexMatchStatement' {regexString} -> regexString) (\s@RegexMatchStatement' {} a -> s {regexString = a} :: RegexMatchStatement)

-- | The part of the web request that you want WAF to inspect.
regexMatchStatement_fieldToMatch :: Lens.Lens' RegexMatchStatement FieldToMatch
regexMatchStatement_fieldToMatch = Lens.lens (\RegexMatchStatement' {fieldToMatch} -> fieldToMatch) (\s@RegexMatchStatement' {} a -> s {fieldToMatch = a} :: RegexMatchStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
regexMatchStatement_textTransformations :: Lens.Lens' RegexMatchStatement (Prelude.NonEmpty TextTransformation)
regexMatchStatement_textTransformations = Lens.lens (\RegexMatchStatement' {textTransformations} -> textTransformations) (\s@RegexMatchStatement' {} a -> s {textTransformations = a} :: RegexMatchStatement) Prelude.. Lens.coerced

instance Data.FromJSON RegexMatchStatement where
  parseJSON =
    Data.withObject
      "RegexMatchStatement"
      ( \x ->
          RegexMatchStatement'
            Prelude.<$> (x Data..: "RegexString")
            Prelude.<*> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable RegexMatchStatement where
  hashWithSalt _salt RegexMatchStatement' {..} =
    _salt `Prelude.hashWithSalt` regexString
      `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData RegexMatchStatement where
  rnf RegexMatchStatement' {..} =
    Prelude.rnf regexString
      `Prelude.seq` Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf textTransformations

instance Data.ToJSON RegexMatchStatement where
  toJSON RegexMatchStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RegexString" Data..= regexString),
            Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
