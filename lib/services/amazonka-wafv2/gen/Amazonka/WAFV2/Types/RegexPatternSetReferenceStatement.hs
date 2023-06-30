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
-- Module      : Amazonka.WAFV2.Types.RegexPatternSetReferenceStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RegexPatternSetReferenceStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement used to search web request components for matches with
-- regular expressions. To use this, create a RegexPatternSet that
-- specifies the expressions that you want to detect, then use the ARN of
-- that set in this statement. A web request matches the pattern set rule
-- statement if the request component matches any of the patterns in the
-- set. To create a regex pattern set, see CreateRegexPatternSet.
--
-- Each regex pattern set rule statement references a regex pattern set.
-- You create and maintain the set independent of your rules. This allows
-- you to use the single set in multiple rules. When you update the
-- referenced set, WAF automatically updates all rules that reference it.
--
-- /See:/ 'newRegexPatternSetReferenceStatement' smart constructor.
data RegexPatternSetReferenceStatement = RegexPatternSetReferenceStatement'
  { -- | The Amazon Resource Name (ARN) of the RegexPatternSet that this
    -- statement references.
    arn :: Prelude.Text,
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
-- Create a value of 'RegexPatternSetReferenceStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'regexPatternSetReferenceStatement_arn' - The Amazon Resource Name (ARN) of the RegexPatternSet that this
-- statement references.
--
-- 'fieldToMatch', 'regexPatternSetReferenceStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'textTransformations', 'regexPatternSetReferenceStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
newRegexPatternSetReferenceStatement ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RegexPatternSetReferenceStatement
newRegexPatternSetReferenceStatement
  pARN_
  pFieldToMatch_
  pTextTransformations_ =
    RegexPatternSetReferenceStatement'
      { arn = pARN_,
        fieldToMatch = pFieldToMatch_,
        textTransformations =
          Lens.coerced
            Lens.# pTextTransformations_
      }

-- | The Amazon Resource Name (ARN) of the RegexPatternSet that this
-- statement references.
regexPatternSetReferenceStatement_arn :: Lens.Lens' RegexPatternSetReferenceStatement Prelude.Text
regexPatternSetReferenceStatement_arn = Lens.lens (\RegexPatternSetReferenceStatement' {arn} -> arn) (\s@RegexPatternSetReferenceStatement' {} a -> s {arn = a} :: RegexPatternSetReferenceStatement)

-- | The part of the web request that you want WAF to inspect.
regexPatternSetReferenceStatement_fieldToMatch :: Lens.Lens' RegexPatternSetReferenceStatement FieldToMatch
regexPatternSetReferenceStatement_fieldToMatch = Lens.lens (\RegexPatternSetReferenceStatement' {fieldToMatch} -> fieldToMatch) (\s@RegexPatternSetReferenceStatement' {} a -> s {fieldToMatch = a} :: RegexPatternSetReferenceStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
regexPatternSetReferenceStatement_textTransformations :: Lens.Lens' RegexPatternSetReferenceStatement (Prelude.NonEmpty TextTransformation)
regexPatternSetReferenceStatement_textTransformations = Lens.lens (\RegexPatternSetReferenceStatement' {textTransformations} -> textTransformations) (\s@RegexPatternSetReferenceStatement' {} a -> s {textTransformations = a} :: RegexPatternSetReferenceStatement) Prelude.. Lens.coerced

instance
  Data.FromJSON
    RegexPatternSetReferenceStatement
  where
  parseJSON =
    Data.withObject
      "RegexPatternSetReferenceStatement"
      ( \x ->
          RegexPatternSetReferenceStatement'
            Prelude.<$> (x Data..: "ARN")
            Prelude.<*> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance
  Prelude.Hashable
    RegexPatternSetReferenceStatement
  where
  hashWithSalt
    _salt
    RegexPatternSetReferenceStatement' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` fieldToMatch
        `Prelude.hashWithSalt` textTransformations

instance
  Prelude.NFData
    RegexPatternSetReferenceStatement
  where
  rnf RegexPatternSetReferenceStatement' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf textTransformations

instance
  Data.ToJSON
    RegexPatternSetReferenceStatement
  where
  toJSON RegexPatternSetReferenceStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ARN" Data..= arn),
            Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
