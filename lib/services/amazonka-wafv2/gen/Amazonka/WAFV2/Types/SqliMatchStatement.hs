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
-- Module      : Amazonka.WAFV2.Types.SqliMatchStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SqliMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.SensitivityLevel
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement that inspects for malicious SQL code. Attackers insert
-- malicious SQL code into web requests to do things like modify your
-- database or extract data from it.
--
-- /See:/ 'newSqliMatchStatement' smart constructor.
data SqliMatchStatement = SqliMatchStatement'
  { -- | The sensitivity that you want WAF to use to inspect for SQL injection
    -- attacks.
    --
    -- @HIGH@ detects more attacks, but might generate more false positives,
    -- especially if your web requests frequently contain unusual strings. For
    -- information about identifying and mitigating false positives, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/web-acl-testing.html Testing and tuning>
    -- in the /WAF Developer Guide/.
    --
    -- @LOW@ is generally a better choice for resources that already have other
    -- protections against SQL injection attacks or that have a low tolerance
    -- for false positives.
    --
    -- Default: @LOW@
    sensitivityLevel :: Prelude.Maybe SensitivityLevel,
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
-- Create a value of 'SqliMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sensitivityLevel', 'sqliMatchStatement_sensitivityLevel' - The sensitivity that you want WAF to use to inspect for SQL injection
-- attacks.
--
-- @HIGH@ detects more attacks, but might generate more false positives,
-- especially if your web requests frequently contain unusual strings. For
-- information about identifying and mitigating false positives, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/web-acl-testing.html Testing and tuning>
-- in the /WAF Developer Guide/.
--
-- @LOW@ is generally a better choice for resources that already have other
-- protections against SQL injection attacks or that have a low tolerance
-- for false positives.
--
-- Default: @LOW@
--
-- 'fieldToMatch', 'sqliMatchStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'textTransformations', 'sqliMatchStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
newSqliMatchStatement ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  SqliMatchStatement
newSqliMatchStatement
  pFieldToMatch_
  pTextTransformations_ =
    SqliMatchStatement'
      { sensitivityLevel =
          Prelude.Nothing,
        fieldToMatch = pFieldToMatch_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_
      }

-- | The sensitivity that you want WAF to use to inspect for SQL injection
-- attacks.
--
-- @HIGH@ detects more attacks, but might generate more false positives,
-- especially if your web requests frequently contain unusual strings. For
-- information about identifying and mitigating false positives, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/web-acl-testing.html Testing and tuning>
-- in the /WAF Developer Guide/.
--
-- @LOW@ is generally a better choice for resources that already have other
-- protections against SQL injection attacks or that have a low tolerance
-- for false positives.
--
-- Default: @LOW@
sqliMatchStatement_sensitivityLevel :: Lens.Lens' SqliMatchStatement (Prelude.Maybe SensitivityLevel)
sqliMatchStatement_sensitivityLevel = Lens.lens (\SqliMatchStatement' {sensitivityLevel} -> sensitivityLevel) (\s@SqliMatchStatement' {} a -> s {sensitivityLevel = a} :: SqliMatchStatement)

-- | The part of the web request that you want WAF to inspect.
sqliMatchStatement_fieldToMatch :: Lens.Lens' SqliMatchStatement FieldToMatch
sqliMatchStatement_fieldToMatch = Lens.lens (\SqliMatchStatement' {fieldToMatch} -> fieldToMatch) (\s@SqliMatchStatement' {} a -> s {fieldToMatch = a} :: SqliMatchStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
sqliMatchStatement_textTransformations :: Lens.Lens' SqliMatchStatement (Prelude.NonEmpty TextTransformation)
sqliMatchStatement_textTransformations = Lens.lens (\SqliMatchStatement' {textTransformations} -> textTransformations) (\s@SqliMatchStatement' {} a -> s {textTransformations = a} :: SqliMatchStatement) Prelude.. Lens.coerced

instance Core.FromJSON SqliMatchStatement where
  parseJSON =
    Core.withObject
      "SqliMatchStatement"
      ( \x ->
          SqliMatchStatement'
            Prelude.<$> (x Core..:? "SensitivityLevel")
            Prelude.<*> (x Core..: "FieldToMatch")
            Prelude.<*> (x Core..: "TextTransformations")
      )

instance Prelude.Hashable SqliMatchStatement where
  hashWithSalt _salt SqliMatchStatement' {..} =
    _salt `Prelude.hashWithSalt` sensitivityLevel
      `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData SqliMatchStatement where
  rnf SqliMatchStatement' {..} =
    Prelude.rnf sensitivityLevel
      `Prelude.seq` Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf textTransformations

instance Core.ToJSON SqliMatchStatement where
  toJSON SqliMatchStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SensitivityLevel" Core..=)
              Prelude.<$> sensitivityLevel,
            Prelude.Just ("FieldToMatch" Core..= fieldToMatch),
            Prelude.Just
              ("TextTransformations" Core..= textTransformations)
          ]
      )
