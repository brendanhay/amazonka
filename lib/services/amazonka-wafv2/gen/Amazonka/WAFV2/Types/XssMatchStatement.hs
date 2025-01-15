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
-- Module      : Amazonka.WAFV2.Types.XssMatchStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.XssMatchStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement that inspects for cross-site scripting (XSS) attacks.
-- In XSS attacks, the attacker uses vulnerabilities in a benign website as
-- a vehicle to inject malicious client-site scripts into other legitimate
-- web browsers.
--
-- /See:/ 'newXssMatchStatement' smart constructor.
data XssMatchStatement = XssMatchStatement'
  { -- | The part of the web request that you want WAF to inspect.
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
-- Create a value of 'XssMatchStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldToMatch', 'xssMatchStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'textTransformations', 'xssMatchStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
newXssMatchStatement ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  XssMatchStatement
newXssMatchStatement
  pFieldToMatch_
  pTextTransformations_ =
    XssMatchStatement'
      { fieldToMatch = pFieldToMatch_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_
      }

-- | The part of the web request that you want WAF to inspect.
xssMatchStatement_fieldToMatch :: Lens.Lens' XssMatchStatement FieldToMatch
xssMatchStatement_fieldToMatch = Lens.lens (\XssMatchStatement' {fieldToMatch} -> fieldToMatch) (\s@XssMatchStatement' {} a -> s {fieldToMatch = a} :: XssMatchStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
xssMatchStatement_textTransformations :: Lens.Lens' XssMatchStatement (Prelude.NonEmpty TextTransformation)
xssMatchStatement_textTransformations = Lens.lens (\XssMatchStatement' {textTransformations} -> textTransformations) (\s@XssMatchStatement' {} a -> s {textTransformations = a} :: XssMatchStatement) Prelude.. Lens.coerced

instance Data.FromJSON XssMatchStatement where
  parseJSON =
    Data.withObject
      "XssMatchStatement"
      ( \x ->
          XssMatchStatement'
            Prelude.<$> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable XssMatchStatement where
  hashWithSalt _salt XssMatchStatement' {..} =
    _salt
      `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData XssMatchStatement where
  rnf XssMatchStatement' {..} =
    Prelude.rnf fieldToMatch `Prelude.seq`
      Prelude.rnf textTransformations

instance Data.ToJSON XssMatchStatement where
  toJSON XssMatchStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
