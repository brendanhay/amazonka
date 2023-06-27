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
-- Module      : Amazonka.WAFV2.Types.RateLimitQueryString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitQueryString where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.TextTransformation

-- | Specifies the request\'s query string as an aggregate key for a
-- rate-based rule. Each distinct string contributes to the aggregation
-- instance. If you use just the query string as your custom key, then each
-- string fully defines an aggregation instance.
--
-- /See:/ 'newRateLimitQueryString' smart constructor.
data RateLimitQueryString = RateLimitQueryString'
  { -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass detection. Text
    -- transformations are used in rule match statements, to transform the
    -- @FieldToMatch@ request component before inspecting it, and they\'re used
    -- in rate-based rule statements, to transform request components before
    -- using them as custom aggregation keys. If you specify one or more
    -- transformations to apply, WAF performs all transformations on the
    -- specified content, starting from the lowest priority setting, and then
    -- uses the component contents.
    textTransformations :: Prelude.NonEmpty TextTransformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateLimitQueryString' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textTransformations', 'rateLimitQueryString_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
newRateLimitQueryString ::
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RateLimitQueryString
newRateLimitQueryString pTextTransformations_ =
  RateLimitQueryString'
    { textTransformations =
        Lens.coerced Lens.# pTextTransformations_
    }

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
rateLimitQueryString_textTransformations :: Lens.Lens' RateLimitQueryString (Prelude.NonEmpty TextTransformation)
rateLimitQueryString_textTransformations = Lens.lens (\RateLimitQueryString' {textTransformations} -> textTransformations) (\s@RateLimitQueryString' {} a -> s {textTransformations = a} :: RateLimitQueryString) Prelude.. Lens.coerced

instance Data.FromJSON RateLimitQueryString where
  parseJSON =
    Data.withObject
      "RateLimitQueryString"
      ( \x ->
          RateLimitQueryString'
            Prelude.<$> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable RateLimitQueryString where
  hashWithSalt _salt RateLimitQueryString' {..} =
    _salt `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData RateLimitQueryString where
  rnf RateLimitQueryString' {..} =
    Prelude.rnf textTransformations

instance Data.ToJSON RateLimitQueryString where
  toJSON RateLimitQueryString' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
