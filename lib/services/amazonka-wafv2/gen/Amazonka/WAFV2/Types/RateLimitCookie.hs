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
-- Module      : Amazonka.WAFV2.Types.RateLimitCookie
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitCookie where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.TextTransformation

-- | Specifies a cookie as an aggregate key for a rate-based rule. Each
-- distinct value in the cookie contributes to the aggregation instance. If
-- you use a single cookie as your custom key, then each value fully
-- defines an aggregation instance.
--
-- /See:/ 'newRateLimitCookie' smart constructor.
data RateLimitCookie = RateLimitCookie'
  { -- | The name of the cookie to use.
    name :: Prelude.Text,
    -- | Text transformations eliminate some of the unusual formatting that
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
-- Create a value of 'RateLimitCookie' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rateLimitCookie_name' - The name of the cookie to use.
--
-- 'textTransformations', 'rateLimitCookie_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
newRateLimitCookie ::
  -- | 'name'
  Prelude.Text ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RateLimitCookie
newRateLimitCookie pName_ pTextTransformations_ =
  RateLimitCookie'
    { name = pName_,
      textTransformations =
        Lens.coerced Lens.# pTextTransformations_
    }

-- | The name of the cookie to use.
rateLimitCookie_name :: Lens.Lens' RateLimitCookie Prelude.Text
rateLimitCookie_name = Lens.lens (\RateLimitCookie' {name} -> name) (\s@RateLimitCookie' {} a -> s {name = a} :: RateLimitCookie)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
rateLimitCookie_textTransformations :: Lens.Lens' RateLimitCookie (Prelude.NonEmpty TextTransformation)
rateLimitCookie_textTransformations = Lens.lens (\RateLimitCookie' {textTransformations} -> textTransformations) (\s@RateLimitCookie' {} a -> s {textTransformations = a} :: RateLimitCookie) Prelude.. Lens.coerced

instance Data.FromJSON RateLimitCookie where
  parseJSON =
    Data.withObject
      "RateLimitCookie"
      ( \x ->
          RateLimitCookie'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable RateLimitCookie where
  hashWithSalt _salt RateLimitCookie' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData RateLimitCookie where
  rnf RateLimitCookie' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf textTransformations

instance Data.ToJSON RateLimitCookie where
  toJSON RateLimitCookie' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
