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
-- Module      : Amazonka.WAFV2.Types.RateLimitQueryArgument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitQueryArgument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.TextTransformation

-- | Specifies a query argument in the request as an aggregate key for a
-- rate-based rule. Each distinct value for the named query argument
-- contributes to the aggregation instance. If you use a single query
-- argument as your custom key, then each value fully defines an
-- aggregation instance.
--
-- /See:/ 'newRateLimitQueryArgument' smart constructor.
data RateLimitQueryArgument = RateLimitQueryArgument'
  { -- | The name of the query argument to use.
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
-- Create a value of 'RateLimitQueryArgument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rateLimitQueryArgument_name' - The name of the query argument to use.
--
-- 'textTransformations', 'rateLimitQueryArgument_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
newRateLimitQueryArgument ::
  -- | 'name'
  Prelude.Text ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RateLimitQueryArgument
newRateLimitQueryArgument
  pName_
  pTextTransformations_ =
    RateLimitQueryArgument'
      { name = pName_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_
      }

-- | The name of the query argument to use.
rateLimitQueryArgument_name :: Lens.Lens' RateLimitQueryArgument Prelude.Text
rateLimitQueryArgument_name = Lens.lens (\RateLimitQueryArgument' {name} -> name) (\s@RateLimitQueryArgument' {} a -> s {name = a} :: RateLimitQueryArgument)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
rateLimitQueryArgument_textTransformations :: Lens.Lens' RateLimitQueryArgument (Prelude.NonEmpty TextTransformation)
rateLimitQueryArgument_textTransformations = Lens.lens (\RateLimitQueryArgument' {textTransformations} -> textTransformations) (\s@RateLimitQueryArgument' {} a -> s {textTransformations = a} :: RateLimitQueryArgument) Prelude.. Lens.coerced

instance Data.FromJSON RateLimitQueryArgument where
  parseJSON =
    Data.withObject
      "RateLimitQueryArgument"
      ( \x ->
          RateLimitQueryArgument'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable RateLimitQueryArgument where
  hashWithSalt _salt RateLimitQueryArgument' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData RateLimitQueryArgument where
  rnf RateLimitQueryArgument' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf textTransformations

instance Data.ToJSON RateLimitQueryArgument where
  toJSON RateLimitQueryArgument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
