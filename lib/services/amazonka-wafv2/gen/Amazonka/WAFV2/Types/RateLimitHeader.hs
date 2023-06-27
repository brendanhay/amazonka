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
-- Module      : Amazonka.WAFV2.Types.RateLimitHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateLimitHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.TextTransformation

-- | Specifies a header as an aggregate key for a rate-based rule. Each
-- distinct value in the header contributes to the aggregation instance. If
-- you use a single header as your custom key, then each value fully
-- defines an aggregation instance.
--
-- /See:/ 'newRateLimitHeader' smart constructor.
data RateLimitHeader = RateLimitHeader'
  { -- | The name of the header to use.
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
-- Create a value of 'RateLimitHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rateLimitHeader_name' - The name of the header to use.
--
-- 'textTransformations', 'rateLimitHeader_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
newRateLimitHeader ::
  -- | 'name'
  Prelude.Text ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  RateLimitHeader
newRateLimitHeader pName_ pTextTransformations_ =
  RateLimitHeader'
    { name = pName_,
      textTransformations =
        Lens.coerced Lens.# pTextTransformations_
    }

-- | The name of the header to use.
rateLimitHeader_name :: Lens.Lens' RateLimitHeader Prelude.Text
rateLimitHeader_name = Lens.lens (\RateLimitHeader' {name} -> name) (\s@RateLimitHeader' {} a -> s {name = a} :: RateLimitHeader)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. Text
-- transformations are used in rule match statements, to transform the
-- @FieldToMatch@ request component before inspecting it, and they\'re used
-- in rate-based rule statements, to transform request components before
-- using them as custom aggregation keys. If you specify one or more
-- transformations to apply, WAF performs all transformations on the
-- specified content, starting from the lowest priority setting, and then
-- uses the component contents.
rateLimitHeader_textTransformations :: Lens.Lens' RateLimitHeader (Prelude.NonEmpty TextTransformation)
rateLimitHeader_textTransformations = Lens.lens (\RateLimitHeader' {textTransformations} -> textTransformations) (\s@RateLimitHeader' {} a -> s {textTransformations = a} :: RateLimitHeader) Prelude.. Lens.coerced

instance Data.FromJSON RateLimitHeader where
  parseJSON =
    Data.withObject
      "RateLimitHeader"
      ( \x ->
          RateLimitHeader'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable RateLimitHeader where
  hashWithSalt _salt RateLimitHeader' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData RateLimitHeader where
  rnf RateLimitHeader' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf textTransformations

instance Data.ToJSON RateLimitHeader where
  toJSON RateLimitHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
