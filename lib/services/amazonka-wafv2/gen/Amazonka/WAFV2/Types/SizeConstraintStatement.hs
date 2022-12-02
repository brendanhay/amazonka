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
-- Module      : Amazonka.WAFV2.Types.SizeConstraintStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SizeConstraintStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ComparisonOperator
import Amazonka.WAFV2.Types.FieldToMatch
import Amazonka.WAFV2.Types.TextTransformation

-- | A rule statement that compares a number of bytes against the size of a
-- request component, using a comparison operator, such as greater than (>)
-- or less than (\<). For example, you can use a size constraint statement
-- to look for query strings that are longer than 100 bytes.
--
-- If you configure WAF to inspect the request body, WAF inspects only the
-- first 8192 bytes (8 KB). If the request body for your web requests never
-- exceeds 8192 bytes, you could use a size constraint statement to block
-- requests that have a request body greater than 8192 bytes.
--
-- If you choose URI for the value of Part of the request to filter on, the
-- slash (\/) in the URI counts as one character. For example, the URI
-- @\/logo.jpg@ is nine characters long.
--
-- /See:/ 'newSizeConstraintStatement' smart constructor.
data SizeConstraintStatement = SizeConstraintStatement'
  { -- | The part of the web request that you want WAF to inspect.
    fieldToMatch :: FieldToMatch,
    -- | The operator to use to compare the request part to the size setting.
    comparisonOperator :: ComparisonOperator,
    -- | The size, in byte, to compare to the request part, after any
    -- transformations.
    size :: Prelude.Natural,
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
-- Create a value of 'SizeConstraintStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldToMatch', 'sizeConstraintStatement_fieldToMatch' - The part of the web request that you want WAF to inspect.
--
-- 'comparisonOperator', 'sizeConstraintStatement_comparisonOperator' - The operator to use to compare the request part to the size setting.
--
-- 'size', 'sizeConstraintStatement_size' - The size, in byte, to compare to the request part, after any
-- transformations.
--
-- 'textTransformations', 'sizeConstraintStatement_textTransformations' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
newSizeConstraintStatement ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'size'
  Prelude.Natural ->
  -- | 'textTransformations'
  Prelude.NonEmpty TextTransformation ->
  SizeConstraintStatement
newSizeConstraintStatement
  pFieldToMatch_
  pComparisonOperator_
  pSize_
  pTextTransformations_ =
    SizeConstraintStatement'
      { fieldToMatch =
          pFieldToMatch_,
        comparisonOperator = pComparisonOperator_,
        size = pSize_,
        textTransformations =
          Lens.coerced Lens.# pTextTransformations_
      }

-- | The part of the web request that you want WAF to inspect.
sizeConstraintStatement_fieldToMatch :: Lens.Lens' SizeConstraintStatement FieldToMatch
sizeConstraintStatement_fieldToMatch = Lens.lens (\SizeConstraintStatement' {fieldToMatch} -> fieldToMatch) (\s@SizeConstraintStatement' {} a -> s {fieldToMatch = a} :: SizeConstraintStatement)

-- | The operator to use to compare the request part to the size setting.
sizeConstraintStatement_comparisonOperator :: Lens.Lens' SizeConstraintStatement ComparisonOperator
sizeConstraintStatement_comparisonOperator = Lens.lens (\SizeConstraintStatement' {comparisonOperator} -> comparisonOperator) (\s@SizeConstraintStatement' {} a -> s {comparisonOperator = a} :: SizeConstraintStatement)

-- | The size, in byte, to compare to the request part, after any
-- transformations.
sizeConstraintStatement_size :: Lens.Lens' SizeConstraintStatement Prelude.Natural
sizeConstraintStatement_size = Lens.lens (\SizeConstraintStatement' {size} -> size) (\s@SizeConstraintStatement' {} a -> s {size = a} :: SizeConstraintStatement)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass detection. If you
-- specify one or more transformations in a rule statement, WAF performs
-- all transformations on the content of the request component identified
-- by @FieldToMatch@, starting from the lowest priority setting, before
-- inspecting the content for a match.
sizeConstraintStatement_textTransformations :: Lens.Lens' SizeConstraintStatement (Prelude.NonEmpty TextTransformation)
sizeConstraintStatement_textTransformations = Lens.lens (\SizeConstraintStatement' {textTransformations} -> textTransformations) (\s@SizeConstraintStatement' {} a -> s {textTransformations = a} :: SizeConstraintStatement) Prelude.. Lens.coerced

instance Data.FromJSON SizeConstraintStatement where
  parseJSON =
    Data.withObject
      "SizeConstraintStatement"
      ( \x ->
          SizeConstraintStatement'
            Prelude.<$> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "ComparisonOperator")
            Prelude.<*> (x Data..: "Size")
            Prelude.<*> (x Data..: "TextTransformations")
      )

instance Prelude.Hashable SizeConstraintStatement where
  hashWithSalt _salt SizeConstraintStatement' {..} =
    _salt `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` textTransformations

instance Prelude.NFData SizeConstraintStatement where
  rnf SizeConstraintStatement' {..} =
    Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf textTransformations

instance Data.ToJSON SizeConstraintStatement where
  toJSON SizeConstraintStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("ComparisonOperator" Data..= comparisonOperator),
            Prelude.Just ("Size" Data..= size),
            Prelude.Just
              ("TextTransformations" Data..= textTransformations)
          ]
      )
