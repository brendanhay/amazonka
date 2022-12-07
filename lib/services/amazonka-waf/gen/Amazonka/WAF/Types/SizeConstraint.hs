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
-- Module      : Amazonka.WAF.Types.SizeConstraint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.SizeConstraint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.ComparisonOperator
import Amazonka.WAF.Types.FieldToMatch
import Amazonka.WAF.Types.TextTransformation

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies a constraint on the size of a part of the web request. AWS WAF
-- uses the @Size@, @ComparisonOperator@, and @FieldToMatch@ to build an
-- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
-- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
-- considered to match.
--
-- /See:/ 'newSizeConstraint' smart constructor.
data SizeConstraint = SizeConstraint'
  { -- | Specifies where in a web request to look for the size constraint.
    fieldToMatch :: FieldToMatch,
    -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass AWS WAF. If you
    -- specify a transformation, AWS WAF performs the transformation on
    -- @FieldToMatch@ before inspecting it for a match.
    --
    -- You can only specify a single type of TextTransformation.
    --
    -- Note that if you choose @BODY@ for the value of @Type@, you must choose
    -- @NONE@ for @TextTransformation@ because CloudFront forwards only the
    -- first 8192 bytes for inspection.
    --
    -- __NONE__
    --
    -- Specify @NONE@ if you don\'t want to perform any text transformations.
    --
    -- __CMD_LINE__
    --
    -- When you\'re concerned that attackers are injecting an operating system
    -- command line command and using unusual formatting to disguise some or
    -- all of the command, use this option to perform the following
    -- transformations:
    --
    -- -   Delete the following characters: \\ \" \' ^
    --
    -- -   Delete spaces before the following characters: \/ (
    --
    -- -   Replace the following characters with a space: , ;
    --
    -- -   Replace multiple spaces with one space
    --
    -- -   Convert uppercase letters (A-Z) to lowercase (a-z)
    --
    -- __COMPRESS_WHITE_SPACE__
    --
    -- Use this option to replace the following characters with a space
    -- character (decimal 32):
    --
    -- -   \\f, formfeed, decimal 12
    --
    -- -   \\t, tab, decimal 9
    --
    -- -   \\n, newline, decimal 10
    --
    -- -   \\r, carriage return, decimal 13
    --
    -- -   \\v, vertical tab, decimal 11
    --
    -- -   non-breaking space, decimal 160
    --
    -- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
    --
    -- __HTML_ENTITY_DECODE__
    --
    -- Use this option to replace HTML-encoded characters with unencoded
    -- characters. @HTML_ENTITY_DECODE@ performs the following operations:
    --
    -- -   Replaces @(ampersand)quot;@ with @\"@
    --
    -- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
    --
    -- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
    --
    -- -   Replaces @(ampersand)gt;@ with @>@
    --
    -- -   Replaces characters that are represented in hexadecimal format,
    --     @(ampersand)#xhhhh;@, with the corresponding characters
    --
    -- -   Replaces characters that are represented in decimal format,
    --     @(ampersand)#nnnn;@, with the corresponding characters
    --
    -- __LOWERCASE__
    --
    -- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
    --
    -- __URL_DECODE__
    --
    -- Use this option to decode a URL-encoded value.
    textTransformation :: TextTransformation,
    -- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in
    -- combination with the provided @Size@ and @FieldToMatch@ to build an
    -- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
    -- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
    -- considered to match.
    --
    -- __EQ__: Used to test if the @Size@ is equal to the size of the
    -- @FieldToMatch@
    --
    -- __NE__: Used to test if the @Size@ is not equal to the size of the
    -- @FieldToMatch@
    --
    -- __LE__: Used to test if the @Size@ is less than or equal to the size of
    -- the @FieldToMatch@
    --
    -- __LT__: Used to test if the @Size@ is strictly less than the size of the
    -- @FieldToMatch@
    --
    -- __GE__: Used to test if the @Size@ is greater than or equal to the size
    -- of the @FieldToMatch@
    --
    -- __GT__: Used to test if the @Size@ is strictly greater than the size of
    -- the @FieldToMatch@
    comparisonOperator :: ComparisonOperator,
    -- | The size in bytes that you want AWS WAF to compare against the size of
    -- the specified @FieldToMatch@. AWS WAF uses this in combination with
    -- @ComparisonOperator@ and @FieldToMatch@ to build an expression in the
    -- form of \"@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@\".
    -- If that expression is true, the @SizeConstraint@ is considered to match.
    --
    -- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
    --
    -- If you specify @URI@ for the value of @Type@, the \/ in the URI counts
    -- as one character. For example, the URI @\/logo.jpg@ is nine characters
    -- long.
    size :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SizeConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldToMatch', 'sizeConstraint_fieldToMatch' - Specifies where in a web request to look for the size constraint.
--
-- 'textTransformation', 'sizeConstraint_textTransformation' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
--
-- Note that if you choose @BODY@ for the value of @Type@, you must choose
-- @NONE@ for @TextTransformation@ because CloudFront forwards only the
-- first 8192 bytes for inspection.
--
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
--
-- __CMD_LINE__
--
-- When you\'re concerned that attackers are injecting an operating system
-- command line command and using unusual formatting to disguise some or
-- all of the command, use this option to perform the following
-- transformations:
--
-- -   Delete the following characters: \\ \" \' ^
--
-- -   Delete spaces before the following characters: \/ (
--
-- -   Replace the following characters with a space: , ;
--
-- -   Replace multiple spaces with one space
--
-- -   Convert uppercase letters (A-Z) to lowercase (a-z)
--
-- __COMPRESS_WHITE_SPACE__
--
-- Use this option to replace the following characters with a space
-- character (decimal 32):
--
-- -   \\f, formfeed, decimal 12
--
-- -   \\t, tab, decimal 9
--
-- -   \\n, newline, decimal 10
--
-- -   \\r, carriage return, decimal 13
--
-- -   \\v, vertical tab, decimal 11
--
-- -   non-breaking space, decimal 160
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
--
-- __HTML_ENTITY_DECODE__
--
-- Use this option to replace HTML-encoded characters with unencoded
-- characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
-- -   Replaces @(ampersand)quot;@ with @\"@
--
-- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
-- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
--
-- -   Replaces @(ampersand)gt;@ with @>@
--
-- -   Replaces characters that are represented in hexadecimal format,
--     @(ampersand)#xhhhh;@, with the corresponding characters
--
-- -   Replaces characters that are represented in decimal format,
--     @(ampersand)#nnnn;@, with the corresponding characters
--
-- __LOWERCASE__
--
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
--
-- __URL_DECODE__
--
-- Use this option to decode a URL-encoded value.
--
-- 'comparisonOperator', 'sizeConstraint_comparisonOperator' - The type of comparison you want AWS WAF to perform. AWS WAF uses this in
-- combination with the provided @Size@ and @FieldToMatch@ to build an
-- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
-- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
-- considered to match.
--
-- __EQ__: Used to test if the @Size@ is equal to the size of the
-- @FieldToMatch@
--
-- __NE__: Used to test if the @Size@ is not equal to the size of the
-- @FieldToMatch@
--
-- __LE__: Used to test if the @Size@ is less than or equal to the size of
-- the @FieldToMatch@
--
-- __LT__: Used to test if the @Size@ is strictly less than the size of the
-- @FieldToMatch@
--
-- __GE__: Used to test if the @Size@ is greater than or equal to the size
-- of the @FieldToMatch@
--
-- __GT__: Used to test if the @Size@ is strictly greater than the size of
-- the @FieldToMatch@
--
-- 'size', 'sizeConstraint_size' - The size in bytes that you want AWS WAF to compare against the size of
-- the specified @FieldToMatch@. AWS WAF uses this in combination with
-- @ComparisonOperator@ and @FieldToMatch@ to build an expression in the
-- form of \"@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@\".
-- If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
--
-- If you specify @URI@ for the value of @Type@, the \/ in the URI counts
-- as one character. For example, the URI @\/logo.jpg@ is nine characters
-- long.
newSizeConstraint ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'size'
  Prelude.Natural ->
  SizeConstraint
newSizeConstraint
  pFieldToMatch_
  pTextTransformation_
  pComparisonOperator_
  pSize_ =
    SizeConstraint'
      { fieldToMatch = pFieldToMatch_,
        textTransformation = pTextTransformation_,
        comparisonOperator = pComparisonOperator_,
        size = pSize_
      }

-- | Specifies where in a web request to look for the size constraint.
sizeConstraint_fieldToMatch :: Lens.Lens' SizeConstraint FieldToMatch
sizeConstraint_fieldToMatch = Lens.lens (\SizeConstraint' {fieldToMatch} -> fieldToMatch) (\s@SizeConstraint' {} a -> s {fieldToMatch = a} :: SizeConstraint)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
--
-- Note that if you choose @BODY@ for the value of @Type@, you must choose
-- @NONE@ for @TextTransformation@ because CloudFront forwards only the
-- first 8192 bytes for inspection.
--
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
--
-- __CMD_LINE__
--
-- When you\'re concerned that attackers are injecting an operating system
-- command line command and using unusual formatting to disguise some or
-- all of the command, use this option to perform the following
-- transformations:
--
-- -   Delete the following characters: \\ \" \' ^
--
-- -   Delete spaces before the following characters: \/ (
--
-- -   Replace the following characters with a space: , ;
--
-- -   Replace multiple spaces with one space
--
-- -   Convert uppercase letters (A-Z) to lowercase (a-z)
--
-- __COMPRESS_WHITE_SPACE__
--
-- Use this option to replace the following characters with a space
-- character (decimal 32):
--
-- -   \\f, formfeed, decimal 12
--
-- -   \\t, tab, decimal 9
--
-- -   \\n, newline, decimal 10
--
-- -   \\r, carriage return, decimal 13
--
-- -   \\v, vertical tab, decimal 11
--
-- -   non-breaking space, decimal 160
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
--
-- __HTML_ENTITY_DECODE__
--
-- Use this option to replace HTML-encoded characters with unencoded
-- characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
-- -   Replaces @(ampersand)quot;@ with @\"@
--
-- -   Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
-- -   Replaces @(ampersand)lt;@ with a \"less than\" symbol
--
-- -   Replaces @(ampersand)gt;@ with @>@
--
-- -   Replaces characters that are represented in hexadecimal format,
--     @(ampersand)#xhhhh;@, with the corresponding characters
--
-- -   Replaces characters that are represented in decimal format,
--     @(ampersand)#nnnn;@, with the corresponding characters
--
-- __LOWERCASE__
--
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
--
-- __URL_DECODE__
--
-- Use this option to decode a URL-encoded value.
sizeConstraint_textTransformation :: Lens.Lens' SizeConstraint TextTransformation
sizeConstraint_textTransformation = Lens.lens (\SizeConstraint' {textTransformation} -> textTransformation) (\s@SizeConstraint' {} a -> s {textTransformation = a} :: SizeConstraint)

-- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in
-- combination with the provided @Size@ and @FieldToMatch@ to build an
-- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
-- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
-- considered to match.
--
-- __EQ__: Used to test if the @Size@ is equal to the size of the
-- @FieldToMatch@
--
-- __NE__: Used to test if the @Size@ is not equal to the size of the
-- @FieldToMatch@
--
-- __LE__: Used to test if the @Size@ is less than or equal to the size of
-- the @FieldToMatch@
--
-- __LT__: Used to test if the @Size@ is strictly less than the size of the
-- @FieldToMatch@
--
-- __GE__: Used to test if the @Size@ is greater than or equal to the size
-- of the @FieldToMatch@
--
-- __GT__: Used to test if the @Size@ is strictly greater than the size of
-- the @FieldToMatch@
sizeConstraint_comparisonOperator :: Lens.Lens' SizeConstraint ComparisonOperator
sizeConstraint_comparisonOperator = Lens.lens (\SizeConstraint' {comparisonOperator} -> comparisonOperator) (\s@SizeConstraint' {} a -> s {comparisonOperator = a} :: SizeConstraint)

-- | The size in bytes that you want AWS WAF to compare against the size of
-- the specified @FieldToMatch@. AWS WAF uses this in combination with
-- @ComparisonOperator@ and @FieldToMatch@ to build an expression in the
-- form of \"@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@\".
-- If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
--
-- If you specify @URI@ for the value of @Type@, the \/ in the URI counts
-- as one character. For example, the URI @\/logo.jpg@ is nine characters
-- long.
sizeConstraint_size :: Lens.Lens' SizeConstraint Prelude.Natural
sizeConstraint_size = Lens.lens (\SizeConstraint' {size} -> size) (\s@SizeConstraint' {} a -> s {size = a} :: SizeConstraint)

instance Data.FromJSON SizeConstraint where
  parseJSON =
    Data.withObject
      "SizeConstraint"
      ( \x ->
          SizeConstraint'
            Prelude.<$> (x Data..: "FieldToMatch")
            Prelude.<*> (x Data..: "TextTransformation")
            Prelude.<*> (x Data..: "ComparisonOperator")
            Prelude.<*> (x Data..: "Size")
      )

instance Prelude.Hashable SizeConstraint where
  hashWithSalt _salt SizeConstraint' {..} =
    _salt `Prelude.hashWithSalt` fieldToMatch
      `Prelude.hashWithSalt` textTransformation
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` size

instance Prelude.NFData SizeConstraint where
  rnf SizeConstraint' {..} =
    Prelude.rnf fieldToMatch
      `Prelude.seq` Prelude.rnf textTransformation
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf size

instance Data.ToJSON SizeConstraint where
  toJSON SizeConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldToMatch" Data..= fieldToMatch),
            Prelude.Just
              ("TextTransformation" Data..= textTransformation),
            Prelude.Just
              ("ComparisonOperator" Data..= comparisonOperator),
            Prelude.Just ("Size" Data..= size)
          ]
      )
