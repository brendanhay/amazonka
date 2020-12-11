-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraint
  ( SizeConstraint (..),

    -- * Smart constructor
    mkSizeConstraint,

    -- * Lenses
    scFieldToMatch,
    scTextTransformation,
    scComparisonOperator,
    scSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ComparisonOperator
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- /See:/ 'mkSizeConstraint' smart constructor.
data SizeConstraint = SizeConstraint'
  { fieldToMatch :: FieldToMatch,
    textTransformation :: TextTransformation,
    comparisonOperator :: ComparisonOperator,
    size :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SizeConstraint' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@
-- __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@
-- __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@
-- __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@
-- __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@
-- __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
-- * 'fieldToMatch' - Specifies where in a web request to look for the size constraint.
-- * 'size' - The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
-- If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
-- * 'textTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
-- Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
-- __CMD_LINE__
-- When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
--
--     * Delete the following characters: \ " ' ^
--
--
--     * Delete spaces before the following characters: / (
--
--
--     * Replace the following characters with a space: , ;
--
--
--     * Replace multiple spaces with one space
--
--
--     * Convert uppercase letters (A-Z) to lowercase (a-z)
--
--
-- __COMPRESS_WHITE_SPACE__
-- Use this option to replace the following characters with a space character (decimal 32):
--
--     * \f, formfeed, decimal 12
--
--
--     * \t, tab, decimal 9
--
--
--     * \n, newline, decimal 10
--
--
--     * \r, carriage return, decimal 13
--
--
--     * \v, vertical tab, decimal 11
--
--
--     * non-breaking space, decimal 160
--
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
-- __HTML_ENTITY_DECODE__
-- Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
--     * Replaces @(ampersand)quot;@ with @"@
--
--
--     * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
--
--     * Replaces @(ampersand)lt;@ with a "less than" symbol
--
--
--     * Replaces @(ampersand)gt;@ with @>@
--
--
--     * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters
--
--
--     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters
--
--
-- __LOWERCASE__
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
-- __URL_DECODE__
-- Use this option to decode a URL-encoded value.
mkSizeConstraint ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'size'
  Lude.Natural ->
  SizeConstraint
mkSizeConstraint
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
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFieldToMatch :: Lens.Lens' SizeConstraint FieldToMatch
scFieldToMatch = Lens.lens (fieldToMatch :: SizeConstraint -> FieldToMatch) (\s a -> s {fieldToMatch = a} :: SizeConstraint)
{-# DEPRECATED scFieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead." #-}

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
-- Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
-- __CMD_LINE__
-- When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
--
--     * Delete the following characters: \ " ' ^
--
--
--     * Delete spaces before the following characters: / (
--
--
--     * Replace the following characters with a space: , ;
--
--
--     * Replace multiple spaces with one space
--
--
--     * Convert uppercase letters (A-Z) to lowercase (a-z)
--
--
-- __COMPRESS_WHITE_SPACE__
-- Use this option to replace the following characters with a space character (decimal 32):
--
--     * \f, formfeed, decimal 12
--
--
--     * \t, tab, decimal 9
--
--
--     * \n, newline, decimal 10
--
--
--     * \r, carriage return, decimal 13
--
--
--     * \v, vertical tab, decimal 11
--
--
--     * non-breaking space, decimal 160
--
--
-- @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space.
-- __HTML_ENTITY_DECODE__
-- Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:
--
--     * Replaces @(ampersand)quot;@ with @"@
--
--
--     * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160
--
--
--     * Replaces @(ampersand)lt;@ with a "less than" symbol
--
--
--     * Replaces @(ampersand)gt;@ with @>@
--
--
--     * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters
--
--
--     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters
--
--
-- __LOWERCASE__
-- Use this option to convert uppercase letters (A-Z) to lowercase (a-z).
-- __URL_DECODE__
-- Use this option to decode a URL-encoded value.
--
-- /Note:/ Consider using 'textTransformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTextTransformation :: Lens.Lens' SizeConstraint TextTransformation
scTextTransformation = Lens.lens (textTransformation :: SizeConstraint -> TextTransformation) (\s a -> s {textTransformation = a} :: SizeConstraint)
{-# DEPRECATED scTextTransformation "Use generic-lens or generic-optics with 'textTransformation' instead." #-}

-- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@
-- __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@
-- __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@
-- __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@
-- __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@
-- __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scComparisonOperator :: Lens.Lens' SizeConstraint ComparisonOperator
scComparisonOperator = Lens.lens (comparisonOperator :: SizeConstraint -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: SizeConstraint)
{-# DEPRECATED scComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- Valid values for size are 0 - 21474836480 bytes (0 - 20 GB).
-- If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSize :: Lens.Lens' SizeConstraint Lude.Natural
scSize = Lens.lens (size :: SizeConstraint -> Lude.Natural) (\s a -> s {size = a} :: SizeConstraint)
{-# DEPRECATED scSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.FromJSON SizeConstraint where
  parseJSON =
    Lude.withObject
      "SizeConstraint"
      ( \x ->
          SizeConstraint'
            Lude.<$> (x Lude..: "FieldToMatch")
            Lude.<*> (x Lude..: "TextTransformation")
            Lude.<*> (x Lude..: "ComparisonOperator")
            Lude.<*> (x Lude..: "Size")
      )

instance Lude.ToJSON SizeConstraint where
  toJSON SizeConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FieldToMatch" Lude..= fieldToMatch),
            Lude.Just ("TextTransformation" Lude..= textTransformation),
            Lude.Just ("ComparisonOperator" Lude..= comparisonOperator),
            Lude.Just ("Size" Lude..= size)
          ]
      )
