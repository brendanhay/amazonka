-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchTuple
  ( RegexMatchTuple (..),

    -- * Smart constructor
    mkRegexMatchTuple,

    -- * Lenses
    rmtFieldToMatch,
    rmtTextTransformation,
    rmtRegexPatternSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | The regular expression pattern that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings. Each @RegexMatchTuple@ object contains:
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
-- /See:/ 'mkRegexMatchTuple' smart constructor.
data RegexMatchTuple = RegexMatchTuple'
  { fieldToMatch ::
      FieldToMatch,
    textTransformation :: TextTransformation,
    regexPatternSetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegexMatchTuple' with the minimum fields required to make a request.
--
-- * 'fieldToMatch' - Specifies where in a web request to look for the @RegexPatternSet@ .
-- * 'regexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ).
--
-- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
-- * 'textTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @RegexPatternSet@ before inspecting a request for a match.
--
-- You can only specify a single type of TextTransformation.
-- __CMD_LINE__
-- When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
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
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
mkRegexMatchTuple ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  -- | 'regexPatternSetId'
  Lude.Text ->
  RegexMatchTuple
mkRegexMatchTuple
  pFieldToMatch_
  pTextTransformation_
  pRegexPatternSetId_ =
    RegexMatchTuple'
      { fieldToMatch = pFieldToMatch_,
        textTransformation = pTextTransformation_,
        regexPatternSetId = pRegexPatternSetId_
      }

-- | Specifies where in a web request to look for the @RegexPatternSet@ .
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmtFieldToMatch :: Lens.Lens' RegexMatchTuple FieldToMatch
rmtFieldToMatch = Lens.lens (fieldToMatch :: RegexMatchTuple -> FieldToMatch) (\s a -> s {fieldToMatch = a} :: RegexMatchTuple)
{-# DEPRECATED rmtFieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead." #-}

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @RegexPatternSet@ before inspecting a request for a match.
--
-- You can only specify a single type of TextTransformation.
-- __CMD_LINE__
-- When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:
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
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
--
-- /Note:/ Consider using 'textTransformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmtTextTransformation :: Lens.Lens' RegexMatchTuple TextTransformation
rmtTextTransformation = Lens.lens (textTransformation :: RegexMatchTuple -> TextTransformation) (\s a -> s {textTransformation = a} :: RegexMatchTuple)
{-# DEPRECATED rmtTextTransformation "Use generic-lens or generic-optics with 'textTransformation' instead." #-}

-- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ).
--
-- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmtRegexPatternSetId :: Lens.Lens' RegexMatchTuple Lude.Text
rmtRegexPatternSetId = Lens.lens (regexPatternSetId :: RegexMatchTuple -> Lude.Text) (\s a -> s {regexPatternSetId = a} :: RegexMatchTuple)
{-# DEPRECATED rmtRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

instance Lude.FromJSON RegexMatchTuple where
  parseJSON =
    Lude.withObject
      "RegexMatchTuple"
      ( \x ->
          RegexMatchTuple'
            Lude.<$> (x Lude..: "FieldToMatch")
            Lude.<*> (x Lude..: "TextTransformation")
            Lude.<*> (x Lude..: "RegexPatternSetId")
      )

instance Lude.ToJSON RegexMatchTuple where
  toJSON RegexMatchTuple' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FieldToMatch" Lude..= fieldToMatch),
            Lude.Just ("TextTransformation" Lude..= textTransformation),
            Lude.Just ("RegexPatternSetId" Lude..= regexPatternSetId)
          ]
      )
