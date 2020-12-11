-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchTuple
  ( XSSMatchTuple (..),

    -- * Smart constructor
    mkXSSMatchTuple,

    -- * Lenses
    xmtFieldToMatch,
    xmtTextTransformation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /See:/ 'mkXSSMatchTuple' smart constructor.
data XSSMatchTuple = XSSMatchTuple'
  { fieldToMatch :: FieldToMatch,
    textTransformation :: TextTransformation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'XSSMatchTuple' with the minimum fields required to make a request.
--
-- * 'fieldToMatch' - Specifies where in a web request to look for cross-site scripting attacks.
-- * 'textTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
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
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
mkXSSMatchTuple ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  XSSMatchTuple
mkXSSMatchTuple pFieldToMatch_ pTextTransformation_ =
  XSSMatchTuple'
    { fieldToMatch = pFieldToMatch_,
      textTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for cross-site scripting attacks.
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmtFieldToMatch :: Lens.Lens' XSSMatchTuple FieldToMatch
xmtFieldToMatch = Lens.lens (fieldToMatch :: XSSMatchTuple -> FieldToMatch) (\s a -> s {fieldToMatch = a} :: XSSMatchTuple)
{-# DEPRECATED xmtFieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead." #-}

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
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
-- __NONE__
-- Specify @NONE@ if you don't want to perform any text transformations.
--
-- /Note:/ Consider using 'textTransformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
xmtTextTransformation :: Lens.Lens' XSSMatchTuple TextTransformation
xmtTextTransformation = Lens.lens (textTransformation :: XSSMatchTuple -> TextTransformation) (\s a -> s {textTransformation = a} :: XSSMatchTuple)
{-# DEPRECATED xmtTextTransformation "Use generic-lens or generic-optics with 'textTransformation' instead." #-}

instance Lude.FromJSON XSSMatchTuple where
  parseJSON =
    Lude.withObject
      "XSSMatchTuple"
      ( \x ->
          XSSMatchTuple'
            Lude.<$> (x Lude..: "FieldToMatch")
            Lude.<*> (x Lude..: "TextTransformation")
      )

instance Lude.ToJSON XSSMatchTuple where
  toJSON XSSMatchTuple' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FieldToMatch" Lude..= fieldToMatch),
            Lude.Just ("TextTransformation" Lude..= textTransformation)
          ]
      )
