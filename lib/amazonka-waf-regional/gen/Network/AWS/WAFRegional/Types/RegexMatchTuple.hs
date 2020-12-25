{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchTuple
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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.FieldToMatch as Types
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.TextTransformation as Types

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
  { -- | Specifies where in a web request to look for the @RegexPatternSet@ .
    fieldToMatch :: Types.FieldToMatch,
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
    textTransformation :: Types.TextTransformation,
    -- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ).
    --
    -- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
    regexPatternSetId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegexMatchTuple' value with any optional fields omitted.
mkRegexMatchTuple ::
  -- | 'fieldToMatch'
  Types.FieldToMatch ->
  -- | 'textTransformation'
  Types.TextTransformation ->
  -- | 'regexPatternSetId'
  Types.ResourceId ->
  RegexMatchTuple
mkRegexMatchTuple fieldToMatch textTransformation regexPatternSetId =
  RegexMatchTuple'
    { fieldToMatch,
      textTransformation,
      regexPatternSetId
    }

-- | Specifies where in a web request to look for the @RegexPatternSet@ .
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmtFieldToMatch :: Lens.Lens' RegexMatchTuple Types.FieldToMatch
rmtFieldToMatch = Lens.field @"fieldToMatch"
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
rmtTextTransformation :: Lens.Lens' RegexMatchTuple Types.TextTransformation
rmtTextTransformation = Lens.field @"textTransformation"
{-# DEPRECATED rmtTextTransformation "Use generic-lens or generic-optics with 'textTransformation' instead." #-}

-- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ).
--
-- @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmtRegexPatternSetId :: Lens.Lens' RegexMatchTuple Types.ResourceId
rmtRegexPatternSetId = Lens.field @"regexPatternSetId"
{-# DEPRECATED rmtRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

instance Core.FromJSON RegexMatchTuple where
  toJSON RegexMatchTuple {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FieldToMatch" Core..= fieldToMatch),
            Core.Just ("TextTransformation" Core..= textTransformation),
            Core.Just ("RegexPatternSetId" Core..= regexPatternSetId)
          ]
      )

instance Core.FromJSON RegexMatchTuple where
  parseJSON =
    Core.withObject "RegexMatchTuple" Core.$
      \x ->
        RegexMatchTuple'
          Core.<$> (x Core..: "FieldToMatch")
          Core.<*> (x Core..: "TextTransformation")
          Core.<*> (x Core..: "RegexPatternSetId")
