{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchTuple
  ( SqlInjectionMatchTuple (..),

    -- * Smart constructor
    mkSqlInjectionMatchTuple,

    -- * Lenses
    simtFieldToMatch,
    simtTextTransformation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.FieldToMatch as Types
import qualified Network.AWS.WAF.Types.TextTransformation as Types

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
-- /See:/ 'mkSqlInjectionMatchTuple' smart constructor.
data SqlInjectionMatchTuple = SqlInjectionMatchTuple'
  { -- | Specifies where in a web request to look for snippets of malicious SQL code.
    fieldToMatch :: Types.FieldToMatch,
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
    textTransformation :: Types.TextTransformation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqlInjectionMatchTuple' value with any optional fields omitted.
mkSqlInjectionMatchTuple ::
  -- | 'fieldToMatch'
  Types.FieldToMatch ->
  -- | 'textTransformation'
  Types.TextTransformation ->
  SqlInjectionMatchTuple
mkSqlInjectionMatchTuple fieldToMatch textTransformation =
  SqlInjectionMatchTuple' {fieldToMatch, textTransformation}

-- | Specifies where in a web request to look for snippets of malicious SQL code.
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simtFieldToMatch :: Lens.Lens' SqlInjectionMatchTuple Types.FieldToMatch
simtFieldToMatch = Lens.field @"fieldToMatch"
{-# DEPRECATED simtFieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead." #-}

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
simtTextTransformation :: Lens.Lens' SqlInjectionMatchTuple Types.TextTransformation
simtTextTransformation = Lens.field @"textTransformation"
{-# DEPRECATED simtTextTransformation "Use generic-lens or generic-optics with 'textTransformation' instead." #-}

instance Core.FromJSON SqlInjectionMatchTuple where
  toJSON SqlInjectionMatchTuple {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FieldToMatch" Core..= fieldToMatch),
            Core.Just ("TextTransformation" Core..= textTransformation)
          ]
      )

instance Core.FromJSON SqlInjectionMatchTuple where
  parseJSON =
    Core.withObject "SqlInjectionMatchTuple" Core.$
      \x ->
        SqlInjectionMatchTuple'
          Core.<$> (x Core..: "FieldToMatch")
          Core.<*> (x Core..: "TextTransformation")
