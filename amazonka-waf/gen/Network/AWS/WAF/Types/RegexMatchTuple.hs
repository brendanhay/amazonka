{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.Types.RegexMatchTuple
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchTuple where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The regular expression pattern that you want AWS WAF to search for in
-- web requests, the location in requests that you want AWS WAF to search,
-- and other settings. Each @RegexMatchTuple@ object contains:
--
-- -   The part of a web request that you want AWS WAF to inspect, such as
--     a query string or the value of the @User-Agent@ header.
--
-- -   The identifier of the pattern (a regular expression) that you want
--     AWS WAF to look for. For more information, see RegexPatternSet.
--
-- -   Whether to perform any conversions on the request, such as
--     converting it to lowercase, before inspecting it for the specified
--     string.
--
-- /See:/ 'newRegexMatchTuple' smart constructor.
data RegexMatchTuple = RegexMatchTuple'
  { -- | Specifies where in a web request to look for the @RegexPatternSet@.
    fieldToMatch :: FieldToMatch,
    -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass AWS WAF. If you
    -- specify a transformation, AWS WAF performs the transformation on
    -- @RegexPatternSet@ before inspecting a request for a match.
    --
    -- You can only specify a single type of TextTransformation.
    --
    -- __CMD_LINE__
    --
    -- When you\'re concerned that attackers are injecting an operating system
    -- commandline command and using unusual formatting to disguise some or all
    -- of the command, use this option to perform the following
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
    -- __NONE__
    --
    -- Specify @NONE@ if you don\'t want to perform any text transformations.
    textTransformation :: TextTransformation,
    -- | The @RegexPatternSetId@ for a @RegexPatternSet@. You use
    -- @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see
    -- GetRegexPatternSet), update a @RegexPatternSet@ (see
    -- UpdateRegexPatternSet), insert a @RegexPatternSet@ into a
    -- @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see
    -- UpdateRegexMatchSet), and delete an @RegexPatternSet@ from AWS WAF (see
    -- DeleteRegexPatternSet).
    --
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexMatchTuple' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldToMatch', 'regexMatchTuple_fieldToMatch' - Specifies where in a web request to look for the @RegexPatternSet@.
--
-- 'textTransformation', 'regexMatchTuple_textTransformation' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @RegexPatternSet@ before inspecting a request for a match.
--
-- You can only specify a single type of TextTransformation.
--
-- __CMD_LINE__
--
-- When you\'re concerned that attackers are injecting an operating system
-- commandline command and using unusual formatting to disguise some or all
-- of the command, use this option to perform the following
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
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
--
-- 'regexPatternSetId', 'regexMatchTuple_regexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@. You use
-- @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see
-- GetRegexPatternSet), update a @RegexPatternSet@ (see
-- UpdateRegexPatternSet), insert a @RegexPatternSet@ into a
-- @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see
-- UpdateRegexMatchSet), and delete an @RegexPatternSet@ from AWS WAF (see
-- DeleteRegexPatternSet).
--
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
newRegexMatchTuple ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  -- | 'regexPatternSetId'
  Prelude.Text ->
  RegexMatchTuple
newRegexMatchTuple
  pFieldToMatch_
  pTextTransformation_
  pRegexPatternSetId_ =
    RegexMatchTuple'
      { fieldToMatch = pFieldToMatch_,
        textTransformation = pTextTransformation_,
        regexPatternSetId = pRegexPatternSetId_
      }

-- | Specifies where in a web request to look for the @RegexPatternSet@.
regexMatchTuple_fieldToMatch :: Lens.Lens' RegexMatchTuple FieldToMatch
regexMatchTuple_fieldToMatch = Lens.lens (\RegexMatchTuple' {fieldToMatch} -> fieldToMatch) (\s@RegexMatchTuple' {} a -> s {fieldToMatch = a} :: RegexMatchTuple)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @RegexPatternSet@ before inspecting a request for a match.
--
-- You can only specify a single type of TextTransformation.
--
-- __CMD_LINE__
--
-- When you\'re concerned that attackers are injecting an operating system
-- commandline command and using unusual formatting to disguise some or all
-- of the command, use this option to perform the following
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
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
regexMatchTuple_textTransformation :: Lens.Lens' RegexMatchTuple TextTransformation
regexMatchTuple_textTransformation = Lens.lens (\RegexMatchTuple' {textTransformation} -> textTransformation) (\s@RegexMatchTuple' {} a -> s {textTransformation = a} :: RegexMatchTuple)

-- | The @RegexPatternSetId@ for a @RegexPatternSet@. You use
-- @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see
-- GetRegexPatternSet), update a @RegexPatternSet@ (see
-- UpdateRegexPatternSet), insert a @RegexPatternSet@ into a
-- @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see
-- UpdateRegexMatchSet), and delete an @RegexPatternSet@ from AWS WAF (see
-- DeleteRegexPatternSet).
--
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
regexMatchTuple_regexPatternSetId :: Lens.Lens' RegexMatchTuple Prelude.Text
regexMatchTuple_regexPatternSetId = Lens.lens (\RegexMatchTuple' {regexPatternSetId} -> regexPatternSetId) (\s@RegexMatchTuple' {} a -> s {regexPatternSetId = a} :: RegexMatchTuple)

instance Prelude.FromJSON RegexMatchTuple where
  parseJSON =
    Prelude.withObject
      "RegexMatchTuple"
      ( \x ->
          RegexMatchTuple'
            Prelude.<$> (x Prelude..: "FieldToMatch")
            Prelude.<*> (x Prelude..: "TextTransformation")
            Prelude.<*> (x Prelude..: "RegexPatternSetId")
      )

instance Prelude.Hashable RegexMatchTuple

instance Prelude.NFData RegexMatchTuple

instance Prelude.ToJSON RegexMatchTuple where
  toJSON RegexMatchTuple' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FieldToMatch" Prelude..= fieldToMatch),
            Prelude.Just
              ("TextTransformation" Prelude..= textTransformation),
            Prelude.Just
              ("RegexPatternSetId" Prelude..= regexPatternSetId)
          ]
      )
