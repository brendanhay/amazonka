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
-- Module      : Network.AWS.WAFRegional.Types.XssMatchTuple
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.XssMatchTuple where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.FieldToMatch
import Network.AWS.WAFRegional.Types.TextTransformation

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the part of a web request that you want AWS WAF to inspect for
-- cross-site scripting attacks and, if you want AWS WAF to inspect a
-- header, the name of the header.
--
-- /See:/ 'newXssMatchTuple' smart constructor.
data XssMatchTuple = XssMatchTuple'
  { -- | Specifies where in a web request to look for cross-site scripting
    -- attacks.
    fieldToMatch :: FieldToMatch,
    -- | Text transformations eliminate some of the unusual formatting that
    -- attackers use in web requests in an effort to bypass AWS WAF. If you
    -- specify a transformation, AWS WAF performs the transformation on
    -- @FieldToMatch@ before inspecting it for a match.
    --
    -- You can only specify a single type of TextTransformation.
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
    -- __NONE__
    --
    -- Specify @NONE@ if you don\'t want to perform any text transformations.
    textTransformation :: TextTransformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'XssMatchTuple' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldToMatch', 'xssMatchTuple_fieldToMatch' - Specifies where in a web request to look for cross-site scripting
-- attacks.
--
-- 'textTransformation', 'xssMatchTuple_textTransformation' - Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
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
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
newXssMatchTuple ::
  -- | 'fieldToMatch'
  FieldToMatch ->
  -- | 'textTransformation'
  TextTransformation ->
  XssMatchTuple
newXssMatchTuple pFieldToMatch_ pTextTransformation_ =
  XssMatchTuple'
    { fieldToMatch = pFieldToMatch_,
      textTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for cross-site scripting
-- attacks.
xssMatchTuple_fieldToMatch :: Lens.Lens' XssMatchTuple FieldToMatch
xssMatchTuple_fieldToMatch = Lens.lens (\XssMatchTuple' {fieldToMatch} -> fieldToMatch) (\s@XssMatchTuple' {} a -> s {fieldToMatch = a} :: XssMatchTuple)

-- | Text transformations eliminate some of the unusual formatting that
-- attackers use in web requests in an effort to bypass AWS WAF. If you
-- specify a transformation, AWS WAF performs the transformation on
-- @FieldToMatch@ before inspecting it for a match.
--
-- You can only specify a single type of TextTransformation.
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
-- __NONE__
--
-- Specify @NONE@ if you don\'t want to perform any text transformations.
xssMatchTuple_textTransformation :: Lens.Lens' XssMatchTuple TextTransformation
xssMatchTuple_textTransformation = Lens.lens (\XssMatchTuple' {textTransformation} -> textTransformation) (\s@XssMatchTuple' {} a -> s {textTransformation = a} :: XssMatchTuple)

instance Prelude.FromJSON XssMatchTuple where
  parseJSON =
    Prelude.withObject
      "XssMatchTuple"
      ( \x ->
          XssMatchTuple'
            Prelude.<$> (x Prelude..: "FieldToMatch")
            Prelude.<*> (x Prelude..: "TextTransformation")
      )

instance Prelude.Hashable XssMatchTuple

instance Prelude.NFData XssMatchTuple

instance Prelude.ToJSON XssMatchTuple where
  toJSON XssMatchTuple' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FieldToMatch" Prelude..= fieldToMatch),
            Prelude.Just
              ( "TextTransformation"
                  Prelude..= textTransformation
              )
          ]
      )
