{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchTuple where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
--
--
--
-- /See:/ 'xssMatchTuple' smart constructor.
data XSSMatchTuple = XSSMatchTuple'
  { _xmtFieldToMatch ::
      !FieldToMatch,
    _xmtTextTransformation :: !TextTransformation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'XSSMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmtFieldToMatch' - Specifies where in a web request to look for cross-site scripting attacks.
--
-- * 'xmtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
xssMatchTuple ::
  -- | 'xmtFieldToMatch'
  FieldToMatch ->
  -- | 'xmtTextTransformation'
  TextTransformation ->
  XSSMatchTuple
xssMatchTuple pFieldToMatch_ pTextTransformation_ =
  XSSMatchTuple'
    { _xmtFieldToMatch = pFieldToMatch_,
      _xmtTextTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for cross-site scripting attacks.
xmtFieldToMatch :: Lens' XSSMatchTuple FieldToMatch
xmtFieldToMatch = lens _xmtFieldToMatch (\s a -> s {_xmtFieldToMatch = a})

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
xmtTextTransformation :: Lens' XSSMatchTuple TextTransformation
xmtTextTransformation = lens _xmtTextTransformation (\s a -> s {_xmtTextTransformation = a})

instance FromJSON XSSMatchTuple where
  parseJSON =
    withObject
      "XSSMatchTuple"
      ( \x ->
          XSSMatchTuple'
            <$> (x .: "FieldToMatch") <*> (x .: "TextTransformation")
      )

instance Hashable XSSMatchTuple

instance NFData XSSMatchTuple

instance ToJSON XSSMatchTuple where
  toJSON XSSMatchTuple' {..} =
    object
      ( catMaybes
          [ Just ("FieldToMatch" .= _xmtFieldToMatch),
            Just ("TextTransformation" .= _xmtTextTransformation)
          ]
      )
