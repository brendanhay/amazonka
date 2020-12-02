{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchTuple where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | The regular expression pattern that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings. Each @RegexMatchTuple@ object contains:
--
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
--
-- /See:/ 'regexMatchTuple' smart constructor.
data RegexMatchTuple = RegexMatchTuple'
  { _rmtFieldToMatch ::
      !FieldToMatch,
    _rmtTextTransformation :: !TextTransformation,
    _rmtRegexPatternSetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmtFieldToMatch' - Specifies where in a web request to look for the @RegexPatternSet@ .
--
-- * 'rmtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @RegexPatternSet@ before inspecting a request for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
--
-- * 'rmtRegexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ). @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
regexMatchTuple ::
  -- | 'rmtFieldToMatch'
  FieldToMatch ->
  -- | 'rmtTextTransformation'
  TextTransformation ->
  -- | 'rmtRegexPatternSetId'
  Text ->
  RegexMatchTuple
regexMatchTuple
  pFieldToMatch_
  pTextTransformation_
  pRegexPatternSetId_ =
    RegexMatchTuple'
      { _rmtFieldToMatch = pFieldToMatch_,
        _rmtTextTransformation = pTextTransformation_,
        _rmtRegexPatternSetId = pRegexPatternSetId_
      }

-- | Specifies where in a web request to look for the @RegexPatternSet@ .
rmtFieldToMatch :: Lens' RegexMatchTuple FieldToMatch
rmtFieldToMatch = lens _rmtFieldToMatch (\s a -> s {_rmtFieldToMatch = a})

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @RegexPatternSet@ before inspecting a request for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
rmtTextTransformation :: Lens' RegexMatchTuple TextTransformation
rmtTextTransformation = lens _rmtTextTransformation (\s a -> s {_rmtTextTransformation = a})

-- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ (see 'GetRegexPatternSet' ), update a @RegexPatternSet@ (see 'UpdateRegexPatternSet' ), insert a @RegexPatternSet@ into a @RegexMatchSet@ or delete one from a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), and delete an @RegexPatternSet@ from AWS WAF (see 'DeleteRegexPatternSet' ). @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
rmtRegexPatternSetId :: Lens' RegexMatchTuple Text
rmtRegexPatternSetId = lens _rmtRegexPatternSetId (\s a -> s {_rmtRegexPatternSetId = a})

instance FromJSON RegexMatchTuple where
  parseJSON =
    withObject
      "RegexMatchTuple"
      ( \x ->
          RegexMatchTuple'
            <$> (x .: "FieldToMatch")
            <*> (x .: "TextTransformation")
            <*> (x .: "RegexPatternSetId")
      )

instance Hashable RegexMatchTuple

instance NFData RegexMatchTuple

instance ToJSON RegexMatchTuple where
  toJSON RegexMatchTuple' {..} =
    object
      ( catMaybes
          [ Just ("FieldToMatch" .= _rmtFieldToMatch),
            Just ("TextTransformation" .= _rmtTextTransformation),
            Just ("RegexPatternSetId" .= _rmtRegexPatternSetId)
          ]
      )
