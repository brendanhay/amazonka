{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchTuple where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
--
--
-- /See:/ 'sqlInjectionMatchTuple' smart constructor.
data SqlInjectionMatchTuple = SqlInjectionMatchTuple'
  { _simtFieldToMatch ::
      !FieldToMatch,
    _simtTextTransformation ::
      !TextTransformation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqlInjectionMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simtFieldToMatch' - Specifies where in a web request to look for snippets of malicious SQL code.
--
-- * 'simtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
sqlInjectionMatchTuple ::
  -- | 'simtFieldToMatch'
  FieldToMatch ->
  -- | 'simtTextTransformation'
  TextTransformation ->
  SqlInjectionMatchTuple
sqlInjectionMatchTuple pFieldToMatch_ pTextTransformation_ =
  SqlInjectionMatchTuple'
    { _simtFieldToMatch = pFieldToMatch_,
      _simtTextTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for snippets of malicious SQL code.
simtFieldToMatch :: Lens' SqlInjectionMatchTuple FieldToMatch
simtFieldToMatch = lens _simtFieldToMatch (\s a -> s {_simtFieldToMatch = a})

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
simtTextTransformation :: Lens' SqlInjectionMatchTuple TextTransformation
simtTextTransformation = lens _simtTextTransformation (\s a -> s {_simtTextTransformation = a})

instance FromJSON SqlInjectionMatchTuple where
  parseJSON =
    withObject
      "SqlInjectionMatchTuple"
      ( \x ->
          SqlInjectionMatchTuple'
            <$> (x .: "FieldToMatch") <*> (x .: "TextTransformation")
      )

instance Hashable SqlInjectionMatchTuple

instance NFData SqlInjectionMatchTuple

instance ToJSON SqlInjectionMatchTuple where
  toJSON SqlInjectionMatchTuple' {..} =
    object
      ( catMaybes
          [ Just ("FieldToMatch" .= _simtFieldToMatch),
            Just ("TextTransformation" .= _simtTextTransformation)
          ]
      )
