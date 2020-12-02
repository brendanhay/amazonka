{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraint where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ComparisonOperator
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.TextTransformation

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
--
--
-- /See:/ 'sizeConstraint' smart constructor.
data SizeConstraint = SizeConstraint'
  { _scFieldToMatch ::
      !FieldToMatch,
    _scTextTransformation :: !TextTransformation,
    _scComparisonOperator :: !ComparisonOperator,
    _scSize :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SizeConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scFieldToMatch' - Specifies where in a web request to look for the size constraint.
--
-- * 'scTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.  __NONE__  Specify @NONE@ if you don't want to perform any text transformations. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value.
--
-- * 'scComparisonOperator' - The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@  __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@  __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@  __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@  __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@  __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
--
-- * 'scSize' - The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. Valid values for size are 0 - 21474836480 bytes (0 - 20 GB). If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
sizeConstraint ::
  -- | 'scFieldToMatch'
  FieldToMatch ->
  -- | 'scTextTransformation'
  TextTransformation ->
  -- | 'scComparisonOperator'
  ComparisonOperator ->
  -- | 'scSize'
  Natural ->
  SizeConstraint
sizeConstraint
  pFieldToMatch_
  pTextTransformation_
  pComparisonOperator_
  pSize_ =
    SizeConstraint'
      { _scFieldToMatch = pFieldToMatch_,
        _scTextTransformation = pTextTransformation_,
        _scComparisonOperator = pComparisonOperator_,
        _scSize = _Nat # pSize_
      }

-- | Specifies where in a web request to look for the size constraint.
scFieldToMatch :: Lens' SizeConstraint FieldToMatch
scFieldToMatch = lens _scFieldToMatch (\s a -> s {_scFieldToMatch = a})

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match. You can only specify a single type of TextTransformation. Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.  __NONE__  Specify @NONE@ if you don't want to perform any text transformations. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value.
scTextTransformation :: Lens' SizeConstraint TextTransformation
scTextTransformation = lens _scTextTransformation (\s a -> s {_scTextTransformation = a})

-- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@  __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@  __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@  __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@  __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@  __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
scComparisonOperator :: Lens' SizeConstraint ComparisonOperator
scComparisonOperator = lens _scComparisonOperator (\s a -> s {_scComparisonOperator = a})

-- | The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. Valid values for size are 0 - 21474836480 bytes (0 - 20 GB). If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
scSize :: Lens' SizeConstraint Natural
scSize = lens _scSize (\s a -> s {_scSize = a}) . _Nat

instance FromJSON SizeConstraint where
  parseJSON =
    withObject
      "SizeConstraint"
      ( \x ->
          SizeConstraint'
            <$> (x .: "FieldToMatch")
            <*> (x .: "TextTransformation")
            <*> (x .: "ComparisonOperator")
            <*> (x .: "Size")
      )

instance Hashable SizeConstraint

instance NFData SizeConstraint

instance ToJSON SizeConstraint where
  toJSON SizeConstraint' {..} =
    object
      ( catMaybes
          [ Just ("FieldToMatch" .= _scFieldToMatch),
            Just ("TextTransformation" .= _scTextTransformation),
            Just ("ComparisonOperator" .= _scComparisonOperator),
            Just ("Size" .= _scSize)
          ]
      )
