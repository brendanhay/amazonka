{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.ByteMatchTuple
  ( ByteMatchTuple (..)
  -- * Smart constructor
  , mkByteMatchTuple
  -- * Lenses
  , bmtFieldToMatch
  , bmtTargetString
  , bmtTextTransformation
  , bmtPositionalConstraint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.FieldToMatch as Types
import qualified Network.AWS.WAFRegional.Types.PositionalConstraint as Types
import qualified Network.AWS.WAFRegional.Types.TextTransformation as Types

-- | The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
--
-- /See:/ 'mkByteMatchTuple' smart constructor.
data ByteMatchTuple = ByteMatchTuple'
  { fieldToMatch :: Types.FieldToMatch
    -- ^ The part of a web request that you want AWS WAF to search, such as a specified header or a query string. For more information, see 'FieldToMatch' .
  , targetString :: Core.Base64
    -- ^ The value that you want AWS WAF to search for. AWS WAF searches for the specified string in the part of web requests that you specified in @FieldToMatch@ . The maximum length of the value is 50 bytes.
--
-- Valid values depend on the values that you specified for @FieldToMatch@ :
--
--     * @HEADER@ : The value that you want AWS WAF to search for in the request header that you specified in 'FieldToMatch' , for example, the value of the @User-Agent@ or @Referer@ header.
--
--
--     * @METHOD@ : The HTTP method, which indicates the type of operation specified in the request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
--
--     * @QUERY_STRING@ : The value that you want AWS WAF to search for in the query string, which is the part of a URL that appears after a @?@ character.
--
--
--     * @URI@ : The value that you want AWS WAF to search for in the part of a URL that identifies a resource, for example, @/images/daily-ad.jpg@ .
--
--
--     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' . 
--
--
--     * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.
--
--
--     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but instead of inspecting a single parameter, AWS WAF inspects all parameters within the query string for the value or regex pattern that you specify in @TargetString@ .
--
--
-- If @TargetString@ includes alphabetic characters A-Z and a-z, note that the value is case sensitive.
-- __If you're using the AWS WAF API__ 
-- Specify a base64-encoded version of the value. The maximum length of the value before you base64-encode it is 50 bytes.
-- For example, suppose the value of @Type@ is @HEADER@ and the value of @Data@ is @User-Agent@ . If you want to search the @User-Agent@ header for the value @BadBot@ , you base64-encode @BadBot@ using MIME base64-encoding and include the resulting value, @QmFkQm90@ , in the value of @TargetString@ .
-- __If you're using the AWS CLI or one of the AWS SDKs__ 
-- The value that you want AWS WAF to search for. The SDK automatically base64 encodes the value.
  , textTransformation :: Types.TextTransformation
    -- ^ Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting it for a match.
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
  , positionalConstraint :: Types.PositionalConstraint
    -- ^ Within the portion of a web request that you want to search (for example, in the query string, if any), specify where you want AWS WAF to search. Valid values include the following:
--
-- __CONTAINS__ 
-- The specified part of the web request must include the value of @TargetString@ , but the location doesn't matter.
-- __CONTAINS_WORD__ 
-- The specified part of the web request must include the value of @TargetString@ , and @TargetString@ must contain only alphanumeric characters or underscore (A-Z, a-z, 0-9, or _). In addition, @TargetString@ must be a word, which means one of the following:
--
--     * @TargetString@ exactly matches the value of the specified part of the web request, such as the value of a header.
--
--
--     * @TargetString@ is at the beginning of the specified part of the web request and is followed by a character other than an alphanumeric character or underscore (_), for example, @BadBot;@ .
--
--
--     * @TargetString@ is at the end of the specified part of the web request and is preceded by a character other than an alphanumeric character or underscore (_), for example, @;BadBot@ .
--
--
--     * @TargetString@ is in the middle of the specified part of the web request and is preceded and followed by characters other than alphanumeric characters or underscore (_), for example, @-BadBot;@ .
--
--
-- __EXACTLY__ 
-- The value of the specified part of the web request must exactly match the value of @TargetString@ .
-- __STARTS_WITH__ 
-- The value of @TargetString@ must appear at the beginning of the specified part of the web request.
-- __ENDS_WITH__ 
-- The value of @TargetString@ must appear at the end of the specified part of the web request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ByteMatchTuple' value with any optional fields omitted.
mkByteMatchTuple
    :: Types.FieldToMatch -- ^ 'fieldToMatch'
    -> Core.Base64 -- ^ 'targetString'
    -> Types.TextTransformation -- ^ 'textTransformation'
    -> Types.PositionalConstraint -- ^ 'positionalConstraint'
    -> ByteMatchTuple
mkByteMatchTuple fieldToMatch targetString textTransformation
  positionalConstraint
  = ByteMatchTuple'{fieldToMatch, targetString, textTransformation,
                    positionalConstraint}

-- | The part of a web request that you want AWS WAF to search, such as a specified header or a query string. For more information, see 'FieldToMatch' .
--
-- /Note:/ Consider using 'fieldToMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmtFieldToMatch :: Lens.Lens' ByteMatchTuple Types.FieldToMatch
bmtFieldToMatch = Lens.field @"fieldToMatch"
{-# INLINEABLE bmtFieldToMatch #-}
{-# DEPRECATED fieldToMatch "Use generic-lens or generic-optics with 'fieldToMatch' instead"  #-}

-- | The value that you want AWS WAF to search for. AWS WAF searches for the specified string in the part of web requests that you specified in @FieldToMatch@ . The maximum length of the value is 50 bytes.
--
-- Valid values depend on the values that you specified for @FieldToMatch@ :
--
--     * @HEADER@ : The value that you want AWS WAF to search for in the request header that you specified in 'FieldToMatch' , for example, the value of the @User-Agent@ or @Referer@ header.
--
--
--     * @METHOD@ : The HTTP method, which indicates the type of operation specified in the request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
--
--     * @QUERY_STRING@ : The value that you want AWS WAF to search for in the query string, which is the part of a URL that appears after a @?@ character.
--
--
--     * @URI@ : The value that you want AWS WAF to search for in the part of a URL that identifies a resource, for example, @/images/daily-ad.jpg@ .
--
--
--     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' . 
--
--
--     * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.
--
--
--     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but instead of inspecting a single parameter, AWS WAF inspects all parameters within the query string for the value or regex pattern that you specify in @TargetString@ .
--
--
-- If @TargetString@ includes alphabetic characters A-Z and a-z, note that the value is case sensitive.
-- __If you're using the AWS WAF API__ 
-- Specify a base64-encoded version of the value. The maximum length of the value before you base64-encode it is 50 bytes.
-- For example, suppose the value of @Type@ is @HEADER@ and the value of @Data@ is @User-Agent@ . If you want to search the @User-Agent@ header for the value @BadBot@ , you base64-encode @BadBot@ using MIME base64-encoding and include the resulting value, @QmFkQm90@ , in the value of @TargetString@ .
-- __If you're using the AWS CLI or one of the AWS SDKs__ 
-- The value that you want AWS WAF to search for. The SDK automatically base64 encodes the value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'targetString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmtTargetString :: Lens.Lens' ByteMatchTuple Core.Base64
bmtTargetString = Lens.field @"targetString"
{-# INLINEABLE bmtTargetString #-}
{-# DEPRECATED targetString "Use generic-lens or generic-optics with 'targetString' instead"  #-}

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
bmtTextTransformation :: Lens.Lens' ByteMatchTuple Types.TextTransformation
bmtTextTransformation = Lens.field @"textTransformation"
{-# INLINEABLE bmtTextTransformation #-}
{-# DEPRECATED textTransformation "Use generic-lens or generic-optics with 'textTransformation' instead"  #-}

-- | Within the portion of a web request that you want to search (for example, in the query string, if any), specify where you want AWS WAF to search. Valid values include the following:
--
-- __CONTAINS__ 
-- The specified part of the web request must include the value of @TargetString@ , but the location doesn't matter.
-- __CONTAINS_WORD__ 
-- The specified part of the web request must include the value of @TargetString@ , and @TargetString@ must contain only alphanumeric characters or underscore (A-Z, a-z, 0-9, or _). In addition, @TargetString@ must be a word, which means one of the following:
--
--     * @TargetString@ exactly matches the value of the specified part of the web request, such as the value of a header.
--
--
--     * @TargetString@ is at the beginning of the specified part of the web request and is followed by a character other than an alphanumeric character or underscore (_), for example, @BadBot;@ .
--
--
--     * @TargetString@ is at the end of the specified part of the web request and is preceded by a character other than an alphanumeric character or underscore (_), for example, @;BadBot@ .
--
--
--     * @TargetString@ is in the middle of the specified part of the web request and is preceded and followed by characters other than alphanumeric characters or underscore (_), for example, @-BadBot;@ .
--
--
-- __EXACTLY__ 
-- The value of the specified part of the web request must exactly match the value of @TargetString@ .
-- __STARTS_WITH__ 
-- The value of @TargetString@ must appear at the beginning of the specified part of the web request.
-- __ENDS_WITH__ 
-- The value of @TargetString@ must appear at the end of the specified part of the web request.
--
-- /Note:/ Consider using 'positionalConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmtPositionalConstraint :: Lens.Lens' ByteMatchTuple Types.PositionalConstraint
bmtPositionalConstraint = Lens.field @"positionalConstraint"
{-# INLINEABLE bmtPositionalConstraint #-}
{-# DEPRECATED positionalConstraint "Use generic-lens or generic-optics with 'positionalConstraint' instead"  #-}

instance Core.FromJSON ByteMatchTuple where
        toJSON ByteMatchTuple{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FieldToMatch" Core..= fieldToMatch),
                  Core.Just ("TargetString" Core..= targetString),
                  Core.Just ("TextTransformation" Core..= textTransformation),
                  Core.Just ("PositionalConstraint" Core..= positionalConstraint)])

instance Core.FromJSON ByteMatchTuple where
        parseJSON
          = Core.withObject "ByteMatchTuple" Core.$
              \ x ->
                ByteMatchTuple' Core.<$>
                  (x Core..: "FieldToMatch") Core.<*> x Core..: "TargetString"
                    Core.<*> x Core..: "TextTransformation"
                    Core.<*> x Core..: "PositionalConstraint"
