{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.FieldToMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.FieldToMatch where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.MatchFieldType

-- | Specifies where in a web request to look for @TargetString@ .
--
--
--
-- /See:/ 'fieldToMatch' smart constructor.
data FieldToMatch = FieldToMatch'
  { _ftmData :: !(Maybe Text),
    _ftmType :: !MatchFieldType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldToMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ftmData' - When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . The name of the header is not case sensitive. When the value of @Type@ is @SINGLE_QUERY_ARG@ , enter the name of the parameter that you want AWS WAF to search, for example, @UserName@ or @SalesRegion@ . The parameter name is not case sensitive. If the value of @Type@ is any other value, omit @Data@ .
--
-- * 'ftmType' - The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .      * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but rather than inspecting a single parameter, AWS WAF will inspect all parameters within the query for the value or regex pattern that you specify in @TargetString@ .
fieldToMatch ::
  -- | 'ftmType'
  MatchFieldType ->
  FieldToMatch
fieldToMatch pType_ =
  FieldToMatch' {_ftmData = Nothing, _ftmType = pType_}

-- | When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . The name of the header is not case sensitive. When the value of @Type@ is @SINGLE_QUERY_ARG@ , enter the name of the parameter that you want AWS WAF to search, for example, @UserName@ or @SalesRegion@ . The parameter name is not case sensitive. If the value of @Type@ is any other value, omit @Data@ .
ftmData :: Lens' FieldToMatch (Maybe Text)
ftmData = lens _ftmData (\s a -> s {_ftmData = a})

-- | The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .      * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but rather than inspecting a single parameter, AWS WAF will inspect all parameters within the query for the value or regex pattern that you specify in @TargetString@ .
ftmType :: Lens' FieldToMatch MatchFieldType
ftmType = lens _ftmType (\s a -> s {_ftmType = a})

instance FromJSON FieldToMatch where
  parseJSON =
    withObject
      "FieldToMatch"
      (\x -> FieldToMatch' <$> (x .:? "Data") <*> (x .: "Type"))

instance Hashable FieldToMatch

instance NFData FieldToMatch

instance ToJSON FieldToMatch where
  toJSON FieldToMatch' {..} =
    object
      (catMaybes [("Data" .=) <$> _ftmData, Just ("Type" .= _ftmType)])
