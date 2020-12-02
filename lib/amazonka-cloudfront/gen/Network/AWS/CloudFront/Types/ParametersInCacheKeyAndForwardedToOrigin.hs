{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin where

import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This object determines the values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
--
-- /See:/ 'parametersInCacheKeyAndForwardedToOrigin' smart constructor.
data ParametersInCacheKeyAndForwardedToOrigin = ParametersInCacheKeyAndForwardedToOrigin'
  { _pickaftoEnableAcceptEncodingBrotli ::
      !( Maybe
           Bool
       ),
    _pickaftoEnableAcceptEncodingGzip ::
      !Bool,
    _pickaftoHeadersConfig ::
      !CachePolicyHeadersConfig,
    _pickaftoCookiesConfig ::
      !CachePolicyCookiesConfig,
    _pickaftoQueryStringsConfig ::
      !CachePolicyQueryStringsConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParametersInCacheKeyAndForwardedToOrigin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pickaftoEnableAcceptEncodingBrotli' - A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin. This field is related to the @EnableAcceptEncodingGzip@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:     * Normalizes the value of the viewer’s @Accept-Encoding@ header     * Includes the normalized header in the cache key     * Includes the normalized header in the request to the origin, if a request is necessary For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ . If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect. If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- * 'pickaftoEnableAcceptEncodingGzip' - A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin. This field is related to the @EnableAcceptEncodingBrotli@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:     * Normalizes the value of the viewer’s @Accept-Encoding@ header     * Includes the normalized header in the cache key     * Includes the normalized header in the request to the origin, if a request is necessary For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ . If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect. If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- * 'pickaftoHeadersConfig' - An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- * 'pickaftoCookiesConfig' - An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- * 'pickaftoQueryStringsConfig' - An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
parametersInCacheKeyAndForwardedToOrigin ::
  -- | 'pickaftoEnableAcceptEncodingGzip'
  Bool ->
  -- | 'pickaftoHeadersConfig'
  CachePolicyHeadersConfig ->
  -- | 'pickaftoCookiesConfig'
  CachePolicyCookiesConfig ->
  -- | 'pickaftoQueryStringsConfig'
  CachePolicyQueryStringsConfig ->
  ParametersInCacheKeyAndForwardedToOrigin
parametersInCacheKeyAndForwardedToOrigin
  pEnableAcceptEncodingGzip_
  pHeadersConfig_
  pCookiesConfig_
  pQueryStringsConfig_ =
    ParametersInCacheKeyAndForwardedToOrigin'
      { _pickaftoEnableAcceptEncodingBrotli =
          Nothing,
        _pickaftoEnableAcceptEncodingGzip =
          pEnableAcceptEncodingGzip_,
        _pickaftoHeadersConfig = pHeadersConfig_,
        _pickaftoCookiesConfig = pCookiesConfig_,
        _pickaftoQueryStringsConfig = pQueryStringsConfig_
      }

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin. This field is related to the @EnableAcceptEncodingGzip@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:     * Normalizes the value of the viewer’s @Accept-Encoding@ header     * Includes the normalized header in the cache key     * Includes the normalized header in the request to the origin, if a request is necessary For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ . If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect. If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
pickaftoEnableAcceptEncodingBrotli :: Lens' ParametersInCacheKeyAndForwardedToOrigin (Maybe Bool)
pickaftoEnableAcceptEncodingBrotli = lens _pickaftoEnableAcceptEncodingBrotli (\s a -> s {_pickaftoEnableAcceptEncodingBrotli = a})

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin. This field is related to the @EnableAcceptEncodingBrotli@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:     * Normalizes the value of the viewer’s @Accept-Encoding@ header     * Includes the normalized header in the cache key     * Includes the normalized header in the request to the origin, if a request is necessary For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ . If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect. If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
pickaftoEnableAcceptEncodingGzip :: Lens' ParametersInCacheKeyAndForwardedToOrigin Bool
pickaftoEnableAcceptEncodingGzip = lens _pickaftoEnableAcceptEncodingGzip (\s a -> s {_pickaftoEnableAcceptEncodingGzip = a})

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
pickaftoHeadersConfig :: Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyHeadersConfig
pickaftoHeadersConfig = lens _pickaftoHeadersConfig (\s a -> s {_pickaftoHeadersConfig = a})

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
pickaftoCookiesConfig :: Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyCookiesConfig
pickaftoCookiesConfig = lens _pickaftoCookiesConfig (\s a -> s {_pickaftoCookiesConfig = a})

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
pickaftoQueryStringsConfig :: Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyQueryStringsConfig
pickaftoQueryStringsConfig = lens _pickaftoQueryStringsConfig (\s a -> s {_pickaftoQueryStringsConfig = a})

instance FromXML ParametersInCacheKeyAndForwardedToOrigin where
  parseXML x =
    ParametersInCacheKeyAndForwardedToOrigin'
      <$> (x .@? "EnableAcceptEncodingBrotli")
      <*> (x .@ "EnableAcceptEncodingGzip")
      <*> (x .@ "HeadersConfig")
      <*> (x .@ "CookiesConfig")
      <*> (x .@ "QueryStringsConfig")

instance Hashable ParametersInCacheKeyAndForwardedToOrigin

instance NFData ParametersInCacheKeyAndForwardedToOrigin

instance ToXML ParametersInCacheKeyAndForwardedToOrigin where
  toXML ParametersInCacheKeyAndForwardedToOrigin' {..} =
    mconcat
      [ "EnableAcceptEncodingBrotli"
          @= _pickaftoEnableAcceptEncodingBrotli,
        "EnableAcceptEncodingGzip" @= _pickaftoEnableAcceptEncodingGzip,
        "HeadersConfig" @= _pickaftoHeadersConfig,
        "CookiesConfig" @= _pickaftoCookiesConfig,
        "QueryStringsConfig" @= _pickaftoQueryStringsConfig
      ]
