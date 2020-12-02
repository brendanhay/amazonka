{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ForwardedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ForwardedValues where

import Network.AWS.CloudFront.Types.CookiePreference
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.QueryStringCacheKeys
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
--
-- /See:/ 'forwardedValues' smart constructor.
data ForwardedValues = ForwardedValues'
  { _fvQueryStringCacheKeys ::
      !(Maybe QueryStringCacheKeys),
    _fvHeaders :: !(Maybe Headers),
    _fvQueryString :: !Bool,
    _fvCookies :: !CookiePreference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ForwardedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fvQueryStringCacheKeys' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
--
-- * 'fvHeaders' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include headers in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send headers to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies the @Headers@ , if any, that you want CloudFront to forward to the origin for this cache behavior (whitelisted headers). For the headers that you specify, CloudFront also caches separate versions of a specified object that is based on the header values in viewer requests. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'fvQueryString' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any: If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin. If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify. If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'fvCookies' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include cookies in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send cookies to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
forwardedValues ::
  -- | 'fvQueryString'
  Bool ->
  -- | 'fvCookies'
  CookiePreference ->
  ForwardedValues
forwardedValues pQueryString_ pCookies_ =
  ForwardedValues'
    { _fvQueryStringCacheKeys = Nothing,
      _fvHeaders = Nothing,
      _fvQueryString = pQueryString_,
      _fvCookies = pCookies_
    }

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
fvQueryStringCacheKeys :: Lens' ForwardedValues (Maybe QueryStringCacheKeys)
fvQueryStringCacheKeys = lens _fvQueryStringCacheKeys (\s a -> s {_fvQueryStringCacheKeys = a})

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include headers in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send headers to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies the @Headers@ , if any, that you want CloudFront to forward to the origin for this cache behavior (whitelisted headers). For the headers that you specify, CloudFront also caches separate versions of a specified object that is based on the header values in viewer requests. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers> in the /Amazon CloudFront Developer Guide/ .
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\s a -> s {_fvHeaders = a})

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any: If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin. If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify. If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
fvQueryString :: Lens' ForwardedValues Bool
fvQueryString = lens _fvQueryString (\s a -> s {_fvQueryString = a})

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. If you want to include cookies in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send cookies to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
fvCookies :: Lens' ForwardedValues CookiePreference
fvCookies = lens _fvCookies (\s a -> s {_fvCookies = a})

instance FromXML ForwardedValues where
  parseXML x =
    ForwardedValues'
      <$> (x .@? "QueryStringCacheKeys")
      <*> (x .@? "Headers")
      <*> (x .@ "QueryString")
      <*> (x .@ "Cookies")

instance Hashable ForwardedValues

instance NFData ForwardedValues

instance ToXML ForwardedValues where
  toXML ForwardedValues' {..} =
    mconcat
      [ "QueryStringCacheKeys" @= _fvQueryStringCacheKeys,
        "Headers" @= _fvHeaders,
        "QueryString" @= _fvQueryString,
        "Cookies" @= _fvCookies
      ]
