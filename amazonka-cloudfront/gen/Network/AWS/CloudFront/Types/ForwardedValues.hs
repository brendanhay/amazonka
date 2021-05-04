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
-- Module      : Network.AWS.CloudFront.Types.ForwardedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ForwardedValues where

import Network.AWS.CloudFront.Types.CookiePreference
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.QueryStringCacheKeys
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include values in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send values to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
--
-- /See:/ 'newForwardedValues' smart constructor.
data ForwardedValues = ForwardedValues'
  { -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include query strings in the cache key, use a cache
    -- policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send query strings to the origin but not include them in
    -- the cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A complex type that contains information about the query string
    -- parameters that you want CloudFront to use for caching for this cache
    -- behavior.
    queryStringCacheKeys :: Prelude.Maybe QueryStringCacheKeys,
    -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include headers in the cache key, use a cache policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send headers to the origin but not include them in the
    -- cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A complex type that specifies the @Headers@, if any, that you want
    -- CloudFront to forward to the origin for this cache behavior (whitelisted
    -- headers). For the headers that you specify, CloudFront also caches
    -- separate versions of a specified object that is based on the header
    -- values in viewer requests.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers>
    -- in the /Amazon CloudFront Developer Guide/.
    headers :: Prelude.Maybe Headers,
    -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include query strings in the cache key, use a cache
    -- policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send query strings to the origin but not include them in
    -- the cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- Indicates whether you want CloudFront to forward query strings to the
    -- origin that is associated with this cache behavior and cache based on
    -- the query string parameters. CloudFront behavior depends on the value of
    -- @QueryString@ and on the values that you specify for
    -- @QueryStringCacheKeys@, if any:
    --
    -- If you specify true for @QueryString@ and you don\'t specify any values
    -- for @QueryStringCacheKeys@, CloudFront forwards all query string
    -- parameters to the origin and caches based on all query string
    -- parameters. Depending on how many query string parameters and values you
    -- have, this can adversely affect performance because CloudFront must
    -- forward more requests to the origin.
    --
    -- If you specify true for @QueryString@ and you specify one or more values
    -- for @QueryStringCacheKeys@, CloudFront forwards all query string
    -- parameters to the origin, but it only caches based on the query string
    -- parameters that you specify.
    --
    -- If you specify false for @QueryString@, CloudFront doesn\'t forward any
    -- query string parameters to the origin, and doesn\'t cache based on query
    -- string parameters.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters>
    -- in the /Amazon CloudFront Developer Guide/.
    queryString :: Prelude.Bool,
    -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include cookies in the cache key, use a cache policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send cookies to the origin but not include them in the
    -- cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A complex type that specifies whether you want CloudFront to forward
    -- cookies to the origin and, if so, which ones. For more information about
    -- forwarding cookies to the origin, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies>
    -- in the /Amazon CloudFront Developer Guide/.
    cookies :: CookiePreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ForwardedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStringCacheKeys', 'forwardedValues_queryStringCacheKeys' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache
-- policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send query strings to the origin but not include them in
-- the cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that contains information about the query string
-- parameters that you want CloudFront to use for caching for this cache
-- behavior.
--
-- 'headers', 'forwardedValues_headers' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include headers in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send headers to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies the @Headers@, if any, that you want
-- CloudFront to forward to the origin for this cache behavior (whitelisted
-- headers). For the headers that you specify, CloudFront also caches
-- separate versions of a specified object that is based on the header
-- values in viewer requests.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'queryString', 'forwardedValues_queryString' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache
-- policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send query strings to the origin but not include them in
-- the cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Indicates whether you want CloudFront to forward query strings to the
-- origin that is associated with this cache behavior and cache based on
-- the query string parameters. CloudFront behavior depends on the value of
-- @QueryString@ and on the values that you specify for
-- @QueryStringCacheKeys@, if any:
--
-- If you specify true for @QueryString@ and you don\'t specify any values
-- for @QueryStringCacheKeys@, CloudFront forwards all query string
-- parameters to the origin and caches based on all query string
-- parameters. Depending on how many query string parameters and values you
-- have, this can adversely affect performance because CloudFront must
-- forward more requests to the origin.
--
-- If you specify true for @QueryString@ and you specify one or more values
-- for @QueryStringCacheKeys@, CloudFront forwards all query string
-- parameters to the origin, but it only caches based on the query string
-- parameters that you specify.
--
-- If you specify false for @QueryString@, CloudFront doesn\'t forward any
-- query string parameters to the origin, and doesn\'t cache based on query
-- string parameters.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'cookies', 'forwardedValues_cookies' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies whether you want CloudFront to forward
-- cookies to the origin and, if so, which ones. For more information about
-- forwarding cookies to the origin, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies>
-- in the /Amazon CloudFront Developer Guide/.
newForwardedValues ::
  -- | 'queryString'
  Prelude.Bool ->
  -- | 'cookies'
  CookiePreference ->
  ForwardedValues
newForwardedValues pQueryString_ pCookies_ =
  ForwardedValues'
    { queryStringCacheKeys =
        Prelude.Nothing,
      headers = Prelude.Nothing,
      queryString = pQueryString_,
      cookies = pCookies_
    }

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache
-- policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send query strings to the origin but not include them in
-- the cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that contains information about the query string
-- parameters that you want CloudFront to use for caching for this cache
-- behavior.
forwardedValues_queryStringCacheKeys :: Lens.Lens' ForwardedValues (Prelude.Maybe QueryStringCacheKeys)
forwardedValues_queryStringCacheKeys = Lens.lens (\ForwardedValues' {queryStringCacheKeys} -> queryStringCacheKeys) (\s@ForwardedValues' {} a -> s {queryStringCacheKeys = a} :: ForwardedValues)

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include headers in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send headers to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies the @Headers@, if any, that you want
-- CloudFront to forward to the origin for this cache behavior (whitelisted
-- headers). For the headers that you specify, CloudFront also caches
-- separate versions of a specified object that is based on the header
-- values in viewer requests.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers>
-- in the /Amazon CloudFront Developer Guide/.
forwardedValues_headers :: Lens.Lens' ForwardedValues (Prelude.Maybe Headers)
forwardedValues_headers = Lens.lens (\ForwardedValues' {headers} -> headers) (\s@ForwardedValues' {} a -> s {headers = a} :: ForwardedValues)

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache
-- policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send query strings to the origin but not include them in
-- the cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Indicates whether you want CloudFront to forward query strings to the
-- origin that is associated with this cache behavior and cache based on
-- the query string parameters. CloudFront behavior depends on the value of
-- @QueryString@ and on the values that you specify for
-- @QueryStringCacheKeys@, if any:
--
-- If you specify true for @QueryString@ and you don\'t specify any values
-- for @QueryStringCacheKeys@, CloudFront forwards all query string
-- parameters to the origin and caches based on all query string
-- parameters. Depending on how many query string parameters and values you
-- have, this can adversely affect performance because CloudFront must
-- forward more requests to the origin.
--
-- If you specify true for @QueryString@ and you specify one or more values
-- for @QueryStringCacheKeys@, CloudFront forwards all query string
-- parameters to the origin, but it only caches based on the query string
-- parameters that you specify.
--
-- If you specify false for @QueryString@, CloudFront doesn\'t forward any
-- query string parameters to the origin, and doesn\'t cache based on query
-- string parameters.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters>
-- in the /Amazon CloudFront Developer Guide/.
forwardedValues_queryString :: Lens.Lens' ForwardedValues Prelude.Bool
forwardedValues_queryString = Lens.lens (\ForwardedValues' {queryString} -> queryString) (\s@ForwardedValues' {} a -> s {queryString = a} :: ForwardedValues)

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies whether you want CloudFront to forward
-- cookies to the origin and, if so, which ones. For more information about
-- forwarding cookies to the origin, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies>
-- in the /Amazon CloudFront Developer Guide/.
forwardedValues_cookies :: Lens.Lens' ForwardedValues CookiePreference
forwardedValues_cookies = Lens.lens (\ForwardedValues' {cookies} -> cookies) (\s@ForwardedValues' {} a -> s {cookies = a} :: ForwardedValues)

instance Prelude.FromXML ForwardedValues where
  parseXML x =
    ForwardedValues'
      Prelude.<$> (x Prelude..@? "QueryStringCacheKeys")
      Prelude.<*> (x Prelude..@? "Headers")
      Prelude.<*> (x Prelude..@ "QueryString")
      Prelude.<*> (x Prelude..@ "Cookies")

instance Prelude.Hashable ForwardedValues

instance Prelude.NFData ForwardedValues

instance Prelude.ToXML ForwardedValues where
  toXML ForwardedValues' {..} =
    Prelude.mconcat
      [ "QueryStringCacheKeys"
          Prelude.@= queryStringCacheKeys,
        "Headers" Prelude.@= headers,
        "QueryString" Prelude.@= queryString,
        "Cookies" Prelude.@= cookies
      ]
