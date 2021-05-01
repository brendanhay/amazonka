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
-- Module      : Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin where

import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This object determines the values that CloudFront includes in the cache
-- key. These values can include HTTP headers, cookies, and URL query
-- strings. CloudFront uses the cache key to find an object in its cache
-- that it can return to the viewer.
--
-- The headers, cookies, and query strings that are included in the cache
-- key are automatically included in requests that CloudFront sends to the
-- origin. CloudFront sends a request when it can’t find an object in its
-- cache that matches the request’s cache key. If you want to send values
-- to the origin but /not/ include them in the cache key, use
-- @OriginRequestPolicy@.
--
-- /See:/ 'newParametersInCacheKeyAndForwardedToOrigin' smart constructor.
data ParametersInCacheKeyAndForwardedToOrigin = ParametersInCacheKeyAndForwardedToOrigin'
  { -- | A flag that can affect whether the @Accept-Encoding@ HTTP header is
    -- included in the cache key and included in requests that CloudFront sends
    -- to the origin.
    --
    -- This field is related to the @EnableAcceptEncodingGzip@ field. If one or
    -- both of these fields is @true@ /and/ the viewer request includes the
    -- @Accept-Encoding@ header, then CloudFront does the following:
    --
    -- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
    --
    -- -   Includes the normalized header in the cache key
    --
    -- -   Includes the normalized header in the request to the origin, if a
    --     request is necessary
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you set this value to @true@, and this cache behavior also has an
    -- origin request policy attached, do not include the @Accept-Encoding@
    -- header in the origin request policy. CloudFront always includes the
    -- @Accept-Encoding@ header in origin requests when the value of this field
    -- is @true@, so including this header in an origin request policy has no
    -- effect.
    --
    -- If both of these fields are @false@, then CloudFront treats the
    -- @Accept-Encoding@ header the same as any other HTTP header in the viewer
    -- request. By default, it’s not included in the cache key and it’s not
    -- included in origin requests. In this case, you can manually add
    -- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
    enableAcceptEncodingBrotli :: Prelude.Maybe Prelude.Bool,
    -- | A flag that can affect whether the @Accept-Encoding@ HTTP header is
    -- included in the cache key and included in requests that CloudFront sends
    -- to the origin.
    --
    -- This field is related to the @EnableAcceptEncodingBrotli@ field. If one
    -- or both of these fields is @true@ /and/ the viewer request includes the
    -- @Accept-Encoding@ header, then CloudFront does the following:
    --
    -- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
    --
    -- -   Includes the normalized header in the cache key
    --
    -- -   Includes the normalized header in the request to the origin, if a
    --     request is necessary
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you set this value to @true@, and this cache behavior also has an
    -- origin request policy attached, do not include the @Accept-Encoding@
    -- header in the origin request policy. CloudFront always includes the
    -- @Accept-Encoding@ header in origin requests when the value of this field
    -- is @true@, so including this header in an origin request policy has no
    -- effect.
    --
    -- If both of these fields are @false@, then CloudFront treats the
    -- @Accept-Encoding@ header the same as any other HTTP header in the viewer
    -- request. By default, it’s not included in the cache key and it’s not
    -- included in origin requests. In this case, you can manually add
    -- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
    enableAcceptEncodingGzip :: Prelude.Bool,
    -- | An object that determines whether any HTTP headers (and if so, which
    -- headers) are included in the cache key and automatically included in
    -- requests that CloudFront sends to the origin.
    headersConfig :: CachePolicyHeadersConfig,
    -- | An object that determines whether any cookies in viewer requests (and if
    -- so, which cookies) are included in the cache key and automatically
    -- included in requests that CloudFront sends to the origin.
    cookiesConfig :: CachePolicyCookiesConfig,
    -- | An object that determines whether any URL query strings in viewer
    -- requests (and if so, which query strings) are included in the cache key
    -- and automatically included in requests that CloudFront sends to the
    -- origin.
    queryStringsConfig :: CachePolicyQueryStringsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ParametersInCacheKeyAndForwardedToOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableAcceptEncodingBrotli', 'parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingBrotli' - A flag that can affect whether the @Accept-Encoding@ HTTP header is
-- included in the cache key and included in requests that CloudFront sends
-- to the origin.
--
-- This field is related to the @EnableAcceptEncodingGzip@ field. If one or
-- both of these fields is @true@ /and/ the viewer request includes the
-- @Accept-Encoding@ header, then CloudFront does the following:
--
-- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
--
-- -   Includes the normalized header in the cache key
--
-- -   Includes the normalized header in the request to the origin, if a
--     request is necessary
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you set this value to @true@, and this cache behavior also has an
-- origin request policy attached, do not include the @Accept-Encoding@
-- header in the origin request policy. CloudFront always includes the
-- @Accept-Encoding@ header in origin requests when the value of this field
-- is @true@, so including this header in an origin request policy has no
-- effect.
--
-- If both of these fields are @false@, then CloudFront treats the
-- @Accept-Encoding@ header the same as any other HTTP header in the viewer
-- request. By default, it’s not included in the cache key and it’s not
-- included in origin requests. In this case, you can manually add
-- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- 'enableAcceptEncodingGzip', 'parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingGzip' - A flag that can affect whether the @Accept-Encoding@ HTTP header is
-- included in the cache key and included in requests that CloudFront sends
-- to the origin.
--
-- This field is related to the @EnableAcceptEncodingBrotli@ field. If one
-- or both of these fields is @true@ /and/ the viewer request includes the
-- @Accept-Encoding@ header, then CloudFront does the following:
--
-- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
--
-- -   Includes the normalized header in the cache key
--
-- -   Includes the normalized header in the request to the origin, if a
--     request is necessary
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you set this value to @true@, and this cache behavior also has an
-- origin request policy attached, do not include the @Accept-Encoding@
-- header in the origin request policy. CloudFront always includes the
-- @Accept-Encoding@ header in origin requests when the value of this field
-- is @true@, so including this header in an origin request policy has no
-- effect.
--
-- If both of these fields are @false@, then CloudFront treats the
-- @Accept-Encoding@ header the same as any other HTTP header in the viewer
-- request. By default, it’s not included in the cache key and it’s not
-- included in origin requests. In this case, you can manually add
-- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- 'headersConfig', 'parametersInCacheKeyAndForwardedToOrigin_headersConfig' - An object that determines whether any HTTP headers (and if so, which
-- headers) are included in the cache key and automatically included in
-- requests that CloudFront sends to the origin.
--
-- 'cookiesConfig', 'parametersInCacheKeyAndForwardedToOrigin_cookiesConfig' - An object that determines whether any cookies in viewer requests (and if
-- so, which cookies) are included in the cache key and automatically
-- included in requests that CloudFront sends to the origin.
--
-- 'queryStringsConfig', 'parametersInCacheKeyAndForwardedToOrigin_queryStringsConfig' - An object that determines whether any URL query strings in viewer
-- requests (and if so, which query strings) are included in the cache key
-- and automatically included in requests that CloudFront sends to the
-- origin.
newParametersInCacheKeyAndForwardedToOrigin ::
  -- | 'enableAcceptEncodingGzip'
  Prelude.Bool ->
  -- | 'headersConfig'
  CachePolicyHeadersConfig ->
  -- | 'cookiesConfig'
  CachePolicyCookiesConfig ->
  -- | 'queryStringsConfig'
  CachePolicyQueryStringsConfig ->
  ParametersInCacheKeyAndForwardedToOrigin
newParametersInCacheKeyAndForwardedToOrigin
  pEnableAcceptEncodingGzip_
  pHeadersConfig_
  pCookiesConfig_
  pQueryStringsConfig_ =
    ParametersInCacheKeyAndForwardedToOrigin'
      { enableAcceptEncodingBrotli =
          Prelude.Nothing,
        enableAcceptEncodingGzip =
          pEnableAcceptEncodingGzip_,
        headersConfig = pHeadersConfig_,
        cookiesConfig = pCookiesConfig_,
        queryStringsConfig =
          pQueryStringsConfig_
      }

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is
-- included in the cache key and included in requests that CloudFront sends
-- to the origin.
--
-- This field is related to the @EnableAcceptEncodingGzip@ field. If one or
-- both of these fields is @true@ /and/ the viewer request includes the
-- @Accept-Encoding@ header, then CloudFront does the following:
--
-- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
--
-- -   Includes the normalized header in the cache key
--
-- -   Includes the normalized header in the request to the origin, if a
--     request is necessary
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you set this value to @true@, and this cache behavior also has an
-- origin request policy attached, do not include the @Accept-Encoding@
-- header in the origin request policy. CloudFront always includes the
-- @Accept-Encoding@ header in origin requests when the value of this field
-- is @true@, so including this header in an origin request policy has no
-- effect.
--
-- If both of these fields are @false@, then CloudFront treats the
-- @Accept-Encoding@ header the same as any other HTTP header in the viewer
-- request. By default, it’s not included in the cache key and it’s not
-- included in origin requests. In this case, you can manually add
-- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingBrotli :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin (Prelude.Maybe Prelude.Bool)
parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingBrotli = Lens.lens (\ParametersInCacheKeyAndForwardedToOrigin' {enableAcceptEncodingBrotli} -> enableAcceptEncodingBrotli) (\s@ParametersInCacheKeyAndForwardedToOrigin' {} a -> s {enableAcceptEncodingBrotli = a} :: ParametersInCacheKeyAndForwardedToOrigin)

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is
-- included in the cache key and included in requests that CloudFront sends
-- to the origin.
--
-- This field is related to the @EnableAcceptEncodingBrotli@ field. If one
-- or both of these fields is @true@ /and/ the viewer request includes the
-- @Accept-Encoding@ header, then CloudFront does the following:
--
-- -   Normalizes the value of the viewer’s @Accept-Encoding@ header
--
-- -   Includes the normalized header in the cache key
--
-- -   Includes the normalized header in the request to the origin, if a
--     request is necessary
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you set this value to @true@, and this cache behavior also has an
-- origin request policy attached, do not include the @Accept-Encoding@
-- header in the origin request policy. CloudFront always includes the
-- @Accept-Encoding@ header in origin requests when the value of this field
-- is @true@, so including this header in an origin request policy has no
-- effect.
--
-- If both of these fields are @false@, then CloudFront treats the
-- @Accept-Encoding@ header the same as any other HTTP header in the viewer
-- request. By default, it’s not included in the cache key and it’s not
-- included in origin requests. In this case, you can manually add
-- @Accept-Encoding@ to the headers whitelist like any other HTTP header.
parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingGzip :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Prelude.Bool
parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingGzip = Lens.lens (\ParametersInCacheKeyAndForwardedToOrigin' {enableAcceptEncodingGzip} -> enableAcceptEncodingGzip) (\s@ParametersInCacheKeyAndForwardedToOrigin' {} a -> s {enableAcceptEncodingGzip = a} :: ParametersInCacheKeyAndForwardedToOrigin)

-- | An object that determines whether any HTTP headers (and if so, which
-- headers) are included in the cache key and automatically included in
-- requests that CloudFront sends to the origin.
parametersInCacheKeyAndForwardedToOrigin_headersConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyHeadersConfig
parametersInCacheKeyAndForwardedToOrigin_headersConfig = Lens.lens (\ParametersInCacheKeyAndForwardedToOrigin' {headersConfig} -> headersConfig) (\s@ParametersInCacheKeyAndForwardedToOrigin' {} a -> s {headersConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)

-- | An object that determines whether any cookies in viewer requests (and if
-- so, which cookies) are included in the cache key and automatically
-- included in requests that CloudFront sends to the origin.
parametersInCacheKeyAndForwardedToOrigin_cookiesConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyCookiesConfig
parametersInCacheKeyAndForwardedToOrigin_cookiesConfig = Lens.lens (\ParametersInCacheKeyAndForwardedToOrigin' {cookiesConfig} -> cookiesConfig) (\s@ParametersInCacheKeyAndForwardedToOrigin' {} a -> s {cookiesConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)

-- | An object that determines whether any URL query strings in viewer
-- requests (and if so, which query strings) are included in the cache key
-- and automatically included in requests that CloudFront sends to the
-- origin.
parametersInCacheKeyAndForwardedToOrigin_queryStringsConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyQueryStringsConfig
parametersInCacheKeyAndForwardedToOrigin_queryStringsConfig = Lens.lens (\ParametersInCacheKeyAndForwardedToOrigin' {queryStringsConfig} -> queryStringsConfig) (\s@ParametersInCacheKeyAndForwardedToOrigin' {} a -> s {queryStringsConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)

instance
  Prelude.FromXML
    ParametersInCacheKeyAndForwardedToOrigin
  where
  parseXML x =
    ParametersInCacheKeyAndForwardedToOrigin'
      Prelude.<$> (x Prelude..@? "EnableAcceptEncodingBrotli")
        Prelude.<*> (x Prelude..@ "EnableAcceptEncodingGzip")
        Prelude.<*> (x Prelude..@ "HeadersConfig")
        Prelude.<*> (x Prelude..@ "CookiesConfig")
        Prelude.<*> (x Prelude..@ "QueryStringsConfig")

instance
  Prelude.Hashable
    ParametersInCacheKeyAndForwardedToOrigin

instance
  Prelude.NFData
    ParametersInCacheKeyAndForwardedToOrigin

instance
  Prelude.ToXML
    ParametersInCacheKeyAndForwardedToOrigin
  where
  toXML ParametersInCacheKeyAndForwardedToOrigin' {..} =
    Prelude.mconcat
      [ "EnableAcceptEncodingBrotli"
          Prelude.@= enableAcceptEncodingBrotli,
        "EnableAcceptEncodingGzip"
          Prelude.@= enableAcceptEncodingGzip,
        "HeadersConfig" Prelude.@= headersConfig,
        "CookiesConfig" Prelude.@= cookiesConfig,
        "QueryStringsConfig" Prelude.@= queryStringsConfig
      ]
