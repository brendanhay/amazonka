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
-- Module      : Network.AWS.Lightsail.Types.CacheSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CookieObject
import Network.AWS.Lightsail.Types.HeaderObject
import Network.AWS.Lightsail.Types.QueryStringObject

-- | Describes the cache settings of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- These settings apply only to your distribution\'s @cacheBehaviors@
-- (including the @defaultCacheBehavior@) that have a @behavior@ of
-- @cache@.
--
-- /See:/ 'newCacheSettings' smart constructor.
data CacheSettings = CacheSettings'
  { -- | The maximum amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the object has been updated.
    --
    -- The value specified applies only when the origin adds HTTP headers such
    -- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
    -- objects.
    maximumTTL :: Core.Maybe Core.Integer,
    -- | An object that describes the headers that are forwarded to the origin.
    -- Your content is cached based on the headers that are forwarded.
    forwardedHeaders :: Core.Maybe HeaderObject,
    -- | The default amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the content has been updated.
    --
    -- The value specified applies only when the origin does not add HTTP
    -- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
    -- @Expires@ to objects.
    defaultTTL :: Core.Maybe Core.Integer,
    -- | The HTTP method responses that are cached by your distribution.
    --
    -- You can specify the following options:
    --
    -- -   @GET,HEAD@ - The distribution caches responses to the @GET@ and
    --     @HEAD@ methods.
    --
    -- -   @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@,
    --     @HEAD@, and @OPTIONS@ methods.
    cachedHTTPMethods :: Core.Maybe Core.Text,
    -- | The HTTP methods that are processed and forwarded to the distribution\'s
    -- origin.
    --
    -- You can specify the following options:
    --
    -- -   @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
    --
    -- -   @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@, @HEAD@,
    --     and @OPTIONS@ methods.
    --
    -- -   @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards
    --     the @GET@, @HEAD@, @OPTIONS@, @PUT@, @PATCH@, @POST@, and @DELETE@
    --     methods.
    --
    -- If you specify the third option, you might need to restrict access to
    -- your distribution\'s origin so users can\'t perform operations that you
    -- don\'t want them to. For example, you might not want users to have
    -- permission to delete objects from your origin.
    allowedHTTPMethods :: Core.Maybe Core.Text,
    -- | An object that describes the query strings that are forwarded to the
    -- origin. Your content is cached based on the query strings that are
    -- forwarded.
    forwardedQueryStrings :: Core.Maybe QueryStringObject,
    -- | An object that describes the cookies that are forwarded to the origin.
    -- Your content is cached based on the cookies that are forwarded.
    forwardedCookies :: Core.Maybe CookieObject,
    -- | The minimum amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the object has been updated.
    --
    -- A value of @0@ must be specified for @minimumTTL@ if the distribution is
    -- configured to forward all headers to the origin.
    minimumTTL :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumTTL', 'cacheSettings_maximumTTL' - The maximum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such
-- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects.
--
-- 'forwardedHeaders', 'cacheSettings_forwardedHeaders' - An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
--
-- 'defaultTTL', 'cacheSettings_defaultTTL' - The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
--
-- 'cachedHTTPMethods', 'cacheSettings_cachedHTTPMethods' - The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
-- -   @GET,HEAD@ - The distribution caches responses to the @GET@ and
--     @HEAD@ methods.
--
-- -   @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@,
--     @HEAD@, and @OPTIONS@ methods.
--
-- 'allowedHTTPMethods', 'cacheSettings_allowedHTTPMethods' - The HTTP methods that are processed and forwarded to the distribution\'s
-- origin.
--
-- You can specify the following options:
--
-- -   @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
-- -   @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@, @HEAD@,
--     and @OPTIONS@ methods.
--
-- -   @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards
--     the @GET@, @HEAD@, @OPTIONS@, @PUT@, @PATCH@, @POST@, and @DELETE@
--     methods.
--
-- If you specify the third option, you might need to restrict access to
-- your distribution\'s origin so users can\'t perform operations that you
-- don\'t want them to. For example, you might not want users to have
-- permission to delete objects from your origin.
--
-- 'forwardedQueryStrings', 'cacheSettings_forwardedQueryStrings' - An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
--
-- 'forwardedCookies', 'cacheSettings_forwardedCookies' - An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
--
-- 'minimumTTL', 'cacheSettings_minimumTTL' - The minimum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is
-- configured to forward all headers to the origin.
newCacheSettings ::
  CacheSettings
newCacheSettings =
  CacheSettings'
    { maximumTTL = Core.Nothing,
      forwardedHeaders = Core.Nothing,
      defaultTTL = Core.Nothing,
      cachedHTTPMethods = Core.Nothing,
      allowedHTTPMethods = Core.Nothing,
      forwardedQueryStrings = Core.Nothing,
      forwardedCookies = Core.Nothing,
      minimumTTL = Core.Nothing
    }

-- | The maximum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such
-- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects.
cacheSettings_maximumTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
cacheSettings_maximumTTL = Lens.lens (\CacheSettings' {maximumTTL} -> maximumTTL) (\s@CacheSettings' {} a -> s {maximumTTL = a} :: CacheSettings)

-- | An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
cacheSettings_forwardedHeaders :: Lens.Lens' CacheSettings (Core.Maybe HeaderObject)
cacheSettings_forwardedHeaders = Lens.lens (\CacheSettings' {forwardedHeaders} -> forwardedHeaders) (\s@CacheSettings' {} a -> s {forwardedHeaders = a} :: CacheSettings)

-- | The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
cacheSettings_defaultTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
cacheSettings_defaultTTL = Lens.lens (\CacheSettings' {defaultTTL} -> defaultTTL) (\s@CacheSettings' {} a -> s {defaultTTL = a} :: CacheSettings)

-- | The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
-- -   @GET,HEAD@ - The distribution caches responses to the @GET@ and
--     @HEAD@ methods.
--
-- -   @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@,
--     @HEAD@, and @OPTIONS@ methods.
cacheSettings_cachedHTTPMethods :: Lens.Lens' CacheSettings (Core.Maybe Core.Text)
cacheSettings_cachedHTTPMethods = Lens.lens (\CacheSettings' {cachedHTTPMethods} -> cachedHTTPMethods) (\s@CacheSettings' {} a -> s {cachedHTTPMethods = a} :: CacheSettings)

-- | The HTTP methods that are processed and forwarded to the distribution\'s
-- origin.
--
-- You can specify the following options:
--
-- -   @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
-- -   @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@, @HEAD@,
--     and @OPTIONS@ methods.
--
-- -   @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards
--     the @GET@, @HEAD@, @OPTIONS@, @PUT@, @PATCH@, @POST@, and @DELETE@
--     methods.
--
-- If you specify the third option, you might need to restrict access to
-- your distribution\'s origin so users can\'t perform operations that you
-- don\'t want them to. For example, you might not want users to have
-- permission to delete objects from your origin.
cacheSettings_allowedHTTPMethods :: Lens.Lens' CacheSettings (Core.Maybe Core.Text)
cacheSettings_allowedHTTPMethods = Lens.lens (\CacheSettings' {allowedHTTPMethods} -> allowedHTTPMethods) (\s@CacheSettings' {} a -> s {allowedHTTPMethods = a} :: CacheSettings)

-- | An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
cacheSettings_forwardedQueryStrings :: Lens.Lens' CacheSettings (Core.Maybe QueryStringObject)
cacheSettings_forwardedQueryStrings = Lens.lens (\CacheSettings' {forwardedQueryStrings} -> forwardedQueryStrings) (\s@CacheSettings' {} a -> s {forwardedQueryStrings = a} :: CacheSettings)

-- | An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
cacheSettings_forwardedCookies :: Lens.Lens' CacheSettings (Core.Maybe CookieObject)
cacheSettings_forwardedCookies = Lens.lens (\CacheSettings' {forwardedCookies} -> forwardedCookies) (\s@CacheSettings' {} a -> s {forwardedCookies = a} :: CacheSettings)

-- | The minimum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is
-- configured to forward all headers to the origin.
cacheSettings_minimumTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
cacheSettings_minimumTTL = Lens.lens (\CacheSettings' {minimumTTL} -> minimumTTL) (\s@CacheSettings' {} a -> s {minimumTTL = a} :: CacheSettings)

instance Core.FromJSON CacheSettings where
  parseJSON =
    Core.withObject
      "CacheSettings"
      ( \x ->
          CacheSettings'
            Core.<$> (x Core..:? "maximumTTL")
            Core.<*> (x Core..:? "forwardedHeaders")
            Core.<*> (x Core..:? "defaultTTL")
            Core.<*> (x Core..:? "cachedHTTPMethods")
            Core.<*> (x Core..:? "allowedHTTPMethods")
            Core.<*> (x Core..:? "forwardedQueryStrings")
            Core.<*> (x Core..:? "forwardedCookies")
            Core.<*> (x Core..:? "minimumTTL")
      )

instance Core.Hashable CacheSettings

instance Core.NFData CacheSettings

instance Core.ToJSON CacheSettings where
  toJSON CacheSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("maximumTTL" Core..=) Core.<$> maximumTTL,
            ("forwardedHeaders" Core..=)
              Core.<$> forwardedHeaders,
            ("defaultTTL" Core..=) Core.<$> defaultTTL,
            ("cachedHTTPMethods" Core..=)
              Core.<$> cachedHTTPMethods,
            ("allowedHTTPMethods" Core..=)
              Core.<$> allowedHTTPMethods,
            ("forwardedQueryStrings" Core..=)
              Core.<$> forwardedQueryStrings,
            ("forwardedCookies" Core..=)
              Core.<$> forwardedCookies,
            ("minimumTTL" Core..=) Core.<$> minimumTTL
          ]
      )
