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
import qualified Network.AWS.Prelude as Prelude

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
    maximumTTL :: Prelude.Maybe Prelude.Integer,
    -- | The HTTP method responses that are cached by your distribution.
    --
    -- You can specify the following options:
    --
    -- -   @GET,HEAD@ - The distribution caches responses to the @GET@ and
    --     @HEAD@ methods.
    --
    -- -   @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@,
    --     @HEAD@, and @OPTIONS@ methods.
    cachedHTTPMethods :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the cookies that are forwarded to the origin.
    -- Your content is cached based on the cookies that are forwarded.
    forwardedCookies :: Prelude.Maybe CookieObject,
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
    allowedHTTPMethods :: Prelude.Maybe Prelude.Text,
    -- | The default amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the content has been updated.
    --
    -- The value specified applies only when the origin does not add HTTP
    -- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
    -- @Expires@ to objects.
    defaultTTL :: Prelude.Maybe Prelude.Integer,
    -- | The minimum amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the object has been updated.
    --
    -- A value of @0@ must be specified for @minimumTTL@ if the distribution is
    -- configured to forward all headers to the origin.
    minimumTTL :: Prelude.Maybe Prelude.Integer,
    -- | An object that describes the headers that are forwarded to the origin.
    -- Your content is cached based on the headers that are forwarded.
    forwardedHeaders :: Prelude.Maybe HeaderObject,
    -- | An object that describes the query strings that are forwarded to the
    -- origin. Your content is cached based on the query strings that are
    -- forwarded.
    forwardedQueryStrings :: Prelude.Maybe QueryStringObject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'forwardedCookies', 'cacheSettings_forwardedCookies' - An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
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
-- 'defaultTTL', 'cacheSettings_defaultTTL' - The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
--
-- 'minimumTTL', 'cacheSettings_minimumTTL' - The minimum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is
-- configured to forward all headers to the origin.
--
-- 'forwardedHeaders', 'cacheSettings_forwardedHeaders' - An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
--
-- 'forwardedQueryStrings', 'cacheSettings_forwardedQueryStrings' - An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
newCacheSettings ::
  CacheSettings
newCacheSettings =
  CacheSettings'
    { maximumTTL = Prelude.Nothing,
      cachedHTTPMethods = Prelude.Nothing,
      forwardedCookies = Prelude.Nothing,
      allowedHTTPMethods = Prelude.Nothing,
      defaultTTL = Prelude.Nothing,
      minimumTTL = Prelude.Nothing,
      forwardedHeaders = Prelude.Nothing,
      forwardedQueryStrings = Prelude.Nothing
    }

-- | The maximum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such
-- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects.
cacheSettings_maximumTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_maximumTTL = Lens.lens (\CacheSettings' {maximumTTL} -> maximumTTL) (\s@CacheSettings' {} a -> s {maximumTTL = a} :: CacheSettings)

-- | The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
-- -   @GET,HEAD@ - The distribution caches responses to the @GET@ and
--     @HEAD@ methods.
--
-- -   @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@,
--     @HEAD@, and @OPTIONS@ methods.
cacheSettings_cachedHTTPMethods :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Text)
cacheSettings_cachedHTTPMethods = Lens.lens (\CacheSettings' {cachedHTTPMethods} -> cachedHTTPMethods) (\s@CacheSettings' {} a -> s {cachedHTTPMethods = a} :: CacheSettings)

-- | An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
cacheSettings_forwardedCookies :: Lens.Lens' CacheSettings (Prelude.Maybe CookieObject)
cacheSettings_forwardedCookies = Lens.lens (\CacheSettings' {forwardedCookies} -> forwardedCookies) (\s@CacheSettings' {} a -> s {forwardedCookies = a} :: CacheSettings)

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
cacheSettings_allowedHTTPMethods :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Text)
cacheSettings_allowedHTTPMethods = Lens.lens (\CacheSettings' {allowedHTTPMethods} -> allowedHTTPMethods) (\s@CacheSettings' {} a -> s {allowedHTTPMethods = a} :: CacheSettings)

-- | The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
cacheSettings_defaultTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_defaultTTL = Lens.lens (\CacheSettings' {defaultTTL} -> defaultTTL) (\s@CacheSettings' {} a -> s {defaultTTL = a} :: CacheSettings)

-- | The minimum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is
-- configured to forward all headers to the origin.
cacheSettings_minimumTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_minimumTTL = Lens.lens (\CacheSettings' {minimumTTL} -> minimumTTL) (\s@CacheSettings' {} a -> s {minimumTTL = a} :: CacheSettings)

-- | An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
cacheSettings_forwardedHeaders :: Lens.Lens' CacheSettings (Prelude.Maybe HeaderObject)
cacheSettings_forwardedHeaders = Lens.lens (\CacheSettings' {forwardedHeaders} -> forwardedHeaders) (\s@CacheSettings' {} a -> s {forwardedHeaders = a} :: CacheSettings)

-- | An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
cacheSettings_forwardedQueryStrings :: Lens.Lens' CacheSettings (Prelude.Maybe QueryStringObject)
cacheSettings_forwardedQueryStrings = Lens.lens (\CacheSettings' {forwardedQueryStrings} -> forwardedQueryStrings) (\s@CacheSettings' {} a -> s {forwardedQueryStrings = a} :: CacheSettings)

instance Core.FromJSON CacheSettings where
  parseJSON =
    Core.withObject
      "CacheSettings"
      ( \x ->
          CacheSettings'
            Prelude.<$> (x Core..:? "maximumTTL")
            Prelude.<*> (x Core..:? "cachedHTTPMethods")
            Prelude.<*> (x Core..:? "forwardedCookies")
            Prelude.<*> (x Core..:? "allowedHTTPMethods")
            Prelude.<*> (x Core..:? "defaultTTL")
            Prelude.<*> (x Core..:? "minimumTTL")
            Prelude.<*> (x Core..:? "forwardedHeaders")
            Prelude.<*> (x Core..:? "forwardedQueryStrings")
      )

instance Prelude.Hashable CacheSettings

instance Prelude.NFData CacheSettings

instance Core.ToJSON CacheSettings where
  toJSON CacheSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maximumTTL" Core..=) Prelude.<$> maximumTTL,
            ("cachedHTTPMethods" Core..=)
              Prelude.<$> cachedHTTPMethods,
            ("forwardedCookies" Core..=)
              Prelude.<$> forwardedCookies,
            ("allowedHTTPMethods" Core..=)
              Prelude.<$> allowedHTTPMethods,
            ("defaultTTL" Core..=) Prelude.<$> defaultTTL,
            ("minimumTTL" Core..=) Prelude.<$> minimumTTL,
            ("forwardedHeaders" Core..=)
              Prelude.<$> forwardedHeaders,
            ("forwardedQueryStrings" Core..=)
              Prelude.<$> forwardedQueryStrings
          ]
      )
