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
-- Module      : Amazonka.Lightsail.Types.CacheSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.CacheSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.CookieObject
import Amazonka.Lightsail.Types.HeaderObject
import Amazonka.Lightsail.Types.QueryStringObject
import qualified Amazonka.Prelude as Prelude

-- | Describes the cache settings of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- These settings apply only to your distribution\'s @cacheBehaviors@
-- (including the @defaultCacheBehavior@) that have a @behavior@ of
-- @cache@.
--
-- /See:/ 'newCacheSettings' smart constructor.
data CacheSettings = CacheSettings'
  { -- | The HTTP methods that are processed and forwarded to the distribution\'s
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
    -- | The default amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the content has been updated.
    --
    -- The value specified applies only when the origin does not add HTTP
    -- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
    -- @Expires@ to objects.
    defaultTTL :: Prelude.Maybe Prelude.Integer,
    -- | An object that describes the cookies that are forwarded to the origin.
    -- Your content is cached based on the cookies that are forwarded.
    forwardedCookies :: Prelude.Maybe CookieObject,
    -- | An object that describes the headers that are forwarded to the origin.
    -- Your content is cached based on the headers that are forwarded.
    forwardedHeaders :: Prelude.Maybe HeaderObject,
    -- | An object that describes the query strings that are forwarded to the
    -- origin. Your content is cached based on the query strings that are
    -- forwarded.
    forwardedQueryStrings :: Prelude.Maybe QueryStringObject,
    -- | The maximum amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the object has been updated.
    --
    -- The value specified applies only when the origin adds HTTP headers such
    -- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
    -- objects.
    maximumTTL :: Prelude.Maybe Prelude.Integer,
    -- | The minimum amount of time that objects stay in the distribution\'s
    -- cache before the distribution forwards another request to the origin to
    -- determine whether the object has been updated.
    --
    -- A value of @0@ must be specified for @minimumTTL@ if the distribution is
    -- configured to forward all headers to the origin.
    minimumTTL :: Prelude.Maybe Prelude.Integer
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
-- 'defaultTTL', 'cacheSettings_defaultTTL' - The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
--
-- 'forwardedCookies', 'cacheSettings_forwardedCookies' - An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
--
-- 'forwardedHeaders', 'cacheSettings_forwardedHeaders' - An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
--
-- 'forwardedQueryStrings', 'cacheSettings_forwardedQueryStrings' - An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
--
-- 'maximumTTL', 'cacheSettings_maximumTTL' - The maximum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such
-- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects.
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
    { allowedHTTPMethods =
        Prelude.Nothing,
      cachedHTTPMethods = Prelude.Nothing,
      defaultTTL = Prelude.Nothing,
      forwardedCookies = Prelude.Nothing,
      forwardedHeaders = Prelude.Nothing,
      forwardedQueryStrings = Prelude.Nothing,
      maximumTTL = Prelude.Nothing,
      minimumTTL = Prelude.Nothing
    }

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

-- | The default amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the content has been updated.
--
-- The value specified applies only when the origin does not add HTTP
-- headers such as @Cache-Control max-age@, @Cache-Control s-maxage@, and
-- @Expires@ to objects.
cacheSettings_defaultTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_defaultTTL = Lens.lens (\CacheSettings' {defaultTTL} -> defaultTTL) (\s@CacheSettings' {} a -> s {defaultTTL = a} :: CacheSettings)

-- | An object that describes the cookies that are forwarded to the origin.
-- Your content is cached based on the cookies that are forwarded.
cacheSettings_forwardedCookies :: Lens.Lens' CacheSettings (Prelude.Maybe CookieObject)
cacheSettings_forwardedCookies = Lens.lens (\CacheSettings' {forwardedCookies} -> forwardedCookies) (\s@CacheSettings' {} a -> s {forwardedCookies = a} :: CacheSettings)

-- | An object that describes the headers that are forwarded to the origin.
-- Your content is cached based on the headers that are forwarded.
cacheSettings_forwardedHeaders :: Lens.Lens' CacheSettings (Prelude.Maybe HeaderObject)
cacheSettings_forwardedHeaders = Lens.lens (\CacheSettings' {forwardedHeaders} -> forwardedHeaders) (\s@CacheSettings' {} a -> s {forwardedHeaders = a} :: CacheSettings)

-- | An object that describes the query strings that are forwarded to the
-- origin. Your content is cached based on the query strings that are
-- forwarded.
cacheSettings_forwardedQueryStrings :: Lens.Lens' CacheSettings (Prelude.Maybe QueryStringObject)
cacheSettings_forwardedQueryStrings = Lens.lens (\CacheSettings' {forwardedQueryStrings} -> forwardedQueryStrings) (\s@CacheSettings' {} a -> s {forwardedQueryStrings = a} :: CacheSettings)

-- | The maximum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such
-- as @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects.
cacheSettings_maximumTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_maximumTTL = Lens.lens (\CacheSettings' {maximumTTL} -> maximumTTL) (\s@CacheSettings' {} a -> s {maximumTTL = a} :: CacheSettings)

-- | The minimum amount of time that objects stay in the distribution\'s
-- cache before the distribution forwards another request to the origin to
-- determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is
-- configured to forward all headers to the origin.
cacheSettings_minimumTTL :: Lens.Lens' CacheSettings (Prelude.Maybe Prelude.Integer)
cacheSettings_minimumTTL = Lens.lens (\CacheSettings' {minimumTTL} -> minimumTTL) (\s@CacheSettings' {} a -> s {minimumTTL = a} :: CacheSettings)

instance Data.FromJSON CacheSettings where
  parseJSON =
    Data.withObject
      "CacheSettings"
      ( \x ->
          CacheSettings'
            Prelude.<$> (x Data..:? "allowedHTTPMethods")
            Prelude.<*> (x Data..:? "cachedHTTPMethods")
            Prelude.<*> (x Data..:? "defaultTTL")
            Prelude.<*> (x Data..:? "forwardedCookies")
            Prelude.<*> (x Data..:? "forwardedHeaders")
            Prelude.<*> (x Data..:? "forwardedQueryStrings")
            Prelude.<*> (x Data..:? "maximumTTL")
            Prelude.<*> (x Data..:? "minimumTTL")
      )

instance Prelude.Hashable CacheSettings where
  hashWithSalt _salt CacheSettings' {..} =
    _salt
      `Prelude.hashWithSalt` allowedHTTPMethods
      `Prelude.hashWithSalt` cachedHTTPMethods
      `Prelude.hashWithSalt` defaultTTL
      `Prelude.hashWithSalt` forwardedCookies
      `Prelude.hashWithSalt` forwardedHeaders
      `Prelude.hashWithSalt` forwardedQueryStrings
      `Prelude.hashWithSalt` maximumTTL
      `Prelude.hashWithSalt` minimumTTL

instance Prelude.NFData CacheSettings where
  rnf CacheSettings' {..} =
    Prelude.rnf allowedHTTPMethods
      `Prelude.seq` Prelude.rnf cachedHTTPMethods
      `Prelude.seq` Prelude.rnf defaultTTL
      `Prelude.seq` Prelude.rnf forwardedCookies
      `Prelude.seq` Prelude.rnf forwardedHeaders
      `Prelude.seq` Prelude.rnf forwardedQueryStrings
      `Prelude.seq` Prelude.rnf maximumTTL
      `Prelude.seq` Prelude.rnf minimumTTL

instance Data.ToJSON CacheSettings where
  toJSON CacheSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedHTTPMethods" Data..=)
              Prelude.<$> allowedHTTPMethods,
            ("cachedHTTPMethods" Data..=)
              Prelude.<$> cachedHTTPMethods,
            ("defaultTTL" Data..=) Prelude.<$> defaultTTL,
            ("forwardedCookies" Data..=)
              Prelude.<$> forwardedCookies,
            ("forwardedHeaders" Data..=)
              Prelude.<$> forwardedHeaders,
            ("forwardedQueryStrings" Data..=)
              Prelude.<$> forwardedQueryStrings,
            ("maximumTTL" Data..=) Prelude.<$> maximumTTL,
            ("minimumTTL" Data..=) Prelude.<$> minimumTTL
          ]
      )
