-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheSettings
  ( CacheSettings (..),

    -- * Smart constructor
    mkCacheSettings,

    -- * Lenses
    csMaximumTTL,
    csCachedHTTPMethods,
    csForwardedCookies,
    csAllowedHTTPMethods,
    csDefaultTTL,
    csMinimumTTL,
    csForwardedHeaders,
    csForwardedQueryStrings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CookieObject
import Network.AWS.Lightsail.Types.HeaderObject
import Network.AWS.Lightsail.Types.QueryStringObject
import qualified Network.AWS.Prelude as Lude

-- | Describes the cache settings of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- These settings apply only to your distribution's @cacheBehaviors@ (including the @defaultCacheBehavior@ ) that have a @behavior@ of @cache@ .
--
-- /See:/ 'mkCacheSettings' smart constructor.
data CacheSettings = CacheSettings'
  { maximumTTL ::
      Lude.Maybe Lude.Integer,
    cachedHTTPMethods :: Lude.Maybe Lude.Text,
    forwardedCookies :: Lude.Maybe CookieObject,
    allowedHTTPMethods :: Lude.Maybe Lude.Text,
    defaultTTL :: Lude.Maybe Lude.Integer,
    minimumTTL :: Lude.Maybe Lude.Integer,
    forwardedHeaders :: Lude.Maybe HeaderObject,
    forwardedQueryStrings :: Lude.Maybe QueryStringObject
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheSettings' with the minimum fields required to make a request.
--
-- * 'allowedHTTPMethods' - The HTTP methods that are processed and forwarded to the distribution's origin.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--     * @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards the @GET@ , @HEAD@ , @OPTIONS@ , @PUT@ , @PATCH@ , @POST@ , and @DELETE@ methods.
--
--
-- If you specify the third option, you might need to restrict access to your distribution's origin so users can't perform operations that you don't want them to. For example, you might not want users to have permission to delete objects from your origin.
-- * 'cachedHTTPMethods' - The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution caches responses to the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
-- * 'defaultTTL' - The default amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the content has been updated.
-- * 'forwardedCookies' - An object that describes the cookies that are forwarded to the origin. Your content is cached based on the cookies that are forwarded.
-- * 'forwardedHeaders' - An object that describes the headers that are forwarded to the origin. Your content is cached based on the headers that are forwarded.
-- * 'forwardedQueryStrings' - An object that describes the query strings that are forwarded to the origin. Your content is cached based on the query strings that are forwarded.
-- * 'maximumTTL' - The maximum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects.
-- * 'minimumTTL' - The minimum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is configured to forward all headers to the origin.
mkCacheSettings ::
  CacheSettings
mkCacheSettings =
  CacheSettings'
    { maximumTTL = Lude.Nothing,
      cachedHTTPMethods = Lude.Nothing,
      forwardedCookies = Lude.Nothing,
      allowedHTTPMethods = Lude.Nothing,
      defaultTTL = Lude.Nothing,
      minimumTTL = Lude.Nothing,
      forwardedHeaders = Lude.Nothing,
      forwardedQueryStrings = Lude.Nothing
    }

-- | The maximum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects.
--
-- /Note:/ Consider using 'maximumTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMaximumTTL :: Lens.Lens' CacheSettings (Lude.Maybe Lude.Integer)
csMaximumTTL = Lens.lens (maximumTTL :: CacheSettings -> Lude.Maybe Lude.Integer) (\s a -> s {maximumTTL = a} :: CacheSettings)
{-# DEPRECATED csMaximumTTL "Use generic-lens or generic-optics with 'maximumTTL' instead." #-}

-- | The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution caches responses to the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--
-- /Note:/ Consider using 'cachedHTTPMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCachedHTTPMethods :: Lens.Lens' CacheSettings (Lude.Maybe Lude.Text)
csCachedHTTPMethods = Lens.lens (cachedHTTPMethods :: CacheSettings -> Lude.Maybe Lude.Text) (\s a -> s {cachedHTTPMethods = a} :: CacheSettings)
{-# DEPRECATED csCachedHTTPMethods "Use generic-lens or generic-optics with 'cachedHTTPMethods' instead." #-}

-- | An object that describes the cookies that are forwarded to the origin. Your content is cached based on the cookies that are forwarded.
--
-- /Note:/ Consider using 'forwardedCookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedCookies :: Lens.Lens' CacheSettings (Lude.Maybe CookieObject)
csForwardedCookies = Lens.lens (forwardedCookies :: CacheSettings -> Lude.Maybe CookieObject) (\s a -> s {forwardedCookies = a} :: CacheSettings)
{-# DEPRECATED csForwardedCookies "Use generic-lens or generic-optics with 'forwardedCookies' instead." #-}

-- | The HTTP methods that are processed and forwarded to the distribution's origin.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--     * @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards the @GET@ , @HEAD@ , @OPTIONS@ , @PUT@ , @PATCH@ , @POST@ , and @DELETE@ methods.
--
--
-- If you specify the third option, you might need to restrict access to your distribution's origin so users can't perform operations that you don't want them to. For example, you might not want users to have permission to delete objects from your origin.
--
-- /Note:/ Consider using 'allowedHTTPMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAllowedHTTPMethods :: Lens.Lens' CacheSettings (Lude.Maybe Lude.Text)
csAllowedHTTPMethods = Lens.lens (allowedHTTPMethods :: CacheSettings -> Lude.Maybe Lude.Text) (\s a -> s {allowedHTTPMethods = a} :: CacheSettings)
{-# DEPRECATED csAllowedHTTPMethods "Use generic-lens or generic-optics with 'allowedHTTPMethods' instead." #-}

-- | The default amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the content has been updated.
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultTTL :: Lens.Lens' CacheSettings (Lude.Maybe Lude.Integer)
csDefaultTTL = Lens.lens (defaultTTL :: CacheSettings -> Lude.Maybe Lude.Integer) (\s a -> s {defaultTTL = a} :: CacheSettings)
{-# DEPRECATED csDefaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead." #-}

-- | The minimum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is configured to forward all headers to the origin.
--
-- /Note:/ Consider using 'minimumTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMinimumTTL :: Lens.Lens' CacheSettings (Lude.Maybe Lude.Integer)
csMinimumTTL = Lens.lens (minimumTTL :: CacheSettings -> Lude.Maybe Lude.Integer) (\s a -> s {minimumTTL = a} :: CacheSettings)
{-# DEPRECATED csMinimumTTL "Use generic-lens or generic-optics with 'minimumTTL' instead." #-}

-- | An object that describes the headers that are forwarded to the origin. Your content is cached based on the headers that are forwarded.
--
-- /Note:/ Consider using 'forwardedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedHeaders :: Lens.Lens' CacheSettings (Lude.Maybe HeaderObject)
csForwardedHeaders = Lens.lens (forwardedHeaders :: CacheSettings -> Lude.Maybe HeaderObject) (\s a -> s {forwardedHeaders = a} :: CacheSettings)
{-# DEPRECATED csForwardedHeaders "Use generic-lens or generic-optics with 'forwardedHeaders' instead." #-}

-- | An object that describes the query strings that are forwarded to the origin. Your content is cached based on the query strings that are forwarded.
--
-- /Note:/ Consider using 'forwardedQueryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedQueryStrings :: Lens.Lens' CacheSettings (Lude.Maybe QueryStringObject)
csForwardedQueryStrings = Lens.lens (forwardedQueryStrings :: CacheSettings -> Lude.Maybe QueryStringObject) (\s a -> s {forwardedQueryStrings = a} :: CacheSettings)
{-# DEPRECATED csForwardedQueryStrings "Use generic-lens or generic-optics with 'forwardedQueryStrings' instead." #-}

instance Lude.FromJSON CacheSettings where
  parseJSON =
    Lude.withObject
      "CacheSettings"
      ( \x ->
          CacheSettings'
            Lude.<$> (x Lude..:? "maximumTTL")
            Lude.<*> (x Lude..:? "cachedHTTPMethods")
            Lude.<*> (x Lude..:? "forwardedCookies")
            Lude.<*> (x Lude..:? "allowedHTTPMethods")
            Lude.<*> (x Lude..:? "defaultTTL")
            Lude.<*> (x Lude..:? "minimumTTL")
            Lude.<*> (x Lude..:? "forwardedHeaders")
            Lude.<*> (x Lude..:? "forwardedQueryStrings")
      )

instance Lude.ToJSON CacheSettings where
  toJSON CacheSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maximumTTL" Lude..=) Lude.<$> maximumTTL,
            ("cachedHTTPMethods" Lude..=) Lude.<$> cachedHTTPMethods,
            ("forwardedCookies" Lude..=) Lude.<$> forwardedCookies,
            ("allowedHTTPMethods" Lude..=) Lude.<$> allowedHTTPMethods,
            ("defaultTTL" Lude..=) Lude.<$> defaultTTL,
            ("minimumTTL" Lude..=) Lude.<$> minimumTTL,
            ("forwardedHeaders" Lude..=) Lude.<$> forwardedHeaders,
            ("forwardedQueryStrings" Lude..=) Lude.<$> forwardedQueryStrings
          ]
      )
