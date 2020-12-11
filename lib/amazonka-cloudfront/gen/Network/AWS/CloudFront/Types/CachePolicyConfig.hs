-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyConfig
  ( CachePolicyConfig (..),

    -- * Smart constructor
    mkCachePolicyConfig,

    -- * Lenses
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcDefaultTTL,
    cpcComment,
    cpcName,
    cpcMinTTL,
  )
where

import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A cache policy configuration.
--
-- This configuration determines the following:
--
--     * The values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find a valid object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
-- /See:/ 'mkCachePolicyConfig' smart constructor.
data CachePolicyConfig = CachePolicyConfig'
  { maxTTL ::
      Lude.Maybe Lude.Integer,
    parametersInCacheKeyAndForwardedToOrigin ::
      Lude.Maybe ParametersInCacheKeyAndForwardedToOrigin,
    defaultTTL :: Lude.Maybe Lude.Integer,
    comment :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    minTTL :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachePolicyConfig' with the minimum fields required to make a request.
--
-- * 'comment' - A comment to describe the cache policy.
-- * 'defaultTTL' - The default amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value as the object’s time to live (TTL) only when the origin does /not/ send @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- The default value for this field is 86400 seconds (one day). If the value of @MinTTL@ is more than 86400 seconds, then the default value for this field is the same as the value of @MinTTL@ .
-- * 'maxTTL' - The maximum amount of time, in seconds, that objects stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value only when the origin sends @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- The default value for this field is 31536000 seconds (one year). If the value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then the default value for this field is the same as the value of @DefaultTTL@ .
-- * 'minTTL' - The minimum amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- * 'name' - A unique name to identify the cache policy.
-- * 'parametersInCacheKeyAndForwardedToOrigin' - The HTTP headers, cookies, and URL query strings to include in the cache key. The values included in the cache key are automatically included in requests that CloudFront sends to the origin.
mkCachePolicyConfig ::
  -- | 'name'
  Lude.Text ->
  -- | 'minTTL'
  Lude.Integer ->
  CachePolicyConfig
mkCachePolicyConfig pName_ pMinTTL_ =
  CachePolicyConfig'
    { maxTTL = Lude.Nothing,
      parametersInCacheKeyAndForwardedToOrigin = Lude.Nothing,
      defaultTTL = Lude.Nothing,
      comment = Lude.Nothing,
      name = pName_,
      minTTL = pMinTTL_
    }

-- | The maximum amount of time, in seconds, that objects stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value only when the origin sends @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- The default value for this field is 31536000 seconds (one year). If the value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then the default value for this field is the same as the value of @DefaultTTL@ .
--
-- /Note:/ Consider using 'maxTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMaxTTL :: Lens.Lens' CachePolicyConfig (Lude.Maybe Lude.Integer)
cpcMaxTTL = Lens.lens (maxTTL :: CachePolicyConfig -> Lude.Maybe Lude.Integer) (\s a -> s {maxTTL = a} :: CachePolicyConfig)
{-# DEPRECATED cpcMaxTTL "Use generic-lens or generic-optics with 'maxTTL' instead." #-}

-- | The HTTP headers, cookies, and URL query strings to include in the cache key. The values included in the cache key are automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'parametersInCacheKeyAndForwardedToOrigin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcParametersInCacheKeyAndForwardedToOrigin :: Lens.Lens' CachePolicyConfig (Lude.Maybe ParametersInCacheKeyAndForwardedToOrigin)
cpcParametersInCacheKeyAndForwardedToOrigin = Lens.lens (parametersInCacheKeyAndForwardedToOrigin :: CachePolicyConfig -> Lude.Maybe ParametersInCacheKeyAndForwardedToOrigin) (\s a -> s {parametersInCacheKeyAndForwardedToOrigin = a} :: CachePolicyConfig)
{-# DEPRECATED cpcParametersInCacheKeyAndForwardedToOrigin "Use generic-lens or generic-optics with 'parametersInCacheKeyAndForwardedToOrigin' instead." #-}

-- | The default amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. CloudFront uses this value as the object’s time to live (TTL) only when the origin does /not/ send @Cache-Control@ or @Expires@ headers with the object. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- The default value for this field is 86400 seconds (one day). If the value of @MinTTL@ is more than 86400 seconds, then the default value for this field is the same as the value of @MinTTL@ .
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcDefaultTTL :: Lens.Lens' CachePolicyConfig (Lude.Maybe Lude.Integer)
cpcDefaultTTL = Lens.lens (defaultTTL :: CachePolicyConfig -> Lude.Maybe Lude.Integer) (\s a -> s {defaultTTL = a} :: CachePolicyConfig)
{-# DEPRECATED cpcDefaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead." #-}

-- | A comment to describe the cache policy.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcComment :: Lens.Lens' CachePolicyConfig (Lude.Maybe Lude.Text)
cpcComment = Lens.lens (comment :: CachePolicyConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: CachePolicyConfig)
{-# DEPRECATED cpcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A unique name to identify the cache policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcName :: Lens.Lens' CachePolicyConfig Lude.Text
cpcName = Lens.lens (name :: CachePolicyConfig -> Lude.Text) (\s a -> s {name = a} :: CachePolicyConfig)
{-# DEPRECATED cpcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The minimum amount of time, in seconds, that you want objects to stay in the CloudFront cache before CloudFront sends another request to the origin to see if the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'minTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcMinTTL :: Lens.Lens' CachePolicyConfig Lude.Integer
cpcMinTTL = Lens.lens (minTTL :: CachePolicyConfig -> Lude.Integer) (\s a -> s {minTTL = a} :: CachePolicyConfig)
{-# DEPRECATED cpcMinTTL "Use generic-lens or generic-optics with 'minTTL' instead." #-}

instance Lude.FromXML CachePolicyConfig where
  parseXML x =
    CachePolicyConfig'
      Lude.<$> (x Lude..@? "MaxTTL")
      Lude.<*> (x Lude..@? "ParametersInCacheKeyAndForwardedToOrigin")
      Lude.<*> (x Lude..@? "DefaultTTL")
      Lude.<*> (x Lude..@? "Comment")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "MinTTL")

instance Lude.ToXML CachePolicyConfig where
  toXML CachePolicyConfig' {..} =
    Lude.mconcat
      [ "MaxTTL" Lude.@= maxTTL,
        "ParametersInCacheKeyAndForwardedToOrigin"
          Lude.@= parametersInCacheKeyAndForwardedToOrigin,
        "DefaultTTL" Lude.@= defaultTTL,
        "Comment" Lude.@= comment,
        "Name" Lude.@= name,
        "MinTTL" Lude.@= minTTL
      ]
