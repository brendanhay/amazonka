{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
  ( OriginRequestPolicyConfig (..),

    -- * Smart constructor
    mkOriginRequestPolicyConfig,

    -- * Lenses
    orpcQueryStringsConfig,
    orpcHeadersConfig,
    orpcName,
    orpcCookiesConfig,
    orpcComment,
  )
where

import Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An origin request policy configuration.
--
-- This configuration determines the values that CloudFront includes in requests that it sends to the origin. Each request that CloudFront sends to the origin includes the following:
--
--     * The request body and the URL path (without the domain name) from the viewer request.
--
--
--     * The headers that CloudFront automatically includes in every origin request, including @Host@ , @User-Agent@ , and @X-Amz-Cf-Id@ .
--
--
--     * All HTTP headers, cookies, and URL query strings that are specified in the cache policy or the origin request policy. These can include items from the viewer request and, in the case of headers, additional ones that are added by CloudFront.
--
--
-- CloudFront sends a request when it can’t find an object in its cache that matches the request. If you want to send values to the origin and also include them in the cache key, use @CachePolicy@ .
--
-- /See:/ 'mkOriginRequestPolicyConfig' smart constructor.
data OriginRequestPolicyConfig = OriginRequestPolicyConfig'
  { -- | The URL query strings from viewer requests to include in origin requests.
    queryStringsConfig :: OriginRequestPolicyQueryStringsConfig,
    -- | The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
    headersConfig :: OriginRequestPolicyHeadersConfig,
    -- | A unique name to identify the origin request policy.
    name :: Lude.Text,
    -- | The cookies from viewer requests to include in origin requests.
    cookiesConfig :: OriginRequestPolicyCookiesConfig,
    -- | A comment to describe the origin request policy.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicyConfig' with the minimum fields required to make a request.
--
-- * 'queryStringsConfig' - The URL query strings from viewer requests to include in origin requests.
-- * 'headersConfig' - The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
-- * 'name' - A unique name to identify the origin request policy.
-- * 'cookiesConfig' - The cookies from viewer requests to include in origin requests.
-- * 'comment' - A comment to describe the origin request policy.
mkOriginRequestPolicyConfig ::
  -- | 'queryStringsConfig'
  OriginRequestPolicyQueryStringsConfig ->
  -- | 'headersConfig'
  OriginRequestPolicyHeadersConfig ->
  -- | 'name'
  Lude.Text ->
  -- | 'cookiesConfig'
  OriginRequestPolicyCookiesConfig ->
  OriginRequestPolicyConfig
mkOriginRequestPolicyConfig
  pQueryStringsConfig_
  pHeadersConfig_
  pName_
  pCookiesConfig_ =
    OriginRequestPolicyConfig'
      { queryStringsConfig =
          pQueryStringsConfig_,
        headersConfig = pHeadersConfig_,
        name = pName_,
        cookiesConfig = pCookiesConfig_,
        comment = Lude.Nothing
      }

-- | The URL query strings from viewer requests to include in origin requests.
--
-- /Note:/ Consider using 'queryStringsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcQueryStringsConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyQueryStringsConfig
orpcQueryStringsConfig = Lens.lens (queryStringsConfig :: OriginRequestPolicyConfig -> OriginRequestPolicyQueryStringsConfig) (\s a -> s {queryStringsConfig = a} :: OriginRequestPolicyConfig)
{-# DEPRECATED orpcQueryStringsConfig "Use generic-lens or generic-optics with 'queryStringsConfig' instead." #-}

-- | The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
--
-- /Note:/ Consider using 'headersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcHeadersConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyHeadersConfig
orpcHeadersConfig = Lens.lens (headersConfig :: OriginRequestPolicyConfig -> OriginRequestPolicyHeadersConfig) (\s a -> s {headersConfig = a} :: OriginRequestPolicyConfig)
{-# DEPRECATED orpcHeadersConfig "Use generic-lens or generic-optics with 'headersConfig' instead." #-}

-- | A unique name to identify the origin request policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcName :: Lens.Lens' OriginRequestPolicyConfig Lude.Text
orpcName = Lens.lens (name :: OriginRequestPolicyConfig -> Lude.Text) (\s a -> s {name = a} :: OriginRequestPolicyConfig)
{-# DEPRECATED orpcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The cookies from viewer requests to include in origin requests.
--
-- /Note:/ Consider using 'cookiesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcCookiesConfig :: Lens.Lens' OriginRequestPolicyConfig OriginRequestPolicyCookiesConfig
orpcCookiesConfig = Lens.lens (cookiesConfig :: OriginRequestPolicyConfig -> OriginRequestPolicyCookiesConfig) (\s a -> s {cookiesConfig = a} :: OriginRequestPolicyConfig)
{-# DEPRECATED orpcCookiesConfig "Use generic-lens or generic-optics with 'cookiesConfig' instead." #-}

-- | A comment to describe the origin request policy.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcComment :: Lens.Lens' OriginRequestPolicyConfig (Lude.Maybe Lude.Text)
orpcComment = Lens.lens (comment :: OriginRequestPolicyConfig -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: OriginRequestPolicyConfig)
{-# DEPRECATED orpcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML OriginRequestPolicyConfig where
  parseXML x =
    OriginRequestPolicyConfig'
      Lude.<$> (x Lude..@ "QueryStringsConfig")
      Lude.<*> (x Lude..@ "HeadersConfig")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "CookiesConfig")
      Lude.<*> (x Lude..@? "Comment")

instance Lude.ToXML OriginRequestPolicyConfig where
  toXML OriginRequestPolicyConfig' {..} =
    Lude.mconcat
      [ "QueryStringsConfig" Lude.@= queryStringsConfig,
        "HeadersConfig" Lude.@= headersConfig,
        "Name" Lude.@= name,
        "CookiesConfig" Lude.@= cookiesConfig,
        "Comment" Lude.@= comment
      ]
