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
    orpcName,
    orpcHeadersConfig,
    orpcCookiesConfig,
    orpcQueryStringsConfig,
    orpcComment,
  )
where

import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.Name as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | A unique name to identify the origin request policy.
    name :: Types.Name,
    -- | The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
    headersConfig :: Types.OriginRequestPolicyHeadersConfig,
    -- | The cookies from viewer requests to include in origin requests.
    cookiesConfig :: Types.OriginRequestPolicyCookiesConfig,
    -- | The URL query strings from viewer requests to include in origin requests.
    queryStringsConfig :: Types.OriginRequestPolicyQueryStringsConfig,
    -- | A comment to describe the origin request policy.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginRequestPolicyConfig' value with any optional fields omitted.
mkOriginRequestPolicyConfig ::
  -- | 'name'
  Types.Name ->
  -- | 'headersConfig'
  Types.OriginRequestPolicyHeadersConfig ->
  -- | 'cookiesConfig'
  Types.OriginRequestPolicyCookiesConfig ->
  -- | 'queryStringsConfig'
  Types.OriginRequestPolicyQueryStringsConfig ->
  OriginRequestPolicyConfig
mkOriginRequestPolicyConfig
  name
  headersConfig
  cookiesConfig
  queryStringsConfig =
    OriginRequestPolicyConfig'
      { name,
        headersConfig,
        cookiesConfig,
        queryStringsConfig,
        comment = Core.Nothing
      }

-- | A unique name to identify the origin request policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcName :: Lens.Lens' OriginRequestPolicyConfig Types.Name
orpcName = Lens.field @"name"
{-# DEPRECATED orpcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The HTTP headers to include in origin requests. These can include headers from viewer requests and additional headers added by CloudFront.
--
-- /Note:/ Consider using 'headersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcHeadersConfig :: Lens.Lens' OriginRequestPolicyConfig Types.OriginRequestPolicyHeadersConfig
orpcHeadersConfig = Lens.field @"headersConfig"
{-# DEPRECATED orpcHeadersConfig "Use generic-lens or generic-optics with 'headersConfig' instead." #-}

-- | The cookies from viewer requests to include in origin requests.
--
-- /Note:/ Consider using 'cookiesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcCookiesConfig :: Lens.Lens' OriginRequestPolicyConfig Types.OriginRequestPolicyCookiesConfig
orpcCookiesConfig = Lens.field @"cookiesConfig"
{-# DEPRECATED orpcCookiesConfig "Use generic-lens or generic-optics with 'cookiesConfig' instead." #-}

-- | The URL query strings from viewer requests to include in origin requests.
--
-- /Note:/ Consider using 'queryStringsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcQueryStringsConfig :: Lens.Lens' OriginRequestPolicyConfig Types.OriginRequestPolicyQueryStringsConfig
orpcQueryStringsConfig = Lens.field @"queryStringsConfig"
{-# DEPRECATED orpcQueryStringsConfig "Use generic-lens or generic-optics with 'queryStringsConfig' instead." #-}

-- | A comment to describe the origin request policy.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpcComment :: Lens.Lens' OriginRequestPolicyConfig (Core.Maybe Types.Comment)
orpcComment = Lens.field @"comment"
{-# DEPRECATED orpcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML OriginRequestPolicyConfig where
  toXML OriginRequestPolicyConfig {..} =
    Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "HeadersConfig" headersConfig
      Core.<> Core.toXMLNode "CookiesConfig" cookiesConfig
      Core.<> Core.toXMLNode "QueryStringsConfig" queryStringsConfig
      Core.<> Core.toXMLNode "Comment" Core.<$> comment

instance Core.FromXML OriginRequestPolicyConfig where
  parseXML x =
    OriginRequestPolicyConfig'
      Core.<$> (x Core..@ "Name")
      Core.<*> (x Core..@ "HeadersConfig")
      Core.<*> (x Core..@ "CookiesConfig")
      Core.<*> (x Core..@ "QueryStringsConfig")
      Core.<*> (x Core..@? "Comment")
