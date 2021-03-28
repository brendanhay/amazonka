{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.OriginRequestPolicy
  ( OriginRequestPolicy (..)
  -- * Smart constructor
  , mkOriginRequestPolicy
  -- * Lenses
  , orpId
  , orpLastModifiedTime
  , orpOriginRequestPolicyConfig
  ) where

import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An origin request policy.
--
-- When it’s attached to a cache behavior, the origin request policy determines the values that CloudFront includes in requests that it sends to the origin. Each request that CloudFront sends to the origin includes the following:
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
-- /See:/ 'mkOriginRequestPolicy' smart constructor.
data OriginRequestPolicy = OriginRequestPolicy'
  { id :: Core.Text
    -- ^ The unique identifier for the origin request policy.
  , lastModifiedTime :: Core.UTCTime
    -- ^ The date and time when the origin request policy was last modified.
  , originRequestPolicyConfig :: Types.OriginRequestPolicyConfig
    -- ^ The origin request policy configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OriginRequestPolicy' value with any optional fields omitted.
mkOriginRequestPolicy
    :: Core.Text -- ^ 'id'
    -> Core.UTCTime -- ^ 'lastModifiedTime'
    -> Types.OriginRequestPolicyConfig -- ^ 'originRequestPolicyConfig'
    -> OriginRequestPolicy
mkOriginRequestPolicy id lastModifiedTime originRequestPolicyConfig
  = OriginRequestPolicy'{id, lastModifiedTime,
                         originRequestPolicyConfig}

-- | The unique identifier for the origin request policy.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpId :: Lens.Lens' OriginRequestPolicy Core.Text
orpId = Lens.field @"id"
{-# INLINEABLE orpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The date and time when the origin request policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpLastModifiedTime :: Lens.Lens' OriginRequestPolicy Core.UTCTime
orpLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE orpLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpOriginRequestPolicyConfig :: Lens.Lens' OriginRequestPolicy Types.OriginRequestPolicyConfig
orpOriginRequestPolicyConfig = Lens.field @"originRequestPolicyConfig"
{-# INLINEABLE orpOriginRequestPolicyConfig #-}
{-# DEPRECATED originRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead"  #-}

instance Core.FromXML OriginRequestPolicy where
        parseXML x
          = OriginRequestPolicy' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "LastModifiedTime" Core.<*>
                x Core..@ "OriginRequestPolicyConfig"
