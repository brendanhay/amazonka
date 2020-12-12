{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicy
  ( OriginRequestPolicy (..),

    -- * Smart constructor
    mkOriginRequestPolicy,

    -- * Lenses
    orpId,
    orpLastModifiedTime,
    orpOriginRequestPolicyConfig,
  )
where

import Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { id :: Lude.Text,
    lastModifiedTime :: Lude.DateTime,
    originRequestPolicyConfig ::
      OriginRequestPolicyConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the origin request policy.
-- * 'lastModifiedTime' - The date and time when the origin request policy was last modified.
-- * 'originRequestPolicyConfig' - The origin request policy configuration.
mkOriginRequestPolicy ::
  -- | 'id'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.DateTime ->
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  OriginRequestPolicy
mkOriginRequestPolicy
  pId_
  pLastModifiedTime_
  pOriginRequestPolicyConfig_ =
    OriginRequestPolicy'
      { id = pId_,
        lastModifiedTime = pLastModifiedTime_,
        originRequestPolicyConfig = pOriginRequestPolicyConfig_
      }

-- | The unique identifier for the origin request policy.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpId :: Lens.Lens' OriginRequestPolicy Lude.Text
orpId = Lens.lens (id :: OriginRequestPolicy -> Lude.Text) (\s a -> s {id = a} :: OriginRequestPolicy)
{-# DEPRECATED orpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the origin request policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpLastModifiedTime :: Lens.Lens' OriginRequestPolicy Lude.DateTime
orpLastModifiedTime = Lens.lens (lastModifiedTime :: OriginRequestPolicy -> Lude.DateTime) (\s a -> s {lastModifiedTime = a} :: OriginRequestPolicy)
{-# DEPRECATED orpLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpOriginRequestPolicyConfig :: Lens.Lens' OriginRequestPolicy OriginRequestPolicyConfig
orpOriginRequestPolicyConfig = Lens.lens (originRequestPolicyConfig :: OriginRequestPolicy -> OriginRequestPolicyConfig) (\s a -> s {originRequestPolicyConfig = a} :: OriginRequestPolicy)
{-# DEPRECATED orpOriginRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead." #-}

instance Lude.FromXML OriginRequestPolicy where
  parseXML x =
    OriginRequestPolicy'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "OriginRequestPolicyConfig")
