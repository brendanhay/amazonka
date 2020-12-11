{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy configuration.
--
-- To get an origin request policy configuration, you must provide the policy’s identifier. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
module Network.AWS.CloudFront.GetOriginRequestPolicyConfig
  ( -- * Creating a request
    GetOriginRequestPolicyConfig (..),
    mkGetOriginRequestPolicyConfig,

    -- ** Request lenses
    gorpcId,

    -- * Destructuring the response
    GetOriginRequestPolicyConfigResponse (..),
    mkGetOriginRequestPolicyConfigResponse,

    -- ** Response lenses
    gorpcrsETag,
    gorpcrsOriginRequestPolicyConfig,
    gorpcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOriginRequestPolicyConfig' smart constructor.
newtype GetOriginRequestPolicyConfig = GetOriginRequestPolicyConfig'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOriginRequestPolicyConfig' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
mkGetOriginRequestPolicyConfig ::
  -- | 'id'
  Lude.Text ->
  GetOriginRequestPolicyConfig
mkGetOriginRequestPolicyConfig pId_ =
  GetOriginRequestPolicyConfig' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcId :: Lens.Lens' GetOriginRequestPolicyConfig Lude.Text
gorpcId = Lens.lens (id :: GetOriginRequestPolicyConfig -> Lude.Text) (\s a -> s {id = a} :: GetOriginRequestPolicyConfig)
{-# DEPRECATED gorpcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetOriginRequestPolicyConfig where
  type
    Rs GetOriginRequestPolicyConfig =
      GetOriginRequestPolicyConfigResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyConfigResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOriginRequestPolicyConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetOriginRequestPolicyConfig where
  toPath GetOriginRequestPolicyConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/origin-request-policy/", Lude.toBS id, "/config"]

instance Lude.ToQuery GetOriginRequestPolicyConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOriginRequestPolicyConfigResponse' smart constructor.
data GetOriginRequestPolicyConfigResponse = GetOriginRequestPolicyConfigResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    originRequestPolicyConfig ::
      Lude.Maybe
        OriginRequestPolicyConfig,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOriginRequestPolicyConfigResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the origin request policy.
-- * 'originRequestPolicyConfig' - The origin request policy configuration.
-- * 'responseStatus' - The response status code.
mkGetOriginRequestPolicyConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOriginRequestPolicyConfigResponse
mkGetOriginRequestPolicyConfigResponse pResponseStatus_ =
  GetOriginRequestPolicyConfigResponse'
    { eTag = Lude.Nothing,
      originRequestPolicyConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrsETag :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Lude.Maybe Lude.Text)
gorpcrsETag = Lens.lens (eTag :: GetOriginRequestPolicyConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetOriginRequestPolicyConfigResponse)
{-# DEPRECATED gorpcrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrsOriginRequestPolicyConfig :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Lude.Maybe OriginRequestPolicyConfig)
gorpcrsOriginRequestPolicyConfig = Lens.lens (originRequestPolicyConfig :: GetOriginRequestPolicyConfigResponse -> Lude.Maybe OriginRequestPolicyConfig) (\s a -> s {originRequestPolicyConfig = a} :: GetOriginRequestPolicyConfigResponse)
{-# DEPRECATED gorpcrsOriginRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrsResponseStatus :: Lens.Lens' GetOriginRequestPolicyConfigResponse Lude.Int
gorpcrsResponseStatus = Lens.lens (responseStatus :: GetOriginRequestPolicyConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOriginRequestPolicyConfigResponse)
{-# DEPRECATED gorpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
