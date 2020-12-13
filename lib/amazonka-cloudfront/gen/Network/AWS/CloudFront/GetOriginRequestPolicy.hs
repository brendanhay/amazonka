{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy, including the following metadata:
--
--
--     * The policy’s identifier.
--
--
--     * The date and time when the policy was last modified.
--
--
-- To get an origin request policy, you must provide the policy’s identifier. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
module Network.AWS.CloudFront.GetOriginRequestPolicy
  ( -- * Creating a request
    GetOriginRequestPolicy (..),
    mkGetOriginRequestPolicy,

    -- ** Request lenses
    gorpId,

    -- * Destructuring the response
    GetOriginRequestPolicyResponse (..),
    mkGetOriginRequestPolicyResponse,

    -- ** Response lenses
    gorprsETag,
    gorprsOriginRequestPolicy,
    gorprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOriginRequestPolicy' smart constructor.
newtype GetOriginRequestPolicy = GetOriginRequestPolicy'
  { -- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOriginRequestPolicy' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
mkGetOriginRequestPolicy ::
  -- | 'id'
  Lude.Text ->
  GetOriginRequestPolicy
mkGetOriginRequestPolicy pId_ = GetOriginRequestPolicy' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpId :: Lens.Lens' GetOriginRequestPolicy Lude.Text
gorpId = Lens.lens (id :: GetOriginRequestPolicy -> Lude.Text) (\s a -> s {id = a} :: GetOriginRequestPolicy)
{-# DEPRECATED gorpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetOriginRequestPolicy where
  type Rs GetOriginRequestPolicy = GetOriginRequestPolicyResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOriginRequestPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetOriginRequestPolicy where
  toPath GetOriginRequestPolicy' {..} =
    Lude.mconcat ["/2020-05-31/origin-request-policy/", Lude.toBS id]

instance Lude.ToQuery GetOriginRequestPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetOriginRequestPolicyResponse' smart constructor.
data GetOriginRequestPolicyResponse = GetOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Lude.Maybe Lude.Text,
    -- | The origin request policy.
    originRequestPolicy :: Lude.Maybe OriginRequestPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOriginRequestPolicyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the origin request policy.
-- * 'originRequestPolicy' - The origin request policy.
-- * 'responseStatus' - The response status code.
mkGetOriginRequestPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOriginRequestPolicyResponse
mkGetOriginRequestPolicyResponse pResponseStatus_ =
  GetOriginRequestPolicyResponse'
    { eTag = Lude.Nothing,
      originRequestPolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprsETag :: Lens.Lens' GetOriginRequestPolicyResponse (Lude.Maybe Lude.Text)
gorprsETag = Lens.lens (eTag :: GetOriginRequestPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetOriginRequestPolicyResponse)
{-# DEPRECATED gorprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprsOriginRequestPolicy :: Lens.Lens' GetOriginRequestPolicyResponse (Lude.Maybe OriginRequestPolicy)
gorprsOriginRequestPolicy = Lens.lens (originRequestPolicy :: GetOriginRequestPolicyResponse -> Lude.Maybe OriginRequestPolicy) (\s a -> s {originRequestPolicy = a} :: GetOriginRequestPolicyResponse)
{-# DEPRECATED gorprsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprsResponseStatus :: Lens.Lens' GetOriginRequestPolicyResponse Lude.Int
gorprsResponseStatus = Lens.lens (responseStatus :: GetOriginRequestPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOriginRequestPolicyResponse)
{-# DEPRECATED gorprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
