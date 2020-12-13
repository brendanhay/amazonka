{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an origin request policy.
--
-- After you create an origin request policy, you can attach it to one or more cache behaviors. When it’s attached to a cache behavior, the origin request policy determines the values that CloudFront includes in requests that it sends to the origin. Each request that CloudFront sends to the origin includes the following:
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
-- CloudFront sends a request when it can’t find a valid object in its cache that matches the request. If you want to send values to the origin and also include them in the cache key, use @CachePolicy@ .
-- For more information about origin request policies, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html Controlling origin requests> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateOriginRequestPolicy
  ( -- * Creating a request
    CreateOriginRequestPolicy (..),
    mkCreateOriginRequestPolicy,

    -- ** Request lenses
    corpOriginRequestPolicyConfig,

    -- * Destructuring the response
    CreateOriginRequestPolicyResponse (..),
    mkCreateOriginRequestPolicyResponse,

    -- ** Response lenses
    corprsETag,
    corprsLocation,
    corprsOriginRequestPolicy,
    corprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateOriginRequestPolicy' smart constructor.
newtype CreateOriginRequestPolicy = CreateOriginRequestPolicy'
  { -- | An origin request policy configuration.
    originRequestPolicyConfig :: OriginRequestPolicyConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOriginRequestPolicy' with the minimum fields required to make a request.
--
-- * 'originRequestPolicyConfig' - An origin request policy configuration.
mkCreateOriginRequestPolicy ::
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  CreateOriginRequestPolicy
mkCreateOriginRequestPolicy pOriginRequestPolicyConfig_ =
  CreateOriginRequestPolicy'
    { originRequestPolicyConfig =
        pOriginRequestPolicyConfig_
    }

-- | An origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corpOriginRequestPolicyConfig :: Lens.Lens' CreateOriginRequestPolicy OriginRequestPolicyConfig
corpOriginRequestPolicyConfig = Lens.lens (originRequestPolicyConfig :: CreateOriginRequestPolicy -> OriginRequestPolicyConfig) (\s a -> s {originRequestPolicyConfig = a} :: CreateOriginRequestPolicy)
{-# DEPRECATED corpOriginRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead." #-}

instance Lude.AWSRequest CreateOriginRequestPolicy where
  type
    Rs CreateOriginRequestPolicy =
      CreateOriginRequestPolicyResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateOriginRequestPolicyResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateOriginRequestPolicy where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginRequestPolicyConfig"
      Lude.. originRequestPolicyConfig

instance Lude.ToHeaders CreateOriginRequestPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateOriginRequestPolicy where
  toPath = Lude.const "/2020-05-31/origin-request-policy"

instance Lude.ToQuery CreateOriginRequestPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOriginRequestPolicyResponse' smart constructor.
data CreateOriginRequestPolicyResponse = CreateOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Lude.Maybe Lude.Text,
    -- | The fully qualified URI of the origin request policy just created.
    location :: Lude.Maybe Lude.Text,
    -- | An origin request policy.
    originRequestPolicy :: Lude.Maybe OriginRequestPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOriginRequestPolicyResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the origin request policy.
-- * 'location' - The fully qualified URI of the origin request policy just created.
-- * 'originRequestPolicy' - An origin request policy.
-- * 'responseStatus' - The response status code.
mkCreateOriginRequestPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOriginRequestPolicyResponse
mkCreateOriginRequestPolicyResponse pResponseStatus_ =
  CreateOriginRequestPolicyResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      originRequestPolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprsETag :: Lens.Lens' CreateOriginRequestPolicyResponse (Lude.Maybe Lude.Text)
corprsETag = Lens.lens (eTag :: CreateOriginRequestPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateOriginRequestPolicyResponse)
{-# DEPRECATED corprsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the origin request policy just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprsLocation :: Lens.Lens' CreateOriginRequestPolicyResponse (Lude.Maybe Lude.Text)
corprsLocation = Lens.lens (location :: CreateOriginRequestPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateOriginRequestPolicyResponse)
{-# DEPRECATED corprsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | An origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprsOriginRequestPolicy :: Lens.Lens' CreateOriginRequestPolicyResponse (Lude.Maybe OriginRequestPolicy)
corprsOriginRequestPolicy = Lens.lens (originRequestPolicy :: CreateOriginRequestPolicyResponse -> Lude.Maybe OriginRequestPolicy) (\s a -> s {originRequestPolicy = a} :: CreateOriginRequestPolicyResponse)
{-# DEPRECATED corprsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprsResponseStatus :: Lens.Lens' CreateOriginRequestPolicyResponse Lude.Int
corprsResponseStatus = Lens.lens (responseStatus :: CreateOriginRequestPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOriginRequestPolicyResponse)
{-# DEPRECATED corprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
