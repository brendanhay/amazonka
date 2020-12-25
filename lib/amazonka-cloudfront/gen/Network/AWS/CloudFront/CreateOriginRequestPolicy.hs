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
    corprrsETag,
    corprrsLocation,
    corprrsOriginRequestPolicy,
    corprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateOriginRequestPolicy' smart constructor.
newtype CreateOriginRequestPolicy = CreateOriginRequestPolicy'
  { -- | An origin request policy configuration.
    originRequestPolicyConfig :: Types.OriginRequestPolicyConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOriginRequestPolicy' value with any optional fields omitted.
mkCreateOriginRequestPolicy ::
  -- | 'originRequestPolicyConfig'
  Types.OriginRequestPolicyConfig ->
  CreateOriginRequestPolicy
mkCreateOriginRequestPolicy originRequestPolicyConfig =
  CreateOriginRequestPolicy' {originRequestPolicyConfig}

-- | An origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corpOriginRequestPolicyConfig :: Lens.Lens' CreateOriginRequestPolicy Types.OriginRequestPolicyConfig
corpOriginRequestPolicyConfig = Lens.field @"originRequestPolicyConfig"
{-# DEPRECATED corpOriginRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead." #-}

instance Core.AWSRequest CreateOriginRequestPolicy where
  type
    Rs CreateOriginRequestPolicy =
      CreateOriginRequestPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/origin-request-policy",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateOriginRequestPolicyResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateOriginRequestPolicyResponse' smart constructor.
data CreateOriginRequestPolicyResponse = CreateOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Core.Maybe Types.String,
    -- | The fully qualified URI of the origin request policy just created.
    location :: Core.Maybe Types.String,
    -- | An origin request policy.
    originRequestPolicy :: Core.Maybe Types.OriginRequestPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateOriginRequestPolicyResponse' value with any optional fields omitted.
mkCreateOriginRequestPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateOriginRequestPolicyResponse
mkCreateOriginRequestPolicyResponse responseStatus =
  CreateOriginRequestPolicyResponse'
    { eTag = Core.Nothing,
      location = Core.Nothing,
      originRequestPolicy = Core.Nothing,
      responseStatus
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprrsETag :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe Types.String)
corprrsETag = Lens.field @"eTag"
{-# DEPRECATED corprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the origin request policy just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprrsLocation :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe Types.String)
corprrsLocation = Lens.field @"location"
{-# DEPRECATED corprrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | An origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprrsOriginRequestPolicy :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe Types.OriginRequestPolicy)
corprrsOriginRequestPolicy = Lens.field @"originRequestPolicy"
{-# DEPRECATED corprrsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corprrsResponseStatus :: Lens.Lens' CreateOriginRequestPolicyResponse Core.Int
corprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED corprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
