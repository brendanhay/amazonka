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
    gorprrsETag,
    gorprrsOriginRequestPolicy,
    gorprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOriginRequestPolicy' smart constructor.
newtype GetOriginRequestPolicy = GetOriginRequestPolicy'
  { -- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOriginRequestPolicy' value with any optional fields omitted.
mkGetOriginRequestPolicy ::
  -- | 'id'
  Types.String ->
  GetOriginRequestPolicy
mkGetOriginRequestPolicy id = GetOriginRequestPolicy' {id}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpId :: Lens.Lens' GetOriginRequestPolicy Types.String
gorpId = Lens.field @"id"
{-# DEPRECATED gorpId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetOriginRequestPolicy where
  type Rs GetOriginRequestPolicy = GetOriginRequestPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/origin-request-policy/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyResponse'
            Core.<$> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetOriginRequestPolicyResponse' smart constructor.
data GetOriginRequestPolicyResponse = GetOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Core.Maybe Types.String,
    -- | The origin request policy.
    originRequestPolicy :: Core.Maybe Types.OriginRequestPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetOriginRequestPolicyResponse' value with any optional fields omitted.
mkGetOriginRequestPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOriginRequestPolicyResponse
mkGetOriginRequestPolicyResponse responseStatus =
  GetOriginRequestPolicyResponse'
    { eTag = Core.Nothing,
      originRequestPolicy = Core.Nothing,
      responseStatus
    }

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprrsETag :: Lens.Lens' GetOriginRequestPolicyResponse (Core.Maybe Types.String)
gorprrsETag = Lens.field @"eTag"
{-# DEPRECATED gorprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprrsOriginRequestPolicy :: Lens.Lens' GetOriginRequestPolicyResponse (Core.Maybe Types.OriginRequestPolicy)
gorprrsOriginRequestPolicy = Lens.field @"originRequestPolicy"
{-# DEPRECATED gorprrsOriginRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorprrsResponseStatus :: Lens.Lens' GetOriginRequestPolicyResponse Core.Int
gorprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gorprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
