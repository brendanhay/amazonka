{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific traffic policy version.
--
-- For information about how of deleting a traffic policy affects the response from @GetTrafficPolicy@ , see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy> .
module Network.AWS.Route53.GetTrafficPolicy
  ( -- * Creating a request
    GetTrafficPolicy (..),
    mkGetTrafficPolicy,

    -- ** Request lenses
    gtpId,
    gtpVersion,

    -- * Destructuring the response
    GetTrafficPolicyResponse (..),
    mkGetTrafficPolicyResponse,

    -- ** Response lenses
    gtprrsTrafficPolicy,
    gtprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Gets information about a specific traffic policy version.
--
-- /See:/ 'mkGetTrafficPolicy' smart constructor.
data GetTrafficPolicy = GetTrafficPolicy'
  { -- | The ID of the traffic policy that you want to get information about.
    id :: Types.Id,
    -- | The version number of the traffic policy that you want to get information about.
    version :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicy' value with any optional fields omitted.
mkGetTrafficPolicy ::
  -- | 'id'
  Types.Id ->
  -- | 'version'
  Core.Natural ->
  GetTrafficPolicy
mkGetTrafficPolicy id version = GetTrafficPolicy' {id, version}

-- | The ID of the traffic policy that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpId :: Lens.Lens' GetTrafficPolicy Types.Id
gtpId = Lens.field @"id"
{-# DEPRECATED gtpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version number of the traffic policy that you want to get information about.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpVersion :: Lens.Lens' GetTrafficPolicy Core.Natural
gtpVersion = Lens.field @"version"
{-# DEPRECATED gtpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest GetTrafficPolicy where
  type Rs GetTrafficPolicy = GetTrafficPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/trafficpolicy/" Core.<> (Core.toText id)
                Core.<> ("/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyResponse'
            Core.<$> (x Core..@ "TrafficPolicy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkGetTrafficPolicyResponse' smart constructor.
data GetTrafficPolicyResponse = GetTrafficPolicyResponse'
  { -- | A complex type that contains settings for the specified traffic policy.
    trafficPolicy :: Types.TrafficPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicyResponse' value with any optional fields omitted.
mkGetTrafficPolicyResponse ::
  -- | 'trafficPolicy'
  Types.TrafficPolicy ->
  -- | 'responseStatus'
  Core.Int ->
  GetTrafficPolicyResponse
mkGetTrafficPolicyResponse trafficPolicy responseStatus =
  GetTrafficPolicyResponse' {trafficPolicy, responseStatus}

-- | A complex type that contains settings for the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtprrsTrafficPolicy :: Lens.Lens' GetTrafficPolicyResponse Types.TrafficPolicy
gtprrsTrafficPolicy = Lens.field @"trafficPolicy"
{-# DEPRECATED gtprrsTrafficPolicy "Use generic-lens or generic-optics with 'trafficPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtprrsResponseStatus :: Lens.Lens' GetTrafficPolicyResponse Core.Int
gtprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
