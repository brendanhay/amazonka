{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstanceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the number of traffic policy instances that are associated with the current AWS account.
module Network.AWS.Route53.GetTrafficPolicyInstanceCount
  ( -- * Creating a request
    GetTrafficPolicyInstanceCount (..),
    mkGetTrafficPolicyInstanceCount,

    -- * Destructuring the response
    GetTrafficPolicyInstanceCountResponse (..),
    mkGetTrafficPolicyInstanceCountResponse,

    -- ** Response lenses
    gtpicrrsTrafficPolicyInstanceCount,
    gtpicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Request to get the number of traffic policy instances that are associated with the current AWS account.
--
-- /See:/ 'mkGetTrafficPolicyInstanceCount' smart constructor.
data GetTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicyInstanceCount' value with any optional fields omitted.
mkGetTrafficPolicyInstanceCount ::
  GetTrafficPolicyInstanceCount
mkGetTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'

instance Core.AWSRequest GetTrafficPolicyInstanceCount where
  type
    Rs GetTrafficPolicyInstanceCount =
      GetTrafficPolicyInstanceCountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath "/2013-04-01/trafficpolicyinstancecount",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceCountResponse'
            Core.<$> (x Core..@ "TrafficPolicyInstanceCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkGetTrafficPolicyInstanceCountResponse' smart constructor.
data GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse'
  { -- | The number of traffic policy instances that are associated with the current AWS account.
    trafficPolicyInstanceCount :: Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicyInstanceCountResponse' value with any optional fields omitted.
mkGetTrafficPolicyInstanceCountResponse ::
  -- | 'trafficPolicyInstanceCount'
  Core.Int ->
  -- | 'responseStatus'
  Core.Int ->
  GetTrafficPolicyInstanceCountResponse
mkGetTrafficPolicyInstanceCountResponse
  trafficPolicyInstanceCount
  responseStatus =
    GetTrafficPolicyInstanceCountResponse'
      { trafficPolicyInstanceCount,
        responseStatus
      }

-- | The number of traffic policy instances that are associated with the current AWS account.
--
-- /Note:/ Consider using 'trafficPolicyInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpicrrsTrafficPolicyInstanceCount :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Core.Int
gtpicrrsTrafficPolicyInstanceCount = Lens.field @"trafficPolicyInstanceCount"
{-# DEPRECATED gtpicrrsTrafficPolicyInstanceCount "Use generic-lens or generic-optics with 'trafficPolicyInstanceCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpicrrsResponseStatus :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Core.Int
gtpicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtpicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
