{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified traffic policy instance.
module Network.AWS.Route53.GetTrafficPolicyInstance
  ( -- * Creating a request
    GetTrafficPolicyInstance (..),
    mkGetTrafficPolicyInstance,

    -- ** Request lenses
    gtpiId,

    -- * Destructuring the response
    GetTrafficPolicyInstanceResponse (..),
    mkGetTrafficPolicyInstanceResponse,

    -- ** Response lenses
    gtpirrsTrafficPolicyInstance,
    gtpirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Gets information about a specified traffic policy instance.
--
-- /See:/ 'mkGetTrafficPolicyInstance' smart constructor.
newtype GetTrafficPolicyInstance = GetTrafficPolicyInstance'
  { -- | The ID of the traffic policy instance that you want to get information about.
    id :: Types.TrafficPolicyInstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicyInstance' value with any optional fields omitted.
mkGetTrafficPolicyInstance ::
  -- | 'id'
  Types.TrafficPolicyInstanceId ->
  GetTrafficPolicyInstance
mkGetTrafficPolicyInstance id = GetTrafficPolicyInstance' {id}

-- | The ID of the traffic policy instance that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpiId :: Lens.Lens' GetTrafficPolicyInstance Types.TrafficPolicyInstanceId
gtpiId = Lens.field @"id"
{-# DEPRECATED gtpiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetTrafficPolicyInstance where
  type Rs GetTrafficPolicyInstance = GetTrafficPolicyInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2013-04-01/trafficpolicyinstance/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceResponse'
            Core.<$> (x Core..@ "TrafficPolicyInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'mkGetTrafficPolicyInstanceResponse' smart constructor.
data GetTrafficPolicyInstanceResponse = GetTrafficPolicyInstanceResponse'
  { -- | A complex type that contains settings for the traffic policy instance.
    trafficPolicyInstance :: Types.TrafficPolicyInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrafficPolicyInstanceResponse' value with any optional fields omitted.
mkGetTrafficPolicyInstanceResponse ::
  -- | 'trafficPolicyInstance'
  Types.TrafficPolicyInstance ->
  -- | 'responseStatus'
  Core.Int ->
  GetTrafficPolicyInstanceResponse
mkGetTrafficPolicyInstanceResponse
  trafficPolicyInstance
  responseStatus =
    GetTrafficPolicyInstanceResponse'
      { trafficPolicyInstance,
        responseStatus
      }

-- | A complex type that contains settings for the traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpirrsTrafficPolicyInstance :: Lens.Lens' GetTrafficPolicyInstanceResponse Types.TrafficPolicyInstance
gtpirrsTrafficPolicyInstance = Lens.field @"trafficPolicyInstance"
{-# DEPRECATED gtpirrsTrafficPolicyInstance "Use generic-lens or generic-optics with 'trafficPolicyInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpirrsResponseStatus :: Lens.Lens' GetTrafficPolicyInstanceResponse Core.Int
gtpirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtpirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
