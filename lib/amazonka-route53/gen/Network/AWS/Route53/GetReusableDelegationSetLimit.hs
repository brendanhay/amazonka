{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSetLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- For the default limit, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case> .
module Network.AWS.Route53.GetReusableDelegationSetLimit
  ( -- * Creating a request
    GetReusableDelegationSetLimit (..),
    mkGetReusableDelegationSetLimit,

    -- ** Request lenses
    grdslType,
    grdslDelegationSetId,

    -- * Destructuring the response
    GetReusableDelegationSetLimitResponse (..),
    mkGetReusableDelegationSetLimitResponse,

    -- ** Response lenses
    grdslrrsLimit,
    grdslrrsCount,
    grdslrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about the request to create a hosted zone.
--
-- /See:/ 'mkGetReusableDelegationSetLimit' smart constructor.
data GetReusableDelegationSetLimit = GetReusableDelegationSetLimit'
  { -- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
    type' :: Types.ReusableDelegationSetLimitType,
    -- | The ID of the delegation set that you want to get the limit for.
    delegationSetId :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSetLimit' value with any optional fields omitted.
mkGetReusableDelegationSetLimit ::
  -- | 'type\''
  Types.ReusableDelegationSetLimitType ->
  -- | 'delegationSetId'
  Types.ResourceId ->
  GetReusableDelegationSetLimit
mkGetReusableDelegationSetLimit type' delegationSetId =
  GetReusableDelegationSetLimit' {type', delegationSetId}

-- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslType :: Lens.Lens' GetReusableDelegationSetLimit Types.ReusableDelegationSetLimitType
grdslType = Lens.field @"type'"
{-# DEPRECATED grdslType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The ID of the delegation set that you want to get the limit for.
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslDelegationSetId :: Lens.Lens' GetReusableDelegationSetLimit Types.ResourceId
grdslDelegationSetId = Lens.field @"delegationSetId"
{-# DEPRECATED grdslDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

instance Core.AWSRequest GetReusableDelegationSetLimit where
  type
    Rs GetReusableDelegationSetLimit =
      GetReusableDelegationSetLimitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/reusabledelegationsetlimit/"
                Core.<> (Core.toText delegationSetId)
                Core.<> ("/")
                Core.<> (Core.toText type')
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetReusableDelegationSetLimitResponse'
            Core.<$> (x Core..@ "Limit")
            Core.<*> (x Core..@ "Count")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the requested limit.
--
-- /See:/ 'mkGetReusableDelegationSetLimitResponse' smart constructor.
data GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse'
  { -- | The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
    limit :: Types.ReusableDelegationSetLimit,
    -- | The current number of hosted zones that you can associate with the specified reusable delegation set.
    count :: Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReusableDelegationSetLimitResponse' value with any optional fields omitted.
mkGetReusableDelegationSetLimitResponse ::
  -- | 'limit'
  Types.ReusableDelegationSetLimit ->
  -- | 'count'
  Core.Natural ->
  -- | 'responseStatus'
  Core.Int ->
  GetReusableDelegationSetLimitResponse
mkGetReusableDelegationSetLimitResponse limit count responseStatus =
  GetReusableDelegationSetLimitResponse'
    { limit,
      count,
      responseStatus
    }

-- | The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsLimit :: Lens.Lens' GetReusableDelegationSetLimitResponse Types.ReusableDelegationSetLimit
grdslrrsLimit = Lens.field @"limit"
{-# DEPRECATED grdslrrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsCount :: Lens.Lens' GetReusableDelegationSetLimitResponse Core.Natural
grdslrrsCount = Lens.field @"count"
{-# DEPRECATED grdslrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdslrrsResponseStatus :: Lens.Lens' GetReusableDelegationSetLimitResponse Core.Int
grdslrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdslrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
