{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the endpoints and endpoint attributes for devices in a supported push notification service, such as GCM (Firebase Cloud Messaging) and APNS. The results for @ListEndpointsByPlatformApplication@ are paginated and return a limited list of endpoints, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call @ListEndpointsByPlatformApplication@ again using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListEndpointsByPlatformApplication
  ( -- * Creating a request
    ListEndpointsByPlatformApplication (..),
    mkListEndpointsByPlatformApplication,

    -- ** Request lenses
    lebpaPlatformApplicationArn,
    lebpaNextToken,

    -- * Destructuring the response
    ListEndpointsByPlatformApplicationResponse (..),
    mkListEndpointsByPlatformApplicationResponse,

    -- ** Response lenses
    lebparrsEndpoints,
    lebparrsNextToken,
    lebparrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'mkListEndpointsByPlatformApplication' smart constructor.
data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication'
  { -- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
    platformApplicationArn :: Types.PlatformApplicationArn,
    -- | NextToken string is used when calling ListEndpointsByPlatformApplication action to retrieve additional records that are available after the first page results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEndpointsByPlatformApplication' value with any optional fields omitted.
mkListEndpointsByPlatformApplication ::
  -- | 'platformApplicationArn'
  Types.PlatformApplicationArn ->
  ListEndpointsByPlatformApplication
mkListEndpointsByPlatformApplication platformApplicationArn =
  ListEndpointsByPlatformApplication'
    { platformApplicationArn,
      nextToken = Core.Nothing
    }

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebpaPlatformApplicationArn :: Lens.Lens' ListEndpointsByPlatformApplication Types.PlatformApplicationArn
lebpaPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# DEPRECATED lebpaPlatformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead." #-}

-- | NextToken string is used when calling ListEndpointsByPlatformApplication action to retrieve additional records that are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebpaNextToken :: Lens.Lens' ListEndpointsByPlatformApplication (Core.Maybe Types.NextToken)
lebpaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lebpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListEndpointsByPlatformApplication where
  type
    Rs ListEndpointsByPlatformApplication =
      ListEndpointsByPlatformApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListEndpointsByPlatformApplication")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "PlatformApplicationArn" platformApplicationArn)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListEndpointsByPlatformApplicationResult"
      ( \s h x ->
          ListEndpointsByPlatformApplicationResponse'
            Core.<$> (x Core..@? "Endpoints" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListEndpointsByPlatformApplication where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"endpoints" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Response for ListEndpointsByPlatformApplication action.
--
-- /See:/ 'mkListEndpointsByPlatformApplicationResponse' smart constructor.
data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse'
  { -- | Endpoints returned for ListEndpointsByPlatformApplication action.
    endpoints :: Core.Maybe [Types.Endpoint],
    -- | NextToken string is returned when calling ListEndpointsByPlatformApplication action if additional records are available after the first page results.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEndpointsByPlatformApplicationResponse' value with any optional fields omitted.
mkListEndpointsByPlatformApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEndpointsByPlatformApplicationResponse
mkListEndpointsByPlatformApplicationResponse responseStatus =
  ListEndpointsByPlatformApplicationResponse'
    { endpoints =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparrsEndpoints :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Core.Maybe [Types.Endpoint])
lebparrsEndpoints = Lens.field @"endpoints"
{-# DEPRECATED lebparrsEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | NextToken string is returned when calling ListEndpointsByPlatformApplication action if additional records are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparrsNextToken :: Lens.Lens' ListEndpointsByPlatformApplicationResponse (Core.Maybe Types.String)
lebparrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lebparrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lebparrsResponseStatus :: Lens.Lens' ListEndpointsByPlatformApplicationResponse Core.Int
lebparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lebparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
