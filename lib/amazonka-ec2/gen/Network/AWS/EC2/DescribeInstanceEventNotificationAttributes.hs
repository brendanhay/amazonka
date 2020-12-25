{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tag keys that are registered to appear in scheduled event notifications for resources in the current Region.
module Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
  ( -- * Creating a request
    DescribeInstanceEventNotificationAttributes (..),
    mkDescribeInstanceEventNotificationAttributes,

    -- ** Request lenses
    dienasDryRun,

    -- * Destructuring the response
    DescribeInstanceEventNotificationAttributesResponse (..),
    mkDescribeInstanceEventNotificationAttributesResponse,

    -- ** Response lenses
    dienarfrsInstanceTagAttribute,
    dienarfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceEventNotificationAttributes' smart constructor.
newtype DescribeInstanceEventNotificationAttributes = DescribeInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceEventNotificationAttributes' value with any optional fields omitted.
mkDescribeInstanceEventNotificationAttributes ::
  DescribeInstanceEventNotificationAttributes
mkDescribeInstanceEventNotificationAttributes =
  DescribeInstanceEventNotificationAttributes'
    { dryRun =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasDryRun :: Lens.Lens' DescribeInstanceEventNotificationAttributes (Core.Maybe Core.Bool)
dienasDryRun = Lens.field @"dryRun"
{-# DEPRECATED dienasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Core.AWSRequest
    DescribeInstanceEventNotificationAttributes
  where
  type
    Rs DescribeInstanceEventNotificationAttributes =
      DescribeInstanceEventNotificationAttributesResponse
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
            ( Core.pure
                ("Action", "DescribeInstanceEventNotificationAttributes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceEventNotificationAttributesResponse'
            Core.<$> (x Core..@? "instanceTagAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeInstanceEventNotificationAttributesResponse' smart constructor.
data DescribeInstanceEventNotificationAttributesResponse = DescribeInstanceEventNotificationAttributesResponse'
  { -- | Information about the registered tag keys.
    instanceTagAttribute :: Core.Maybe Types.InstanceTagNotificationAttribute,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceEventNotificationAttributesResponse' value with any optional fields omitted.
mkDescribeInstanceEventNotificationAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceEventNotificationAttributesResponse
mkDescribeInstanceEventNotificationAttributesResponse
  responseStatus =
    DescribeInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Core.Nothing,
        responseStatus
      }

-- | Information about the registered tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarfrsInstanceTagAttribute :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse (Core.Maybe Types.InstanceTagNotificationAttribute)
dienarfrsInstanceTagAttribute = Lens.field @"instanceTagAttribute"
{-# DEPRECATED dienarfrsInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarfrsResponseStatus :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse Core.Int
dienarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dienarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
