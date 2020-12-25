{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Amazon EC2 Auto Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
  ( -- * Creating a request
    DescribeAutoScalingNotificationTypes (..),
    mkDescribeAutoScalingNotificationTypes,

    -- * Destructuring the response
    DescribeAutoScalingNotificationTypesResponse (..),
    mkDescribeAutoScalingNotificationTypesResponse,

    -- ** Response lenses
    dasntrrsAutoScalingNotificationTypes,
    dasntrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutoScalingNotificationTypes' value with any optional fields omitted.
mkDescribeAutoScalingNotificationTypes ::
  DescribeAutoScalingNotificationTypes
mkDescribeAutoScalingNotificationTypes =
  DescribeAutoScalingNotificationTypes'

instance Core.AWSRequest DescribeAutoScalingNotificationTypes where
  type
    Rs DescribeAutoScalingNotificationTypes =
      DescribeAutoScalingNotificationTypesResponse
  request x@_ =
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
            ( Core.pure ("Action", "DescribeAutoScalingNotificationTypes")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingNotificationTypesResult"
      ( \s h x ->
          DescribeAutoScalingNotificationTypesResponse'
            Core.<$> ( x Core..@? "AutoScalingNotificationTypes"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAutoScalingNotificationTypesResponse' smart constructor.
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
  { -- | The notification types.
    autoScalingNotificationTypes :: Core.Maybe [Types.XmlStringMaxLen255],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutoScalingNotificationTypesResponse' value with any optional fields omitted.
mkDescribeAutoScalingNotificationTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAutoScalingNotificationTypesResponse
mkDescribeAutoScalingNotificationTypesResponse responseStatus =
  DescribeAutoScalingNotificationTypesResponse'
    { autoScalingNotificationTypes =
        Core.Nothing,
      responseStatus
    }

-- | The notification types.
--
-- /Note:/ Consider using 'autoScalingNotificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasntrrsAutoScalingNotificationTypes :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse (Core.Maybe [Types.XmlStringMaxLen255])
dasntrrsAutoScalingNotificationTypes = Lens.field @"autoScalingNotificationTypes"
{-# DEPRECATED dasntrrsAutoScalingNotificationTypes "Use generic-lens or generic-optics with 'autoScalingNotificationTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasntrrsResponseStatus :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse Core.Int
dasntrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasntrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
