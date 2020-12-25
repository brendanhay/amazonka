{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
  ( -- * Creating a request
    DescribeSpotDatafeedSubscription (..),
    mkDescribeSpotDatafeedSubscription,

    -- ** Request lenses
    dsdsDryRun,

    -- * Destructuring the response
    DescribeSpotDatafeedSubscriptionResponse (..),
    mkDescribeSpotDatafeedSubscriptionResponse,

    -- ** Response lenses
    dsdsrrsSpotDatafeedSubscription,
    dsdsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotDatafeedSubscription.
--
-- /See:/ 'mkDescribeSpotDatafeedSubscription' smart constructor.
newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotDatafeedSubscription' value with any optional fields omitted.
mkDescribeSpotDatafeedSubscription ::
  DescribeSpotDatafeedSubscription
mkDescribeSpotDatafeedSubscription =
  DescribeSpotDatafeedSubscription' {dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsDryRun :: Lens.Lens' DescribeSpotDatafeedSubscription (Core.Maybe Core.Bool)
dsdsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeSpotDatafeedSubscription where
  type
    Rs DescribeSpotDatafeedSubscription =
      DescribeSpotDatafeedSubscriptionResponse
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
            ( Core.pure ("Action", "DescribeSpotDatafeedSubscription")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotDatafeedSubscriptionResponse'
            Core.<$> (x Core..@? "spotDatafeedSubscription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DescribeSpotDatafeedSubscription.
--
-- /See:/ 'mkDescribeSpotDatafeedSubscriptionResponse' smart constructor.
data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'
  { -- | The Spot Instance data feed subscription.
    spotDatafeedSubscription :: Core.Maybe Types.SpotDatafeedSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotDatafeedSubscriptionResponse' value with any optional fields omitted.
mkDescribeSpotDatafeedSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSpotDatafeedSubscriptionResponse
mkDescribeSpotDatafeedSubscriptionResponse responseStatus =
  DescribeSpotDatafeedSubscriptionResponse'
    { spotDatafeedSubscription =
        Core.Nothing,
      responseStatus
    }

-- | The Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'spotDatafeedSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsrrsSpotDatafeedSubscription :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse (Core.Maybe Types.SpotDatafeedSubscription)
dsdsrrsSpotDatafeedSubscription = Lens.field @"spotDatafeedSubscription"
{-# DEPRECATED dsdsrrsSpotDatafeedSubscription "Use generic-lens or generic-optics with 'spotDatafeedSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsrrsResponseStatus :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse Core.Int
dsdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
