{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data feed for Spot Instances.
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
  ( -- * Creating a request
    DeleteSpotDatafeedSubscription (..),
    mkDeleteSpotDatafeedSubscription,

    -- ** Request lenses
    dsdsfDryRun,

    -- * Destructuring the response
    DeleteSpotDatafeedSubscriptionResponse (..),
    mkDeleteSpotDatafeedSubscriptionResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
-- /See:/ 'mkDeleteSpotDatafeedSubscription' smart constructor.
newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSpotDatafeedSubscription' value with any optional fields omitted.
mkDeleteSpotDatafeedSubscription ::
  DeleteSpotDatafeedSubscription
mkDeleteSpotDatafeedSubscription =
  DeleteSpotDatafeedSubscription' {dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsfDryRun :: Lens.Lens' DeleteSpotDatafeedSubscription (Core.Maybe Core.Bool)
dsdsfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsdsfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteSpotDatafeedSubscription where
  type
    Rs DeleteSpotDatafeedSubscription =
      DeleteSpotDatafeedSubscriptionResponse
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
            ( Core.pure ("Action", "DeleteSpotDatafeedSubscription")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveNull DeleteSpotDatafeedSubscriptionResponse'

-- | /See:/ 'mkDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSpotDatafeedSubscriptionResponse' value with any optional fields omitted.
mkDeleteSpotDatafeedSubscriptionResponse ::
  DeleteSpotDatafeedSubscriptionResponse
mkDeleteSpotDatafeedSubscriptionResponse =
  DeleteSpotDatafeedSubscriptionResponse'
