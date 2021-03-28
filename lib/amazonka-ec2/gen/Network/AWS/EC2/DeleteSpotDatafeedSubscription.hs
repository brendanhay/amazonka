{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteSpotDatafeedSubscription (..)
    , mkDeleteSpotDatafeedSubscription
    -- ** Request lenses
    , dsdsfDryRun

    -- * Destructuring the response
    , DeleteSpotDatafeedSubscriptionResponse (..)
    , mkDeleteSpotDatafeedSubscriptionResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
-- /See:/ 'mkDeleteSpotDatafeedSubscription' smart constructor.
newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSpotDatafeedSubscription' value with any optional fields omitted.
mkDeleteSpotDatafeedSubscription
    :: DeleteSpotDatafeedSubscription
mkDeleteSpotDatafeedSubscription
  = DeleteSpotDatafeedSubscription'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdsfDryRun :: Lens.Lens' DeleteSpotDatafeedSubscription (Core.Maybe Core.Bool)
dsdsfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsdsfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteSpotDatafeedSubscription where
        toQuery DeleteSpotDatafeedSubscription{..}
          = Core.toQueryPair "Action"
              ("DeleteSpotDatafeedSubscription" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteSpotDatafeedSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSpotDatafeedSubscription where
        type Rs DeleteSpotDatafeedSubscription =
             DeleteSpotDatafeedSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteSpotDatafeedSubscriptionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSpotDatafeedSubscriptionResponse' value with any optional fields omitted.
mkDeleteSpotDatafeedSubscriptionResponse
    :: DeleteSpotDatafeedSubscriptionResponse
mkDeleteSpotDatafeedSubscriptionResponse
  = DeleteSpotDatafeedSubscriptionResponse'
