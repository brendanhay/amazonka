{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about the AWS Shield Advanced subscription for an account.
module Network.AWS.Shield.DescribeSubscription
  ( -- * Creating a request
    DescribeSubscription (..),
    mkDescribeSubscription,

    -- * Destructuring the response
    DescribeSubscriptionResponse (..),
    mkDescribeSubscriptionResponse,

    -- ** Response lenses
    dsrrsSubscription,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeSubscription' smart constructor.
data DescribeSubscription = DescribeSubscription'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscription' value with any optional fields omitted.
mkDescribeSubscription ::
  DescribeSubscription
mkDescribeSubscription = DescribeSubscription'

instance Core.FromJSON DescribeSubscription where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeSubscription where
  type Rs DescribeSubscription = DescribeSubscriptionResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.DescribeSubscription")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscriptionResponse'
            Core.<$> (x Core..:? "Subscription") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeSubscriptionResponse' smart constructor.
data DescribeSubscriptionResponse = DescribeSubscriptionResponse'
  { -- | The AWS Shield Advanced subscription details for an account.
    subscription :: Core.Maybe Types.Subscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSubscriptionResponse' value with any optional fields omitted.
mkDescribeSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSubscriptionResponse
mkDescribeSubscriptionResponse responseStatus =
  DescribeSubscriptionResponse'
    { subscription = Core.Nothing,
      responseStatus
    }

-- | The AWS Shield Advanced subscription details for an account.
--
-- /Note:/ Consider using 'subscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSubscription :: Lens.Lens' DescribeSubscriptionResponse (Core.Maybe Types.Subscription)
dsrrsSubscription = Lens.field @"subscription"
{-# DEPRECATED dsrrsSubscription "Use generic-lens or generic-optics with 'subscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSubscriptionResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
