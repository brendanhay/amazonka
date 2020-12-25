{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeSubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a work team provided by a vendor. It returns details about the subscription with a vendor in the AWS Marketplace.
module Network.AWS.SageMaker.DescribeSubscribedWorkteam
  ( -- * Creating a request
    DescribeSubscribedWorkteam (..),
    mkDescribeSubscribedWorkteam,

    -- ** Request lenses
    dswWorkteamArn,

    -- * Destructuring the response
    DescribeSubscribedWorkteamResponse (..),
    mkDescribeSubscribedWorkteamResponse,

    -- ** Response lenses
    dswrrsSubscribedWorkteam,
    dswrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeSubscribedWorkteam' smart constructor.
newtype DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { -- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
    workteamArn :: Types.WorkteamArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribedWorkteam' value with any optional fields omitted.
mkDescribeSubscribedWorkteam ::
  -- | 'workteamArn'
  Types.WorkteamArn ->
  DescribeSubscribedWorkteam
mkDescribeSubscribedWorkteam workteamArn =
  DescribeSubscribedWorkteam' {workteamArn}

-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswWorkteamArn :: Lens.Lens' DescribeSubscribedWorkteam Types.WorkteamArn
dswWorkteamArn = Lens.field @"workteamArn"
{-# DEPRECATED dswWorkteamArn "Use generic-lens or generic-optics with 'workteamArn' instead." #-}

instance Core.FromJSON DescribeSubscribedWorkteam where
  toJSON DescribeSubscribedWorkteam {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkteamArn" Core..= workteamArn)])

instance Core.AWSRequest DescribeSubscribedWorkteam where
  type
    Rs DescribeSubscribedWorkteam =
      DescribeSubscribedWorkteamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeSubscribedWorkteam")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribedWorkteamResponse'
            Core.<$> (x Core..: "SubscribedWorkteam")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { -- | A @Workteam@ instance that contains information about the work team.
    subscribedWorkteam :: Types.SubscribedWorkteam,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribedWorkteamResponse' value with any optional fields omitted.
mkDescribeSubscribedWorkteamResponse ::
  -- | 'subscribedWorkteam'
  Types.SubscribedWorkteam ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeSubscribedWorkteamResponse
mkDescribeSubscribedWorkteamResponse
  subscribedWorkteam
  responseStatus =
    DescribeSubscribedWorkteamResponse'
      { subscribedWorkteam,
        responseStatus
      }

-- | A @Workteam@ instance that contains information about the work team.
--
-- /Note:/ Consider using 'subscribedWorkteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrrsSubscribedWorkteam :: Lens.Lens' DescribeSubscribedWorkteamResponse Types.SubscribedWorkteam
dswrrsSubscribedWorkteam = Lens.field @"subscribedWorkteam"
{-# DEPRECATED dswrrsSubscribedWorkteam "Use generic-lens or generic-optics with 'subscribedWorkteam' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswrrsResponseStatus :: Lens.Lens' DescribeSubscribedWorkteamResponse Core.Int
dswrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dswrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
