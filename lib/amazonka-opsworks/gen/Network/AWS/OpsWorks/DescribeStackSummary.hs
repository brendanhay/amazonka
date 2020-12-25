{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the number of layers and apps in a specified stack, and the number of instances in each state, such as @running_setup@ or @online@ .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStackSummary
  ( -- * Creating a request
    DescribeStackSummary (..),
    mkDescribeStackSummary,

    -- ** Request lenses
    dssStackId,

    -- * Destructuring the response
    DescribeStackSummaryResponse (..),
    mkDescribeStackSummaryResponse,

    -- ** Response lenses
    dssrrsStackSummary,
    dssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackSummary' smart constructor.
newtype DescribeStackSummary = DescribeStackSummary'
  { -- | The stack ID.
    stackId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackSummary' value with any optional fields omitted.
mkDescribeStackSummary ::
  -- | 'stackId'
  Types.String ->
  DescribeStackSummary
mkDescribeStackSummary stackId = DescribeStackSummary' {stackId}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssStackId :: Lens.Lens' DescribeStackSummary Types.String
dssStackId = Lens.field @"stackId"
{-# DEPRECATED dssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeStackSummary where
  toJSON DescribeStackSummary {..} =
    Core.object
      (Core.catMaybes [Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest DescribeStackSummary where
  type Rs DescribeStackSummary = DescribeStackSummaryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeStackSummary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStackSummaryResponse'
            Core.<$> (x Core..:? "StackSummary") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeStackSummary@ request.
--
-- /See:/ 'mkDescribeStackSummaryResponse' smart constructor.
data DescribeStackSummaryResponse = DescribeStackSummaryResponse'
  { -- | A @StackSummary@ object that contains the results.
    stackSummary :: Core.Maybe Types.StackSummary,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackSummaryResponse' value with any optional fields omitted.
mkDescribeStackSummaryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackSummaryResponse
mkDescribeStackSummaryResponse responseStatus =
  DescribeStackSummaryResponse'
    { stackSummary = Core.Nothing,
      responseStatus
    }

-- | A @StackSummary@ object that contains the results.
--
-- /Note:/ Consider using 'stackSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsStackSummary :: Lens.Lens' DescribeStackSummaryResponse (Core.Maybe Types.StackSummary)
dssrrsStackSummary = Lens.field @"stackSummary"
{-# DEPRECATED dssrrsStackSummary "Use generic-lens or generic-optics with 'stackSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsResponseStatus :: Lens.Lens' DescribeStackSummaryResponse Core.Int
dssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
