{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more stacks.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStacks
  ( -- * Creating a request
    DescribeStacks (..),
    mkDescribeStacks,

    -- ** Request lenses
    dsStackIds,

    -- * Destructuring the response
    DescribeStacksResponse (..),
    mkDescribeStacksResponse,

    -- ** Response lenses
    dsrrsStacks,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStacks' smart constructor.
newtype DescribeStacks = DescribeStacks'
  { -- | An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
    stackIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStacks' value with any optional fields omitted.
mkDescribeStacks ::
  DescribeStacks
mkDescribeStacks = DescribeStacks' {stackIds = Core.Nothing}

-- | An array of stack IDs that specify the stacks to be described. If you omit this parameter, @DescribeStacks@ returns a description of every stack.
--
-- /Note:/ Consider using 'stackIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackIds :: Lens.Lens' DescribeStacks (Core.Maybe [Types.String])
dsStackIds = Lens.field @"stackIds"
{-# DEPRECATED dsStackIds "Use generic-lens or generic-optics with 'stackIds' instead." #-}

instance Core.FromJSON DescribeStacks where
  toJSON DescribeStacks {..} =
    Core.object
      (Core.catMaybes [("StackIds" Core..=) Core.<$> stackIds])

instance Core.AWSRequest DescribeStacks where
  type Rs DescribeStacks = DescribeStacksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeStacks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Core.<$> (x Core..:? "Stacks") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeStacks@ request.
--
-- /See:/ 'mkDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | An array of @Stack@ objects that describe the stacks.
    stacks :: Core.Maybe [Types.Stack],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStacksResponse' value with any optional fields omitted.
mkDescribeStacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStacksResponse
mkDescribeStacksResponse responseStatus =
  DescribeStacksResponse' {stacks = Core.Nothing, responseStatus}

-- | An array of @Stack@ objects that describe the stacks.
--
-- /Note:/ Consider using 'stacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStacks :: Lens.Lens' DescribeStacksResponse (Core.Maybe [Types.Stack])
dsrrsStacks = Lens.field @"stacks"
{-# DEPRECATED dsrrsStacks "Use generic-lens or generic-optics with 'stacks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStacksResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
