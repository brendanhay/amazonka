{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the health of the specified targets or all of your targets.
module Network.AWS.ELBv2.DescribeTargetHealth
  ( -- * Creating a request
    DescribeTargetHealth (..),
    mkDescribeTargetHealth,

    -- ** Request lenses
    dthTargetGroupArn,
    dthTargets,

    -- * Destructuring the response
    DescribeTargetHealthResponse (..),
    mkDescribeTargetHealthResponse,

    -- ** Response lenses
    dthrrsTargetHealthDescriptions,
    dthrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTargetHealth' smart constructor.
data DescribeTargetHealth = DescribeTargetHealth'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Types.TargetGroupArn,
    -- | The targets.
    targets :: Core.Maybe [Types.TargetDescription]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetHealth' value with any optional fields omitted.
mkDescribeTargetHealth ::
  -- | 'targetGroupArn'
  Types.TargetGroupArn ->
  DescribeTargetHealth
mkDescribeTargetHealth targetGroupArn =
  DescribeTargetHealth' {targetGroupArn, targets = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthTargetGroupArn :: Lens.Lens' DescribeTargetHealth Types.TargetGroupArn
dthTargetGroupArn = Lens.field @"targetGroupArn"
{-# DEPRECATED dthTargetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead." #-}

-- | The targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthTargets :: Lens.Lens' DescribeTargetHealth (Core.Maybe [Types.TargetDescription])
dthTargets = Lens.field @"targets"
{-# DEPRECATED dthTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.AWSRequest DescribeTargetHealth where
  type Rs DescribeTargetHealth = DescribeTargetHealthResponse
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
            ( Core.pure ("Action", "DescribeTargetHealth")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "TargetGroupArn" targetGroupArn)
                Core.<> ( Core.toQueryValue
                            "Targets"
                            (Core.toQueryList "member" Core.<$> targets)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeTargetHealthResult"
      ( \s h x ->
          DescribeTargetHealthResponse'
            Core.<$> ( x Core..@? "TargetHealthDescriptions"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTargetHealthResponse' smart constructor.
data DescribeTargetHealthResponse = DescribeTargetHealthResponse'
  { -- | Information about the health of the targets.
    targetHealthDescriptions :: Core.Maybe [Types.TargetHealthDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetHealthResponse' value with any optional fields omitted.
mkDescribeTargetHealthResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTargetHealthResponse
mkDescribeTargetHealthResponse responseStatus =
  DescribeTargetHealthResponse'
    { targetHealthDescriptions =
        Core.Nothing,
      responseStatus
    }

-- | Information about the health of the targets.
--
-- /Note:/ Consider using 'targetHealthDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthrrsTargetHealthDescriptions :: Lens.Lens' DescribeTargetHealthResponse (Core.Maybe [Types.TargetHealthDescription])
dthrrsTargetHealthDescriptions = Lens.field @"targetHealthDescriptions"
{-# DEPRECATED dthrrsTargetHealthDescriptions "Use generic-lens or generic-optics with 'targetHealthDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthrrsResponseStatus :: Lens.Lens' DescribeTargetHealthResponse Core.Int
dthrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dthrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
