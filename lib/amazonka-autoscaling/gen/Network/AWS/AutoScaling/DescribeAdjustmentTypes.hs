{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available adjustment types for Amazon EC2 Auto Scaling scaling policies. These settings apply to step scaling policies and simple scaling policies; they do not apply to target tracking scaling policies.
--
-- The following adjustment types are supported:
--
--     * ChangeInCapacity
--
--
--     * ExactCapacity
--
--
--     * PercentChangeInCapacity
module Network.AWS.AutoScaling.DescribeAdjustmentTypes
  ( -- * Creating a request
    DescribeAdjustmentTypes (..),
    mkDescribeAdjustmentTypes,

    -- * Destructuring the response
    DescribeAdjustmentTypesResponse (..),
    mkDescribeAdjustmentTypesResponse,

    -- ** Response lenses
    datrrsAdjustmentTypes,
    datrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAdjustmentTypes' smart constructor.
data DescribeAdjustmentTypes = DescribeAdjustmentTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAdjustmentTypes' value with any optional fields omitted.
mkDescribeAdjustmentTypes ::
  DescribeAdjustmentTypes
mkDescribeAdjustmentTypes = DescribeAdjustmentTypes'

instance Core.AWSRequest DescribeAdjustmentTypes where
  type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse
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
            ( Core.pure ("Action", "DescribeAdjustmentTypes")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAdjustmentTypesResult"
      ( \s h x ->
          DescribeAdjustmentTypesResponse'
            Core.<$> (x Core..@? "AdjustmentTypes" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAdjustmentTypesResponse' smart constructor.
data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'
  { -- | The policy adjustment types.
    adjustmentTypes :: Core.Maybe [Types.AdjustmentType],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAdjustmentTypesResponse' value with any optional fields omitted.
mkDescribeAdjustmentTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAdjustmentTypesResponse
mkDescribeAdjustmentTypesResponse responseStatus =
  DescribeAdjustmentTypesResponse'
    { adjustmentTypes = Core.Nothing,
      responseStatus
    }

-- | The policy adjustment types.
--
-- /Note:/ Consider using 'adjustmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsAdjustmentTypes :: Lens.Lens' DescribeAdjustmentTypesResponse (Core.Maybe [Types.AdjustmentType])
datrrsAdjustmentTypes = Lens.field @"adjustmentTypes"
{-# DEPRECATED datrrsAdjustmentTypes "Use generic-lens or generic-optics with 'adjustmentTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsResponseStatus :: Lens.Lens' DescribeAdjustmentTypesResponse Core.Int
datrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED datrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
