{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with the 'ResumeProcesses' and 'SuspendProcesses' APIs.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
  ( -- * Creating a request
    DescribeScalingProcessTypes (..),
    mkDescribeScalingProcessTypes,

    -- * Destructuring the response
    DescribeScalingProcessTypesResponse (..),
    mkDescribeScalingProcessTypesResponse,

    -- ** Response lenses
    dsptrrsProcesses,
    dsptrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes = DescribeScalingProcessTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingProcessTypes' value with any optional fields omitted.
mkDescribeScalingProcessTypes ::
  DescribeScalingProcessTypes
mkDescribeScalingProcessTypes = DescribeScalingProcessTypes'

instance Core.AWSRequest DescribeScalingProcessTypes where
  type
    Rs DescribeScalingProcessTypes =
      DescribeScalingProcessTypesResponse
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
            ( Core.pure ("Action", "DescribeScalingProcessTypes")
                Core.<> (Core.pure ("Version", "2011-01-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeScalingProcessTypesResult"
      ( \s h x ->
          DescribeScalingProcessTypesResponse'
            Core.<$> (x Core..@? "Processes" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeScalingProcessTypesResponse' smart constructor.
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
  { -- | The names of the process types.
    processes :: Core.Maybe [Types.ProcessType],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingProcessTypesResponse' value with any optional fields omitted.
mkDescribeScalingProcessTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScalingProcessTypesResponse
mkDescribeScalingProcessTypesResponse responseStatus =
  DescribeScalingProcessTypesResponse'
    { processes = Core.Nothing,
      responseStatus
    }

-- | The names of the process types.
--
-- /Note:/ Consider using 'processes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrrsProcesses :: Lens.Lens' DescribeScalingProcessTypesResponse (Core.Maybe [Types.ProcessType])
dsptrrsProcesses = Lens.field @"processes"
{-# DEPRECATED dsptrrsProcesses "Use generic-lens or generic-optics with 'processes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrrsResponseStatus :: Lens.Lens' DescribeScalingProcessTypesResponse Core.Int
dsptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
