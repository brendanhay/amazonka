{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified conversion tasks or all your conversion tasks. For more information, see the <https://docs.aws.amazon.com/vm-import/latest/userguide/ VM Import/Export User Guide> .
--
-- For information about the import manifest referenced by this API action, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
module Network.AWS.EC2.DescribeConversionTasks
  ( -- * Creating a request
    DescribeConversionTasks (..),
    mkDescribeConversionTasks,

    -- ** Request lenses
    dctConversionTaskIds,
    dctDryRun,

    -- * Destructuring the response
    DescribeConversionTasksResponse (..),
    mkDescribeConversionTasksResponse,

    -- ** Response lenses
    dctrrsConversionTasks,
    dctrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConversionTasks' smart constructor.
data DescribeConversionTasks = DescribeConversionTasks'
  { -- | The conversion task IDs.
    conversionTaskIds :: Core.Maybe [Types.ConversionTaskId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConversionTasks' value with any optional fields omitted.
mkDescribeConversionTasks ::
  DescribeConversionTasks
mkDescribeConversionTasks =
  DescribeConversionTasks'
    { conversionTaskIds = Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The conversion task IDs.
--
-- /Note:/ Consider using 'conversionTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctConversionTaskIds :: Lens.Lens' DescribeConversionTasks (Core.Maybe [Types.ConversionTaskId])
dctConversionTaskIds = Lens.field @"conversionTaskIds"
{-# DEPRECATED dctConversionTaskIds "Use generic-lens or generic-optics with 'conversionTaskIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctDryRun :: Lens.Lens' DescribeConversionTasks (Core.Maybe Core.Bool)
dctDryRun = Lens.field @"dryRun"
{-# DEPRECATED dctDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeConversionTasks where
  type Rs DescribeConversionTasks = DescribeConversionTasksResponse
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
            ( Core.pure ("Action", "DescribeConversionTasks")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ConversionTaskId" Core.<$> conversionTaskIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeConversionTasksResponse'
            Core.<$> (x Core..@? "conversionTasks" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeConversionTasksResponse' smart constructor.
data DescribeConversionTasksResponse = DescribeConversionTasksResponse'
  { -- | Information about the conversion tasks.
    conversionTasks :: Core.Maybe [Types.ConversionTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConversionTasksResponse' value with any optional fields omitted.
mkDescribeConversionTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConversionTasksResponse
mkDescribeConversionTasksResponse responseStatus =
  DescribeConversionTasksResponse'
    { conversionTasks = Core.Nothing,
      responseStatus
    }

-- | Information about the conversion tasks.
--
-- /Note:/ Consider using 'conversionTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrrsConversionTasks :: Lens.Lens' DescribeConversionTasksResponse (Core.Maybe [Types.ConversionTask])
dctrrsConversionTasks = Lens.field @"conversionTasks"
{-# DEPRECATED dctrrsConversionTasks "Use generic-lens or generic-optics with 'conversionTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrrsResponseStatus :: Lens.Lens' DescribeConversionTasksResponse Core.Int
dctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
