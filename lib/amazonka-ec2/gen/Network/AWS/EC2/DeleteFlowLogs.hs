{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFlowLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more flow logs.
module Network.AWS.EC2.DeleteFlowLogs
  ( -- * Creating a request
    DeleteFlowLogs (..),
    mkDeleteFlowLogs,

    -- ** Request lenses
    dflFlowLogIds,
    dflDryRun,

    -- * Destructuring the response
    DeleteFlowLogsResponse (..),
    mkDeleteFlowLogsResponse,

    -- ** Response lenses
    dflrrsUnsuccessful,
    dflrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFlowLogs' smart constructor.
data DeleteFlowLogs = DeleteFlowLogs'
  { -- | One or more flow log IDs.
    --
    -- Constraint: Maximum of 1000 flow log IDs.
    flowLogIds :: [Types.VpcFlowLogId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowLogs' value with any optional fields omitted.
mkDeleteFlowLogs ::
  DeleteFlowLogs
mkDeleteFlowLogs =
  DeleteFlowLogs' {flowLogIds = Core.mempty, dryRun = Core.Nothing}

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflFlowLogIds :: Lens.Lens' DeleteFlowLogs [Types.VpcFlowLogId]
dflFlowLogIds = Lens.field @"flowLogIds"
{-# DEPRECATED dflFlowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflDryRun :: Lens.Lens' DeleteFlowLogs (Core.Maybe Core.Bool)
dflDryRun = Lens.field @"dryRun"
{-# DEPRECATED dflDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteFlowLogs where
  type Rs DeleteFlowLogs = DeleteFlowLogsResponse
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
            ( Core.pure ("Action", "DeleteFlowLogs")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "FlowLogId" flowLogIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteFlowLogsResponse'
            Core.<$> (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFlowLogsResponse' smart constructor.
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
  { -- | Information about the flow logs that could not be deleted successfully.
    unsuccessful :: Core.Maybe [Types.UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowLogsResponse' value with any optional fields omitted.
mkDeleteFlowLogsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFlowLogsResponse
mkDeleteFlowLogsResponse responseStatus =
  DeleteFlowLogsResponse'
    { unsuccessful = Core.Nothing,
      responseStatus
    }

-- | Information about the flow logs that could not be deleted successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrrsUnsuccessful :: Lens.Lens' DeleteFlowLogsResponse (Core.Maybe [Types.UnsuccessfulItem])
dflrrsUnsuccessful = Lens.field @"unsuccessful"
{-# DEPRECATED dflrrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrrsResponseStatus :: Lens.Lens' DeleteFlowLogsResponse Core.Int
dflrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dflrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
