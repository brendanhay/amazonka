{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteFlowLogs (..)
    , mkDeleteFlowLogs
    -- ** Request lenses
    , dflFlowLogIds
    , dflDryRun

    -- * Destructuring the response
    , DeleteFlowLogsResponse (..)
    , mkDeleteFlowLogsResponse
    -- ** Response lenses
    , dflrrsUnsuccessful
    , dflrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFlowLogs' smart constructor.
data DeleteFlowLogs = DeleteFlowLogs'
  { flowLogIds :: [Types.VpcFlowLogId]
    -- ^ One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowLogs' value with any optional fields omitted.
mkDeleteFlowLogs
    :: DeleteFlowLogs
mkDeleteFlowLogs
  = DeleteFlowLogs'{flowLogIds = Core.mempty, dryRun = Core.Nothing}

-- | One or more flow log IDs.
--
-- Constraint: Maximum of 1000 flow log IDs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflFlowLogIds :: Lens.Lens' DeleteFlowLogs [Types.VpcFlowLogId]
dflFlowLogIds = Lens.field @"flowLogIds"
{-# INLINEABLE dflFlowLogIds #-}
{-# DEPRECATED flowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflDryRun :: Lens.Lens' DeleteFlowLogs (Core.Maybe Core.Bool)
dflDryRun = Lens.field @"dryRun"
{-# INLINEABLE dflDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteFlowLogs where
        toQuery DeleteFlowLogs{..}
          = Core.toQueryPair "Action" ("DeleteFlowLogs" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "FlowLogId" flowLogIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteFlowLogs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteFlowLogs where
        type Rs DeleteFlowLogs = DeleteFlowLogsResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DeleteFlowLogsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFlowLogsResponse' smart constructor.
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the flow logs that could not be deleted successfully.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowLogsResponse' value with any optional fields omitted.
mkDeleteFlowLogsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteFlowLogsResponse
mkDeleteFlowLogsResponse responseStatus
  = DeleteFlowLogsResponse'{unsuccessful = Core.Nothing,
                            responseStatus}

-- | Information about the flow logs that could not be deleted successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrrsUnsuccessful :: Lens.Lens' DeleteFlowLogsResponse (Core.Maybe [Types.UnsuccessfulItem])
dflrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE dflrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflrrsResponseStatus :: Lens.Lens' DeleteFlowLogsResponse Core.Int
dflrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dflrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
