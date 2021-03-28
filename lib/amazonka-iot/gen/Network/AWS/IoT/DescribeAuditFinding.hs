{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single audit finding. Properties include the reason for noncompliance, the severity of the issue, and when the audit that returned the finding was started.
module Network.AWS.IoT.DescribeAuditFinding
    (
    -- * Creating a request
      DescribeAuditFinding (..)
    , mkDescribeAuditFinding
    -- ** Request lenses
    , dafFindingId

    -- * Destructuring the response
    , DescribeAuditFindingResponse (..)
    , mkDescribeAuditFindingResponse
    -- ** Response lenses
    , dafrrsFinding
    , dafrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAuditFinding' smart constructor.
newtype DescribeAuditFinding = DescribeAuditFinding'
  { findingId :: Types.FindingId
    -- ^ A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAuditFinding' value with any optional fields omitted.
mkDescribeAuditFinding
    :: Types.FindingId -- ^ 'findingId'
    -> DescribeAuditFinding
mkDescribeAuditFinding findingId = DescribeAuditFinding'{findingId}

-- | A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFindingId :: Lens.Lens' DescribeAuditFinding Types.FindingId
dafFindingId = Lens.field @"findingId"
{-# INLINEABLE dafFindingId #-}
{-# DEPRECATED findingId "Use generic-lens or generic-optics with 'findingId' instead"  #-}

instance Core.ToQuery DescribeAuditFinding where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAuditFinding where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAuditFinding where
        type Rs DescribeAuditFinding = DescribeAuditFindingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/audit/findings/" Core.<> Core.toText findingId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAuditFindingResponse' Core.<$>
                   (x Core..:? "finding") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAuditFindingResponse' smart constructor.
data DescribeAuditFindingResponse = DescribeAuditFindingResponse'
  { finding :: Core.Maybe Types.AuditFinding
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAuditFindingResponse' value with any optional fields omitted.
mkDescribeAuditFindingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAuditFindingResponse
mkDescribeAuditFindingResponse responseStatus
  = DescribeAuditFindingResponse'{finding = Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrrsFinding :: Lens.Lens' DescribeAuditFindingResponse (Core.Maybe Types.AuditFinding)
dafrrsFinding = Lens.field @"finding"
{-# INLINEABLE dafrrsFinding #-}
{-# DEPRECATED finding "Use generic-lens or generic-optics with 'finding' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrrsResponseStatus :: Lens.Lens' DescribeAuditFindingResponse Core.Int
dafrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dafrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
