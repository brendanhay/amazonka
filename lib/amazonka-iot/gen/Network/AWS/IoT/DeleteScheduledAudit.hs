{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled audit.
module Network.AWS.IoT.DeleteScheduledAudit
    (
    -- * Creating a request
      DeleteScheduledAudit (..)
    , mkDeleteScheduledAudit
    -- ** Request lenses
    , dsaScheduledAuditName

    -- * Destructuring the response
    , DeleteScheduledAuditResponse (..)
    , mkDeleteScheduledAuditResponse
    -- ** Response lenses
    , dsarfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteScheduledAudit' smart constructor.
newtype DeleteScheduledAudit = DeleteScheduledAudit'
  { scheduledAuditName :: Types.ScheduledAuditName
    -- ^ The name of the scheduled audit you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledAudit' value with any optional fields omitted.
mkDeleteScheduledAudit
    :: Types.ScheduledAuditName -- ^ 'scheduledAuditName'
    -> DeleteScheduledAudit
mkDeleteScheduledAudit scheduledAuditName
  = DeleteScheduledAudit'{scheduledAuditName}

-- | The name of the scheduled audit you want to delete.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaScheduledAuditName :: Lens.Lens' DeleteScheduledAudit Types.ScheduledAuditName
dsaScheduledAuditName = Lens.field @"scheduledAuditName"
{-# INLINEABLE dsaScheduledAuditName #-}
{-# DEPRECATED scheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead"  #-}

instance Core.ToQuery DeleteScheduledAudit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteScheduledAudit where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteScheduledAudit where
        type Rs DeleteScheduledAudit = DeleteScheduledAuditResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/audit/scheduledaudits/" Core.<> Core.toText scheduledAuditName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteScheduledAuditResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteScheduledAuditResponse' smart constructor.
newtype DeleteScheduledAuditResponse = DeleteScheduledAuditResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledAuditResponse' value with any optional fields omitted.
mkDeleteScheduledAuditResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteScheduledAuditResponse
mkDeleteScheduledAuditResponse responseStatus
  = DeleteScheduledAuditResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarfrsResponseStatus :: Lens.Lens' DeleteScheduledAuditResponse Core.Int
dsarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
