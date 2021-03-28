{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CancelReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a single premigration assessment run.
--
-- This operation prevents any individual assessments from running if they haven't started running. It also attempts to cancel any individual assessments that are currently running.
module Network.AWS.DMS.CancelReplicationTaskAssessmentRun
    (
    -- * Creating a request
      CancelReplicationTaskAssessmentRun (..)
    , mkCancelReplicationTaskAssessmentRun
    -- ** Request lenses
    , crtarReplicationTaskAssessmentRunArn

    -- * Destructuring the response
    , CancelReplicationTaskAssessmentRunResponse (..)
    , mkCancelReplicationTaskAssessmentRunResponse
    -- ** Response lenses
    , crtarrrsReplicationTaskAssessmentRun
    , crtarrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCancelReplicationTaskAssessmentRun' smart constructor.
newtype CancelReplicationTaskAssessmentRun = CancelReplicationTaskAssessmentRun'
  { replicationTaskAssessmentRunArn :: Core.Text
    -- ^ Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelReplicationTaskAssessmentRun' value with any optional fields omitted.
mkCancelReplicationTaskAssessmentRun
    :: Core.Text -- ^ 'replicationTaskAssessmentRunArn'
    -> CancelReplicationTaskAssessmentRun
mkCancelReplicationTaskAssessmentRun
  replicationTaskAssessmentRunArn
  = CancelReplicationTaskAssessmentRun'{replicationTaskAssessmentRunArn}

-- | Amazon Resource Name (ARN) of the premigration assessment run to be canceled.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarReplicationTaskAssessmentRunArn :: Lens.Lens' CancelReplicationTaskAssessmentRun Core.Text
crtarReplicationTaskAssessmentRunArn = Lens.field @"replicationTaskAssessmentRunArn"
{-# INLINEABLE crtarReplicationTaskAssessmentRunArn #-}
{-# DEPRECATED replicationTaskAssessmentRunArn "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunArn' instead"  #-}

instance Core.ToQuery CancelReplicationTaskAssessmentRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelReplicationTaskAssessmentRun where
        toHeaders CancelReplicationTaskAssessmentRun{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.CancelReplicationTaskAssessmentRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelReplicationTaskAssessmentRun where
        toJSON CancelReplicationTaskAssessmentRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationTaskAssessmentRunArn" Core..=
                       replicationTaskAssessmentRunArn)])

instance Core.AWSRequest CancelReplicationTaskAssessmentRun where
        type Rs CancelReplicationTaskAssessmentRun =
             CancelReplicationTaskAssessmentRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelReplicationTaskAssessmentRunResponse' Core.<$>
                   (x Core..:? "ReplicationTaskAssessmentRun") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkCancelReplicationTaskAssessmentRunResponse' smart constructor.
data CancelReplicationTaskAssessmentRunResponse = CancelReplicationTaskAssessmentRunResponse'
  { replicationTaskAssessmentRun :: Core.Maybe Types.ReplicationTaskAssessmentRun
    -- ^ The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CancelReplicationTaskAssessmentRunResponse' value with any optional fields omitted.
mkCancelReplicationTaskAssessmentRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelReplicationTaskAssessmentRunResponse
mkCancelReplicationTaskAssessmentRunResponse responseStatus
  = CancelReplicationTaskAssessmentRunResponse'{replicationTaskAssessmentRun
                                                  = Core.Nothing,
                                                responseStatus}

-- | The @ReplicationTaskAssessmentRun@ object for the canceled assessment run.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarrrsReplicationTaskAssessmentRun :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse (Core.Maybe Types.ReplicationTaskAssessmentRun)
crtarrrsReplicationTaskAssessmentRun = Lens.field @"replicationTaskAssessmentRun"
{-# INLINEABLE crtarrrsReplicationTaskAssessmentRun #-}
{-# DEPRECATED replicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtarrrsResponseStatus :: Lens.Lens' CancelReplicationTaskAssessmentRunResponse Core.Int
crtarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crtarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
