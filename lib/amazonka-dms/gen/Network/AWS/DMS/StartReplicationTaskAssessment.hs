{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task assessment for unsupported data types in the source database. 
module Network.AWS.DMS.StartReplicationTaskAssessment
    (
    -- * Creating a request
      StartReplicationTaskAssessment (..)
    , mkStartReplicationTaskAssessment
    -- ** Request lenses
    , srtaReplicationTaskArn

    -- * Destructuring the response
    , StartReplicationTaskAssessmentResponse (..)
    , mkStartReplicationTaskAssessmentResponse
    -- ** Response lenses
    , srtarrsReplicationTask
    , srtarrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkStartReplicationTaskAssessment' smart constructor.
newtype StartReplicationTaskAssessment = StartReplicationTaskAssessment'
  { replicationTaskArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartReplicationTaskAssessment' value with any optional fields omitted.
mkStartReplicationTaskAssessment
    :: Core.Text -- ^ 'replicationTaskArn'
    -> StartReplicationTaskAssessment
mkStartReplicationTaskAssessment replicationTaskArn
  = StartReplicationTaskAssessment'{replicationTaskArn}

-- | The Amazon Resource Name (ARN) of the replication task. 
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtaReplicationTaskArn :: Lens.Lens' StartReplicationTaskAssessment Core.Text
srtaReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE srtaReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

instance Core.ToQuery StartReplicationTaskAssessment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartReplicationTaskAssessment where
        toHeaders StartReplicationTaskAssessment{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.StartReplicationTaskAssessment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartReplicationTaskAssessment where
        toJSON StartReplicationTaskAssessment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn)])

instance Core.AWSRequest StartReplicationTaskAssessment where
        type Rs StartReplicationTaskAssessment =
             StartReplicationTaskAssessmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartReplicationTaskAssessmentResponse' Core.<$>
                   (x Core..:? "ReplicationTask") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkStartReplicationTaskAssessmentResponse' smart constructor.
data StartReplicationTaskAssessmentResponse = StartReplicationTaskAssessmentResponse'
  { replicationTask :: Core.Maybe Types.ReplicationTask
    -- ^ The assessed replication task. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartReplicationTaskAssessmentResponse' value with any optional fields omitted.
mkStartReplicationTaskAssessmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartReplicationTaskAssessmentResponse
mkStartReplicationTaskAssessmentResponse responseStatus
  = StartReplicationTaskAssessmentResponse'{replicationTask =
                                              Core.Nothing,
                                            responseStatus}

-- | The assessed replication task. 
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrsReplicationTask :: Lens.Lens' StartReplicationTaskAssessmentResponse (Core.Maybe Types.ReplicationTask)
srtarrsReplicationTask = Lens.field @"replicationTask"
{-# INLINEABLE srtarrsReplicationTask #-}
{-# DEPRECATED replicationTask "Use generic-lens or generic-optics with 'replicationTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrsResponseStatus :: Lens.Lens' StartReplicationTaskAssessmentResponse Core.Int
srtarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srtarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
