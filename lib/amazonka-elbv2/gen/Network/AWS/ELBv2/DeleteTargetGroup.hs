{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified target group.
--
-- You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks. Deleting a target group does not affect its registered targets. For example, any EC2 instances continue to run until you stop or terminate them.
module Network.AWS.ELBv2.DeleteTargetGroup
    (
    -- * Creating a request
      DeleteTargetGroup (..)
    , mkDeleteTargetGroup
    -- ** Request lenses
    , dtgTargetGroupArn

    -- * Destructuring the response
    , DeleteTargetGroupResponse (..)
    , mkDeleteTargetGroupResponse
    -- ** Response lenses
    , dtgrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTargetGroup' smart constructor.
newtype DeleteTargetGroup = DeleteTargetGroup'
  { targetGroupArn :: Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTargetGroup' value with any optional fields omitted.
mkDeleteTargetGroup
    :: Types.TargetGroupArn -- ^ 'targetGroupArn'
    -> DeleteTargetGroup
mkDeleteTargetGroup targetGroupArn
  = DeleteTargetGroup'{targetGroupArn}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTargetGroupArn :: Lens.Lens' DeleteTargetGroup Types.TargetGroupArn
dtgTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE dtgTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

instance Core.ToQuery DeleteTargetGroup where
        toQuery DeleteTargetGroup{..}
          = Core.toQueryPair "Action" ("DeleteTargetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupArn" targetGroupArn

instance Core.ToHeaders DeleteTargetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTargetGroup where
        type Rs DeleteTargetGroup = DeleteTargetGroupResponse
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
          = Response.receiveXMLWrapper "DeleteTargetGroupResult"
              (\ s h x ->
                 DeleteTargetGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTargetGroupResponse' smart constructor.
newtype DeleteTargetGroupResponse = DeleteTargetGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTargetGroupResponse' value with any optional fields omitted.
mkDeleteTargetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTargetGroupResponse
mkDeleteTargetGroupResponse responseStatus
  = DeleteTargetGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsResponseStatus :: Lens.Lens' DeleteTargetGroupResponse Core.Int
dtgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
