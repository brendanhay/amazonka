{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduled action.
module Network.AWS.AutoScaling.DeleteScheduledAction
    (
    -- * Creating a request
      DeleteScheduledAction (..)
    , mkDeleteScheduledAction
    -- ** Request lenses
    , dsaAutoScalingGroupName
    , dsaScheduledActionName

    -- * Destructuring the response
    , DeleteScheduledActionResponse (..)
    , mkDeleteScheduledActionResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , scheduledActionName :: Types.ResourceName
    -- ^ The name of the action to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledAction' value with any optional fields omitted.
mkDeleteScheduledAction
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> Types.ResourceName -- ^ 'scheduledActionName'
    -> DeleteScheduledAction
mkDeleteScheduledAction autoScalingGroupName scheduledActionName
  = DeleteScheduledAction'{autoScalingGroupName, scheduledActionName}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAutoScalingGroupName :: Lens.Lens' DeleteScheduledAction Types.ResourceName
dsaAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dsaAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The name of the action to delete.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaScheduledActionName :: Lens.Lens' DeleteScheduledAction Types.ResourceName
dsaScheduledActionName = Lens.field @"scheduledActionName"
{-# INLINEABLE dsaScheduledActionName #-}
{-# DEPRECATED scheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead"  #-}

instance Core.ToQuery DeleteScheduledAction where
        toQuery DeleteScheduledAction{..}
          = Core.toQueryPair "Action" ("DeleteScheduledAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "ScheduledActionName" scheduledActionName

instance Core.ToHeaders DeleteScheduledAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteScheduledAction where
        type Rs DeleteScheduledAction = DeleteScheduledActionResponse
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
        parseResponse = Response.receiveNull DeleteScheduledActionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteScheduledActionResponse' value with any optional fields omitted.
mkDeleteScheduledActionResponse
    :: DeleteScheduledActionResponse
mkDeleteScheduledActionResponse = DeleteScheduledActionResponse'
