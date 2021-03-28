{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a scheduled managed action immediately. A managed action can be applied only if its status is @Scheduled@ . Get the status and action ID of a managed action with 'DescribeEnvironmentManagedActions' .
module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
    (
    -- * Creating a request
      ApplyEnvironmentManagedAction (..)
    , mkApplyEnvironmentManagedAction
    -- ** Request lenses
    , aemaActionId
    , aemaEnvironmentId
    , aemaEnvironmentName

    -- * Destructuring the response
    , ApplyEnvironmentManagedActionResponse (..)
    , mkApplyEnvironmentManagedActionResponse
    -- ** Response lenses
    , aemarrsActionDescription
    , aemarrsActionId
    , aemarrsActionType
    , aemarrsStatus
    , aemarrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to execute a scheduled managed action immediately.
--
-- /See:/ 'mkApplyEnvironmentManagedAction' smart constructor.
data ApplyEnvironmentManagedAction = ApplyEnvironmentManagedAction'
  { actionId :: Core.Text
    -- ^ The action ID of the scheduled managed action to execute.
  , environmentId :: Core.Maybe Core.Text
    -- ^ The environment ID of the target environment.
  , environmentName :: Core.Maybe Core.Text
    -- ^ The name of the target environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyEnvironmentManagedAction' value with any optional fields omitted.
mkApplyEnvironmentManagedAction
    :: Core.Text -- ^ 'actionId'
    -> ApplyEnvironmentManagedAction
mkApplyEnvironmentManagedAction actionId
  = ApplyEnvironmentManagedAction'{actionId,
                                   environmentId = Core.Nothing, environmentName = Core.Nothing}

-- | The action ID of the scheduled managed action to execute.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaActionId :: Lens.Lens' ApplyEnvironmentManagedAction Core.Text
aemaActionId = Lens.field @"actionId"
{-# INLINEABLE aemaActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentId :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Core.Text)
aemaEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE aemaEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentName :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Core.Text)
aemaEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE aemaEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery ApplyEnvironmentManagedAction where
        toQuery ApplyEnvironmentManagedAction{..}
          = Core.toQueryPair "Action"
              ("ApplyEnvironmentManagedAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ActionId" actionId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders ApplyEnvironmentManagedAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ApplyEnvironmentManagedAction where
        type Rs ApplyEnvironmentManagedAction =
             ApplyEnvironmentManagedActionResponse
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
          = Response.receiveXMLWrapper "ApplyEnvironmentManagedActionResult"
              (\ s h x ->
                 ApplyEnvironmentManagedActionResponse' Core.<$>
                   (x Core..@? "ActionDescription") Core.<*> x Core..@? "ActionId"
                     Core.<*> x Core..@? "ActionType"
                     Core.<*> x Core..@? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result message containing information about the managed action.
--
-- /See:/ 'mkApplyEnvironmentManagedActionResponse' smart constructor.
data ApplyEnvironmentManagedActionResponse = ApplyEnvironmentManagedActionResponse'
  { actionDescription :: Core.Maybe Core.Text
    -- ^ A description of the managed action.
  , actionId :: Core.Maybe Core.Text
    -- ^ The action ID of the managed action.
  , actionType :: Core.Maybe Types.ActionType
    -- ^ The type of managed action.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the managed action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyEnvironmentManagedActionResponse' value with any optional fields omitted.
mkApplyEnvironmentManagedActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ApplyEnvironmentManagedActionResponse
mkApplyEnvironmentManagedActionResponse responseStatus
  = ApplyEnvironmentManagedActionResponse'{actionDescription =
                                             Core.Nothing,
                                           actionId = Core.Nothing, actionType = Core.Nothing,
                                           status = Core.Nothing, responseStatus}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionDescription :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
aemarrsActionDescription = Lens.field @"actionDescription"
{-# INLINEABLE aemarrsActionDescription #-}
{-# DEPRECATED actionDescription "Use generic-lens or generic-optics with 'actionDescription' instead"  #-}

-- | The action ID of the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionId :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
aemarrsActionId = Lens.field @"actionId"
{-# INLINEABLE aemarrsActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The type of managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionType :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Types.ActionType)
aemarrsActionType = Lens.field @"actionType"
{-# INLINEABLE aemarrsActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | The status of the managed action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Core.Text)
aemarrsStatus = Lens.field @"status"
{-# INLINEABLE aemarrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsResponseStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse Core.Int
aemarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aemarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
