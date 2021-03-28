{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for the specified mitigation action.
module Network.AWS.IoT.UpdateMitigationAction
    (
    -- * Creating a request
      UpdateMitigationAction (..)
    , mkUpdateMitigationAction
    -- ** Request lenses
    , umaActionName
    , umaActionParams
    , umaRoleArn

    -- * Destructuring the response
    , UpdateMitigationActionResponse (..)
    , mkUpdateMitigationActionResponse
    -- ** Response lenses
    , umarrsActionArn
    , umarrsActionId
    , umarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { actionName :: Types.MitigationActionName
    -- ^ The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
  , actionParams :: Core.Maybe Types.MitigationActionParams
    -- ^ Defines the type of action and the parameters for that action.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the IAM role that is used to apply the mitigation action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMitigationAction' value with any optional fields omitted.
mkUpdateMitigationAction
    :: Types.MitigationActionName -- ^ 'actionName'
    -> UpdateMitigationAction
mkUpdateMitigationAction actionName
  = UpdateMitigationAction'{actionName, actionParams = Core.Nothing,
                            roleArn = Core.Nothing}

-- | The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaActionName :: Lens.Lens' UpdateMitigationAction Types.MitigationActionName
umaActionName = Lens.field @"actionName"
{-# INLINEABLE umaActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Defines the type of action and the parameters for that action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaActionParams :: Lens.Lens' UpdateMitigationAction (Core.Maybe Types.MitigationActionParams)
umaActionParams = Lens.field @"actionParams"
{-# INLINEABLE umaActionParams #-}
{-# DEPRECATED actionParams "Use generic-lens or generic-optics with 'actionParams' instead"  #-}

-- | The ARN of the IAM role that is used to apply the mitigation action.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaRoleArn :: Lens.Lens' UpdateMitigationAction (Core.Maybe Types.RoleArn)
umaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE umaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery UpdateMitigationAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMitigationAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateMitigationAction where
        toJSON UpdateMitigationAction{..}
          = Core.object
              (Core.catMaybes
                 [("actionParams" Core..=) Core.<$> actionParams,
                  ("roleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest UpdateMitigationAction where
        type Rs UpdateMitigationAction = UpdateMitigationActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/mitigationactions/actions/" Core.<> Core.toText actionName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMitigationActionResponse' Core.<$>
                   (x Core..:? "actionArn") Core.<*> x Core..:? "actionId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateMitigationActionResponse' smart constructor.
data UpdateMitigationActionResponse = UpdateMitigationActionResponse'
  { actionArn :: Core.Maybe Types.ActionArn
    -- ^ The ARN for the new mitigation action.
  , actionId :: Core.Maybe Types.ActionId
    -- ^ A unique identifier for the mitigation action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMitigationActionResponse' value with any optional fields omitted.
mkUpdateMitigationActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMitigationActionResponse
mkUpdateMitigationActionResponse responseStatus
  = UpdateMitigationActionResponse'{actionArn = Core.Nothing,
                                    actionId = Core.Nothing, responseStatus}

-- | The ARN for the new mitigation action.
--
-- /Note:/ Consider using 'actionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarrsActionArn :: Lens.Lens' UpdateMitigationActionResponse (Core.Maybe Types.ActionArn)
umarrsActionArn = Lens.field @"actionArn"
{-# INLINEABLE umarrsActionArn #-}
{-# DEPRECATED actionArn "Use generic-lens or generic-optics with 'actionArn' instead"  #-}

-- | A unique identifier for the mitigation action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarrsActionId :: Lens.Lens' UpdateMitigationActionResponse (Core.Maybe Types.ActionId)
umarrsActionId = Lens.field @"actionId"
{-# INLINEABLE umarrsActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarrsResponseStatus :: Lens.Lens' UpdateMitigationActionResponse Core.Int
umarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
