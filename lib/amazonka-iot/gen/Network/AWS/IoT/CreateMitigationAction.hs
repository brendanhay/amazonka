{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines an action that can be applied to audit findings by using StartAuditMitigationActionsTask. Only certain types of mitigation actions can be applied to specific check names. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-mitigation-actions.html Mitigation actions> . Each mitigation action can apply only one type of change.
module Network.AWS.IoT.CreateMitigationAction
    (
    -- * Creating a request
      CreateMitigationAction (..)
    , mkCreateMitigationAction
    -- ** Request lenses
    , cActionName
    , cRoleArn
    , cActionParams
    , cTags

    -- * Destructuring the response
    , CreateMitigationActionResponse (..)
    , mkCreateMitigationActionResponse
    -- ** Response lenses
    , cmarrsActionArn
    , cmarrsActionId
    , cmarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMitigationAction' smart constructor.
data CreateMitigationAction = CreateMitigationAction'
  { actionName :: Types.MitigationActionName
    -- ^ A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the IAM role that is used to apply the mitigation action.
  , actionParams :: Types.MitigationActionParams
    -- ^ Defines the type of action and the parameters for that action.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata that can be used to manage the mitigation action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMitigationAction' value with any optional fields omitted.
mkCreateMitigationAction
    :: Types.MitigationActionName -- ^ 'actionName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Types.MitigationActionParams -- ^ 'actionParams'
    -> CreateMitigationAction
mkCreateMitigationAction actionName roleArn actionParams
  = CreateMitigationAction'{actionName, roleArn, actionParams,
                            tags = Core.Nothing}

-- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionName :: Lens.Lens' CreateMitigationAction Types.MitigationActionName
cActionName = Lens.field @"actionName"
{-# INLINEABLE cActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | The ARN of the IAM role that is used to apply the mitigation action.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRoleArn :: Lens.Lens' CreateMitigationAction Types.RoleArn
cRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Defines the type of action and the parameters for that action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionParams :: Lens.Lens' CreateMitigationAction Types.MitigationActionParams
cActionParams = Lens.field @"actionParams"
{-# INLINEABLE cActionParams #-}
{-# DEPRECATED actionParams "Use generic-lens or generic-optics with 'actionParams' instead"  #-}

-- | Metadata that can be used to manage the mitigation action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateMitigationAction (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateMitigationAction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMitigationAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateMitigationAction where
        toJSON CreateMitigationAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("actionParams" Core..= actionParams),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateMitigationAction where
        type Rs CreateMitigationAction = CreateMitigationActionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/mitigationactions/actions/" Core.<> Core.toText actionName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMitigationActionResponse' Core.<$>
                   (x Core..:? "actionArn") Core.<*> x Core..:? "actionId" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateMitigationActionResponse' smart constructor.
data CreateMitigationActionResponse = CreateMitigationActionResponse'
  { actionArn :: Core.Maybe Types.ActionArn
    -- ^ The ARN for the new mitigation action.
  , actionId :: Core.Maybe Types.ActionId
    -- ^ A unique identifier for the new mitigation action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMitigationActionResponse' value with any optional fields omitted.
mkCreateMitigationActionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMitigationActionResponse
mkCreateMitigationActionResponse responseStatus
  = CreateMitigationActionResponse'{actionArn = Core.Nothing,
                                    actionId = Core.Nothing, responseStatus}

-- | The ARN for the new mitigation action.
--
-- /Note:/ Consider using 'actionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsActionArn :: Lens.Lens' CreateMitigationActionResponse (Core.Maybe Types.ActionArn)
cmarrsActionArn = Lens.field @"actionArn"
{-# INLINEABLE cmarrsActionArn #-}
{-# DEPRECATED actionArn "Use generic-lens or generic-optics with 'actionArn' instead"  #-}

-- | A unique identifier for the new mitigation action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsActionId :: Lens.Lens' CreateMitigationActionResponse (Core.Maybe Types.ActionId)
cmarrsActionId = Lens.field @"actionId"
{-# INLINEABLE cmarrsActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsResponseStatus :: Lens.Lens' CreateMitigationActionResponse Core.Int
cmarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
