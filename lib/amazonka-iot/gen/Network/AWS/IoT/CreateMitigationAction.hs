{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateMitigationAction (..),
    mkCreateMitigationAction,

    -- ** Request lenses
    cActionName,
    cRoleArn,
    cActionParams,
    cTags,

    -- * Destructuring the response
    CreateMitigationActionResponse (..),
    mkCreateMitigationActionResponse,

    -- ** Response lenses
    cmarrsActionArn,
    cmarrsActionId,
    cmarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMitigationAction' smart constructor.
data CreateMitigationAction = CreateMitigationAction'
  { -- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
    actionName :: Types.MitigationActionName,
    -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleArn :: Types.RoleArn,
    -- | Defines the type of action and the parameters for that action.
    actionParams :: Types.MitigationActionParams,
    -- | Metadata that can be used to manage the mitigation action.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMitigationAction' value with any optional fields omitted.
mkCreateMitigationAction ::
  -- | 'actionName'
  Types.MitigationActionName ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'actionParams'
  Types.MitigationActionParams ->
  CreateMitigationAction
mkCreateMitigationAction actionName roleArn actionParams =
  CreateMitigationAction'
    { actionName,
      roleArn,
      actionParams,
      tags = Core.Nothing
    }

-- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionName :: Lens.Lens' CreateMitigationAction Types.MitigationActionName
cActionName = Lens.field @"actionName"
{-# DEPRECATED cActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The ARN of the IAM role that is used to apply the mitigation action.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRoleArn :: Lens.Lens' CreateMitigationAction Types.RoleArn
cRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Defines the type of action and the parameters for that action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionParams :: Lens.Lens' CreateMitigationAction Types.MitigationActionParams
cActionParams = Lens.field @"actionParams"
{-# DEPRECATED cActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | Metadata that can be used to manage the mitigation action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateMitigationAction (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateMitigationAction where
  toJSON CreateMitigationAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("actionParams" Core..= actionParams),
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateMitigationAction where
  type Rs CreateMitigationAction = CreateMitigationActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/mitigationactions/actions/" Core.<> (Core.toText actionName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMitigationActionResponse'
            Core.<$> (x Core..:? "actionArn")
            Core.<*> (x Core..:? "actionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateMitigationActionResponse' smart constructor.
data CreateMitigationActionResponse = CreateMitigationActionResponse'
  { -- | The ARN for the new mitigation action.
    actionArn :: Core.Maybe Types.ActionArn,
    -- | A unique identifier for the new mitigation action.
    actionId :: Core.Maybe Types.ActionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMitigationActionResponse' value with any optional fields omitted.
mkCreateMitigationActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMitigationActionResponse
mkCreateMitigationActionResponse responseStatus =
  CreateMitigationActionResponse'
    { actionArn = Core.Nothing,
      actionId = Core.Nothing,
      responseStatus
    }

-- | The ARN for the new mitigation action.
--
-- /Note:/ Consider using 'actionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsActionArn :: Lens.Lens' CreateMitigationActionResponse (Core.Maybe Types.ActionArn)
cmarrsActionArn = Lens.field @"actionArn"
{-# DEPRECATED cmarrsActionArn "Use generic-lens or generic-optics with 'actionArn' instead." #-}

-- | A unique identifier for the new mitigation action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsActionId :: Lens.Lens' CreateMitigationActionResponse (Core.Maybe Types.ActionId)
cmarrsActionId = Lens.field @"actionId"
{-# DEPRECATED cmarrsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarrsResponseStatus :: Lens.Lens' CreateMitigationActionResponse Core.Int
cmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
