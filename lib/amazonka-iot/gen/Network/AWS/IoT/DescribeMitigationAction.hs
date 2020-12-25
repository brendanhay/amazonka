{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a mitigation action.
module Network.AWS.IoT.DescribeMitigationAction
  ( -- * Creating a request
    DescribeMitigationAction (..),
    mkDescribeMitigationAction,

    -- ** Request lenses
    dActionName,

    -- * Destructuring the response
    DescribeMitigationActionResponse (..),
    mkDescribeMitigationActionResponse,

    -- ** Response lenses
    dmarfrsActionArn,
    dmarfrsActionId,
    dmarfrsActionName,
    dmarfrsActionParams,
    dmarfrsActionType,
    dmarfrsCreationDate,
    dmarfrsLastModifiedDate,
    dmarfrsRoleArn,
    dmarfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMitigationAction' smart constructor.
newtype DescribeMitigationAction = DescribeMitigationAction'
  { -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Types.MitigationActionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMitigationAction' value with any optional fields omitted.
mkDescribeMitigationAction ::
  -- | 'actionName'
  Types.MitigationActionName ->
  DescribeMitigationAction
mkDescribeMitigationAction actionName =
  DescribeMitigationAction' {actionName}

-- | The friendly name that uniquely identifies the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActionName :: Lens.Lens' DescribeMitigationAction Types.MitigationActionName
dActionName = Lens.field @"actionName"
{-# DEPRECATED dActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Core.AWSRequest DescribeMitigationAction where
  type Rs DescribeMitigationAction = DescribeMitigationActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/mitigationactions/actions/" Core.<> (Core.toText actionName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMitigationActionResponse'
            Core.<$> (x Core..:? "actionArn")
            Core.<*> (x Core..:? "actionId")
            Core.<*> (x Core..:? "actionName")
            Core.<*> (x Core..:? "actionParams")
            Core.<*> (x Core..:? "actionType")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeMitigationActionResponse' smart constructor.
data DescribeMitigationActionResponse = DescribeMitigationActionResponse'
  { -- | The ARN that identifies this migration action.
    actionArn :: Core.Maybe Types.MitigationActionArn,
    -- | A unique identifier for this action.
    actionId :: Core.Maybe Types.MitigationActionId,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Core.Maybe Types.MitigationActionName,
    -- | Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
    actionParams :: Core.Maybe Types.MitigationActionParams,
    -- | The type of mitigation action.
    actionType :: Core.Maybe Types.MitigationActionType,
    -- | The date and time when the mitigation action was added to your AWS account.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The date and time when the mitigation action was last changed.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the IAM role used to apply this action.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeMitigationActionResponse' value with any optional fields omitted.
mkDescribeMitigationActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMitigationActionResponse
mkDescribeMitigationActionResponse responseStatus =
  DescribeMitigationActionResponse'
    { actionArn = Core.Nothing,
      actionId = Core.Nothing,
      actionName = Core.Nothing,
      actionParams = Core.Nothing,
      actionType = Core.Nothing,
      creationDate = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      roleArn = Core.Nothing,
      responseStatus
    }

-- | The ARN that identifies this migration action.
--
-- /Note:/ Consider using 'actionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsActionArn :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.MitigationActionArn)
dmarfrsActionArn = Lens.field @"actionArn"
{-# DEPRECATED dmarfrsActionArn "Use generic-lens or generic-optics with 'actionArn' instead." #-}

-- | A unique identifier for this action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsActionId :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.MitigationActionId)
dmarfrsActionId = Lens.field @"actionId"
{-# DEPRECATED dmarfrsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The friendly name that uniquely identifies the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsActionName :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.MitigationActionName)
dmarfrsActionName = Lens.field @"actionName"
{-# DEPRECATED dmarfrsActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsActionParams :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.MitigationActionParams)
dmarfrsActionParams = Lens.field @"actionParams"
{-# DEPRECATED dmarfrsActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | The type of mitigation action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsActionType :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.MitigationActionType)
dmarfrsActionType = Lens.field @"actionType"
{-# DEPRECATED dmarfrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The date and time when the mitigation action was added to your AWS account.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsCreationDate :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.NominalDiffTime)
dmarfrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dmarfrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The date and time when the mitigation action was last changed.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsLastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Core.NominalDiffTime)
dmarfrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED dmarfrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN of the IAM role used to apply this action.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsRoleArn :: Lens.Lens' DescribeMitigationActionResponse (Core.Maybe Types.RoleArn)
dmarfrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dmarfrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmarfrsResponseStatus :: Lens.Lens' DescribeMitigationActionResponse Core.Int
dmarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
