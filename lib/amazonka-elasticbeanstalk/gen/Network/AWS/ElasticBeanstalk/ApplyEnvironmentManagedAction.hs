{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ApplyEnvironmentManagedAction (..),
    mkApplyEnvironmentManagedAction,

    -- ** Request lenses
    aemaActionId,
    aemaEnvironmentId,
    aemaEnvironmentName,

    -- * Destructuring the response
    ApplyEnvironmentManagedActionResponse (..),
    mkApplyEnvironmentManagedActionResponse,

    -- ** Response lenses
    aemarrsActionDescription,
    aemarrsActionId,
    aemarrsActionType,
    aemarrsStatus,
    aemarrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to execute a scheduled managed action immediately.
--
-- /See:/ 'mkApplyEnvironmentManagedAction' smart constructor.
data ApplyEnvironmentManagedAction = ApplyEnvironmentManagedAction'
  { -- | The action ID of the scheduled managed action to execute.
    actionId :: Types.String,
    -- | The environment ID of the target environment.
    environmentId :: Core.Maybe Types.String,
    -- | The name of the target environment.
    environmentName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyEnvironmentManagedAction' value with any optional fields omitted.
mkApplyEnvironmentManagedAction ::
  -- | 'actionId'
  Types.String ->
  ApplyEnvironmentManagedAction
mkApplyEnvironmentManagedAction actionId =
  ApplyEnvironmentManagedAction'
    { actionId,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing
    }

-- | The action ID of the scheduled managed action to execute.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaActionId :: Lens.Lens' ApplyEnvironmentManagedAction Types.String
aemaActionId = Lens.field @"actionId"
{-# DEPRECATED aemaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentId :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Types.String)
aemaEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED aemaEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentName :: Lens.Lens' ApplyEnvironmentManagedAction (Core.Maybe Types.String)
aemaEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED aemaEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Core.AWSRequest ApplyEnvironmentManagedAction where
  type
    Rs ApplyEnvironmentManagedAction =
      ApplyEnvironmentManagedActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ApplyEnvironmentManagedAction")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ActionId" actionId)
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ApplyEnvironmentManagedActionResult"
      ( \s h x ->
          ApplyEnvironmentManagedActionResponse'
            Core.<$> (x Core..@? "ActionDescription")
            Core.<*> (x Core..@? "ActionId")
            Core.<*> (x Core..@? "ActionType")
            Core.<*> (x Core..@? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result message containing information about the managed action.
--
-- /See:/ 'mkApplyEnvironmentManagedActionResponse' smart constructor.
data ApplyEnvironmentManagedActionResponse = ApplyEnvironmentManagedActionResponse'
  { -- | A description of the managed action.
    actionDescription :: Core.Maybe Types.String,
    -- | The action ID of the managed action.
    actionId :: Core.Maybe Types.String,
    -- | The type of managed action.
    actionType :: Core.Maybe Types.ActionType,
    -- | The status of the managed action.
    status :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplyEnvironmentManagedActionResponse' value with any optional fields omitted.
mkApplyEnvironmentManagedActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ApplyEnvironmentManagedActionResponse
mkApplyEnvironmentManagedActionResponse responseStatus =
  ApplyEnvironmentManagedActionResponse'
    { actionDescription =
        Core.Nothing,
      actionId = Core.Nothing,
      actionType = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionDescription :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Types.String)
aemarrsActionDescription = Lens.field @"actionDescription"
{-# DEPRECATED aemarrsActionDescription "Use generic-lens or generic-optics with 'actionDescription' instead." #-}

-- | The action ID of the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionId :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Types.String)
aemarrsActionId = Lens.field @"actionId"
{-# DEPRECATED aemarrsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The type of managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsActionType :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Types.ActionType)
aemarrsActionType = Lens.field @"actionType"
{-# DEPRECATED aemarrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The status of the managed action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Core.Maybe Types.String)
aemarrsStatus = Lens.field @"status"
{-# DEPRECATED aemarrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarrsResponseStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse Core.Int
aemarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aemarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
