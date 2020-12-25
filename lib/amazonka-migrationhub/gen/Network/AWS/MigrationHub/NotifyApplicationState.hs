{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.NotifyApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the migration state of an application. For a given application identified by the value passed to @ApplicationId@ , its status is set or updated by passing one of three values to @Status@ : @NOT_STARTED | IN_PROGRESS | COMPLETED@ .
module Network.AWS.MigrationHub.NotifyApplicationState
  ( -- * Creating a request
    NotifyApplicationState (..),
    mkNotifyApplicationState,

    -- ** Request lenses
    nasApplicationId,
    nasStatus,
    nasDryRun,
    nasUpdateDateTime,

    -- * Destructuring the response
    NotifyApplicationStateResponse (..),
    mkNotifyApplicationStateResponse,

    -- ** Response lenses
    nasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkNotifyApplicationState' smart constructor.
data NotifyApplicationState = NotifyApplicationState'
  { -- | The configurationId in Application Discovery Service that uniquely identifies the grouped application.
    applicationId :: Types.ApplicationId,
    -- | Status of the application - Not Started, In-Progress, Complete.
    status :: Types.ApplicationStatus,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The timestamp when the application state changed.
    updateDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NotifyApplicationState' value with any optional fields omitted.
mkNotifyApplicationState ::
  -- | 'applicationId'
  Types.ApplicationId ->
  -- | 'status'
  Types.ApplicationStatus ->
  NotifyApplicationState
mkNotifyApplicationState applicationId status =
  NotifyApplicationState'
    { applicationId,
      status,
      dryRun = Core.Nothing,
      updateDateTime = Core.Nothing
    }

-- | The configurationId in Application Discovery Service that uniquely identifies the grouped application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasApplicationId :: Lens.Lens' NotifyApplicationState Types.ApplicationId
nasApplicationId = Lens.field @"applicationId"
{-# DEPRECATED nasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Status of the application - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasStatus :: Lens.Lens' NotifyApplicationState Types.ApplicationStatus
nasStatus = Lens.field @"status"
{-# DEPRECATED nasStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasDryRun :: Lens.Lens' NotifyApplicationState (Core.Maybe Core.Bool)
nasDryRun = Lens.field @"dryRun"
{-# DEPRECATED nasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The timestamp when the application state changed.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasUpdateDateTime :: Lens.Lens' NotifyApplicationState (Core.Maybe Core.NominalDiffTime)
nasUpdateDateTime = Lens.field @"updateDateTime"
{-# DEPRECATED nasUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

instance Core.FromJSON NotifyApplicationState where
  toJSON NotifyApplicationState {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationId" Core..= applicationId),
            Core.Just ("Status" Core..= status),
            ("DryRun" Core..=) Core.<$> dryRun,
            ("UpdateDateTime" Core..=) Core.<$> updateDateTime
          ]
      )

instance Core.AWSRequest NotifyApplicationState where
  type Rs NotifyApplicationState = NotifyApplicationStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.NotifyApplicationState")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyApplicationStateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkNotifyApplicationStateResponse' smart constructor.
newtype NotifyApplicationStateResponse = NotifyApplicationStateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyApplicationStateResponse' value with any optional fields omitted.
mkNotifyApplicationStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  NotifyApplicationStateResponse
mkNotifyApplicationStateResponse responseStatus =
  NotifyApplicationStateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nasrrsResponseStatus :: Lens.Lens' NotifyApplicationStateResponse Core.Int
nasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED nasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
