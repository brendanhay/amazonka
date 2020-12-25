{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ImportMigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new migration task which represents a server, database, etc., being migrated to AWS by a migration tool.
--
-- This API is a prerequisite to calling the @NotifyMigrationTaskState@ API as the migration tool must first register the migration task with Migration Hub.
module Network.AWS.MigrationHub.ImportMigrationTask
  ( -- * Creating a request
    ImportMigrationTask (..),
    mkImportMigrationTask,

    -- ** Request lenses
    imtProgressUpdateStream,
    imtMigrationTaskName,
    imtDryRun,

    -- * Destructuring the response
    ImportMigrationTaskResponse (..),
    mkImportMigrationTaskResponse,

    -- ** Response lenses
    imtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportMigrationTask' smart constructor.
data ImportMigrationTask = ImportMigrationTask'
  { -- | The name of the ProgressUpdateStream. >
    progressUpdateStream :: Types.ProgressUpdateStream,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Types.MigrationTaskName,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportMigrationTask' value with any optional fields omitted.
mkImportMigrationTask ::
  -- | 'progressUpdateStream'
  Types.ProgressUpdateStream ->
  -- | 'migrationTaskName'
  Types.MigrationTaskName ->
  ImportMigrationTask
mkImportMigrationTask progressUpdateStream migrationTaskName =
  ImportMigrationTask'
    { progressUpdateStream,
      migrationTaskName,
      dryRun = Core.Nothing
    }

-- | The name of the ProgressUpdateStream. >
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtProgressUpdateStream :: Lens.Lens' ImportMigrationTask Types.ProgressUpdateStream
imtProgressUpdateStream = Lens.field @"progressUpdateStream"
{-# DEPRECATED imtProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtMigrationTaskName :: Lens.Lens' ImportMigrationTask Types.MigrationTaskName
imtMigrationTaskName = Lens.field @"migrationTaskName"
{-# DEPRECATED imtMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtDryRun :: Lens.Lens' ImportMigrationTask (Core.Maybe Core.Bool)
imtDryRun = Lens.field @"dryRun"
{-# DEPRECATED imtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON ImportMigrationTask where
  toJSON ImportMigrationTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProgressUpdateStream" Core..= progressUpdateStream),
            Core.Just ("MigrationTaskName" Core..= migrationTaskName),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest ImportMigrationTask where
  type Rs ImportMigrationTask = ImportMigrationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSMigrationHub.ImportMigrationTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ImportMigrationTaskResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkImportMigrationTaskResponse' smart constructor.
newtype ImportMigrationTaskResponse = ImportMigrationTaskResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImportMigrationTaskResponse' value with any optional fields omitted.
mkImportMigrationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ImportMigrationTaskResponse
mkImportMigrationTaskResponse responseStatus =
  ImportMigrationTaskResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imtrrsResponseStatus :: Lens.Lens' ImportMigrationTaskResponse Core.Int
imtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED imtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
