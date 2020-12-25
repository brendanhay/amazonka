{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DeleteProgressUpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a progress update stream, including all of its tasks, which was previously created as an AWS resource used for access control. This API has the following traits:
--
--
--     * The only parameter needed for @DeleteProgressUpdateStream@ is the stream name (same as a @CreateProgressUpdateStream@ call).
--
--
--     * The call will return, and a background process will asynchronously delete the stream and all of its resources (tasks, associated resources, resource attributes, created artifacts).
--
--
--     * If the stream takes time to be deleted, it might still show up on a @ListProgressUpdateStreams@ call.
--
--
--     * @CreateProgressUpdateStream@ , @ImportMigrationTask@ , @NotifyMigrationTaskState@ , and all Associate[*] APIs related to the tasks belonging to the stream will throw "InvalidInputException" if the stream of the same name is in the process of being deleted.
--
--
--     * Once the stream and all of its resources are deleted, @CreateProgressUpdateStream@ for a stream of the same name will succeed, and that stream will be an entirely new logical resource (without any resources associated with the old stream).
module Network.AWS.MigrationHub.DeleteProgressUpdateStream
  ( -- * Creating a request
    DeleteProgressUpdateStream (..),
    mkDeleteProgressUpdateStream,

    -- ** Request lenses
    dpusProgressUpdateStreamName,
    dpusDryRun,

    -- * Destructuring the response
    DeleteProgressUpdateStreamResponse (..),
    mkDeleteProgressUpdateStreamResponse,

    -- ** Response lenses
    dpusrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProgressUpdateStream' smart constructor.
data DeleteProgressUpdateStream = DeleteProgressUpdateStream'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
    progressUpdateStreamName :: Types.ProgressUpdateStreamName,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProgressUpdateStream' value with any optional fields omitted.
mkDeleteProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Types.ProgressUpdateStreamName ->
  DeleteProgressUpdateStream
mkDeleteProgressUpdateStream progressUpdateStreamName =
  DeleteProgressUpdateStream'
    { progressUpdateStreamName,
      dryRun = Core.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusProgressUpdateStreamName :: Lens.Lens' DeleteProgressUpdateStream Types.ProgressUpdateStreamName
dpusProgressUpdateStreamName = Lens.field @"progressUpdateStreamName"
{-# DEPRECATED dpusProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusDryRun :: Lens.Lens' DeleteProgressUpdateStream (Core.Maybe Core.Bool)
dpusDryRun = Lens.field @"dryRun"
{-# DEPRECATED dpusDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON DeleteProgressUpdateStream where
  toJSON DeleteProgressUpdateStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProgressUpdateStreamName" Core..= progressUpdateStreamName),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest DeleteProgressUpdateStream where
  type
    Rs DeleteProgressUpdateStream =
      DeleteProgressUpdateStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.DeleteProgressUpdateStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProgressUpdateStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProgressUpdateStreamResponse' smart constructor.
newtype DeleteProgressUpdateStreamResponse = DeleteProgressUpdateStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProgressUpdateStreamResponse' value with any optional fields omitted.
mkDeleteProgressUpdateStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProgressUpdateStreamResponse
mkDeleteProgressUpdateStreamResponse responseStatus =
  DeleteProgressUpdateStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusrrsResponseStatus :: Lens.Lens' DeleteProgressUpdateStreamResponse Core.Int
dpusrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpusrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
