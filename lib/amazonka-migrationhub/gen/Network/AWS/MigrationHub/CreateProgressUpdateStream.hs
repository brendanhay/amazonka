{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.CreateProgressUpdateStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a progress update stream which is an AWS resource used for access control as well as a namespace for migration task names that is implicitly linked to your AWS account. It must uniquely identify the migration tool as it is used for all updates made by the tool; however, it does not need to be unique for each AWS account because it is scoped to the AWS account.
module Network.AWS.MigrationHub.CreateProgressUpdateStream
  ( -- * Creating a request
    CreateProgressUpdateStream (..),
    mkCreateProgressUpdateStream,

    -- ** Request lenses
    cpusProgressUpdateStreamName,
    cpusDryRun,

    -- * Destructuring the response
    CreateProgressUpdateStreamResponse (..),
    mkCreateProgressUpdateStreamResponse,

    -- ** Response lenses
    cpusrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProgressUpdateStream' smart constructor.
data CreateProgressUpdateStream = CreateProgressUpdateStream'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
    progressUpdateStreamName :: Types.ProgressUpdateStreamName,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProgressUpdateStream' value with any optional fields omitted.
mkCreateProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Types.ProgressUpdateStreamName ->
  CreateProgressUpdateStream
mkCreateProgressUpdateStream progressUpdateStreamName =
  CreateProgressUpdateStream'
    { progressUpdateStreamName,
      dryRun = Core.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusProgressUpdateStreamName :: Lens.Lens' CreateProgressUpdateStream Types.ProgressUpdateStreamName
cpusProgressUpdateStreamName = Lens.field @"progressUpdateStreamName"
{-# DEPRECATED cpusProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusDryRun :: Lens.Lens' CreateProgressUpdateStream (Core.Maybe Core.Bool)
cpusDryRun = Lens.field @"dryRun"
{-# DEPRECATED cpusDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.FromJSON CreateProgressUpdateStream where
  toJSON CreateProgressUpdateStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProgressUpdateStreamName" Core..= progressUpdateStreamName),
            ("DryRun" Core..=) Core.<$> dryRun
          ]
      )

instance Core.AWSRequest CreateProgressUpdateStream where
  type
    Rs CreateProgressUpdateStream =
      CreateProgressUpdateStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSMigrationHub.CreateProgressUpdateStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProgressUpdateStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProgressUpdateStreamResponse' smart constructor.
newtype CreateProgressUpdateStreamResponse = CreateProgressUpdateStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProgressUpdateStreamResponse' value with any optional fields omitted.
mkCreateProgressUpdateStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProgressUpdateStreamResponse
mkCreateProgressUpdateStreamResponse responseStatus =
  CreateProgressUpdateStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpusrrsResponseStatus :: Lens.Lens' CreateProgressUpdateStreamResponse Core.Int
cpusrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpusrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
