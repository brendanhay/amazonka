{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log stream and permanently deletes all the archived log events associated with the log stream.
module Network.AWS.CloudWatchLogs.DeleteLogStream
  ( -- * Creating a request
    DeleteLogStream (..),
    mkDeleteLogStream,

    -- ** Request lenses
    dlsLogGroupName,
    dlsLogStreamName,

    -- * Destructuring the response
    DeleteLogStreamResponse (..),
    mkDeleteLogStreamResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLogStream' smart constructor.
data DeleteLogStream = DeleteLogStream'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The name of the log stream.
    logStreamName :: Types.LogStreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogStream' value with any optional fields omitted.
mkDeleteLogStream ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'logStreamName'
  Types.LogStreamName ->
  DeleteLogStream
mkDeleteLogStream logGroupName logStreamName =
  DeleteLogStream' {logGroupName, logStreamName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsLogGroupName :: Lens.Lens' DeleteLogStream Types.LogGroupName
dlsLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dlsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsLogStreamName :: Lens.Lens' DeleteLogStream Types.LogStreamName
dlsLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED dlsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Core.FromJSON DeleteLogStream where
  toJSON DeleteLogStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("logStreamName" Core..= logStreamName)
          ]
      )

instance Core.AWSRequest DeleteLogStream where
  type Rs DeleteLogStream = DeleteLogStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DeleteLogStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteLogStreamResponse'

-- | /See:/ 'mkDeleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse = DeleteLogStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogStreamResponse' value with any optional fields omitted.
mkDeleteLogStreamResponse ::
  DeleteLogStreamResponse
mkDeleteLogStreamResponse = DeleteLogStreamResponse'
