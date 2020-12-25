{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CreateLogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log stream for the specified log group. A log stream is a sequence of log events that originate from a single source, such as an application instance or a resource that is being monitored.
--
-- There is no limit on the number of log streams that you can create for a log group. There is a limit of 50 TPS on @CreateLogStream@ operations, after which transactions are throttled.
-- You must use the following guidelines when naming a log stream:
--
--     * Log stream names must be unique within the log group.
--
--
--     * Log stream names can be between 1 and 512 characters long.
--
--
--     * The ':' (colon) and '*' (asterisk) characters are not allowed.
module Network.AWS.CloudWatchLogs.CreateLogStream
  ( -- * Creating a request
    CreateLogStream (..),
    mkCreateLogStream,

    -- ** Request lenses
    clsLogGroupName,
    clsLogStreamName,

    -- * Destructuring the response
    CreateLogStreamResponse (..),
    mkCreateLogStreamResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLogStream' smart constructor.
data CreateLogStream = CreateLogStream'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The name of the log stream.
    logStreamName :: Types.LogStreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogStream' value with any optional fields omitted.
mkCreateLogStream ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'logStreamName'
  Types.LogStreamName ->
  CreateLogStream
mkCreateLogStream logGroupName logStreamName =
  CreateLogStream' {logGroupName, logStreamName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogGroupName :: Lens.Lens' CreateLogStream Types.LogGroupName
clsLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED clsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogStreamName :: Lens.Lens' CreateLogStream Types.LogStreamName
clsLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED clsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Core.FromJSON CreateLogStream where
  toJSON CreateLogStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("logStreamName" Core..= logStreamName)
          ]
      )

instance Core.AWSRequest CreateLogStream where
  type Rs CreateLogStream = CreateLogStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.CreateLogStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CreateLogStreamResponse'

-- | /See:/ 'mkCreateLogStreamResponse' smart constructor.
data CreateLogStreamResponse = CreateLogStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogStreamResponse' value with any optional fields omitted.
mkCreateLogStreamResponse ::
  CreateLogStreamResponse
mkCreateLogStreamResponse = CreateLogStreamResponse'
