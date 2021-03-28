{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.CloudWatchLogs.CreateLogStream
    (
    -- * Creating a request
      CreateLogStream (..)
    , mkCreateLogStream
    -- ** Request lenses
    , clsLogGroupName
    , clsLogStreamName

    -- * Destructuring the response
    , CreateLogStreamResponse (..)
    , mkCreateLogStreamResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLogStream' smart constructor.
data CreateLogStream = CreateLogStream'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , logStreamName :: Types.LogStreamName
    -- ^ The name of the log stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogStream' value with any optional fields omitted.
mkCreateLogStream
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> Types.LogStreamName -- ^ 'logStreamName'
    -> CreateLogStream
mkCreateLogStream logGroupName logStreamName
  = CreateLogStream'{logGroupName, logStreamName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogGroupName :: Lens.Lens' CreateLogStream Types.LogGroupName
clsLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE clsLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogStreamName :: Lens.Lens' CreateLogStream Types.LogStreamName
clsLogStreamName = Lens.field @"logStreamName"
{-# INLINEABLE clsLogStreamName #-}
{-# DEPRECATED logStreamName "Use generic-lens or generic-optics with 'logStreamName' instead"  #-}

instance Core.ToQuery CreateLogStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLogStream where
        toHeaders CreateLogStream{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.CreateLogStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLogStream where
        toJSON CreateLogStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("logStreamName" Core..= logStreamName)])

instance Core.AWSRequest CreateLogStream where
        type Rs CreateLogStream = CreateLogStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateLogStreamResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLogStreamResponse' smart constructor.
data CreateLogStreamResponse = CreateLogStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLogStreamResponse' value with any optional fields omitted.
mkCreateLogStreamResponse
    :: CreateLogStreamResponse
mkCreateLogStreamResponse = CreateLogStreamResponse'
