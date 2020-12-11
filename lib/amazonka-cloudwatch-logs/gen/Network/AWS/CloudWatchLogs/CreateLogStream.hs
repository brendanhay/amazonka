{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLogStream' smart constructor.
data CreateLogStream = CreateLogStream'
  { logGroupName :: Lude.Text,
    logStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogStream' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'logStreamName' - The name of the log stream.
mkCreateLogStream ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'logStreamName'
  Lude.Text ->
  CreateLogStream
mkCreateLogStream pLogGroupName_ pLogStreamName_ =
  CreateLogStream'
    { logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogGroupName :: Lens.Lens' CreateLogStream Lude.Text
clsLogGroupName = Lens.lens (logGroupName :: CreateLogStream -> Lude.Text) (\s a -> s {logGroupName = a} :: CreateLogStream)
{-# DEPRECATED clsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clsLogStreamName :: Lens.Lens' CreateLogStream Lude.Text
clsLogStreamName = Lens.lens (logStreamName :: CreateLogStream -> Lude.Text) (\s a -> s {logStreamName = a} :: CreateLogStream)
{-# DEPRECATED clsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Lude.AWSRequest CreateLogStream where
  type Rs CreateLogStream = CreateLogStreamResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull CreateLogStreamResponse'

instance Lude.ToHeaders CreateLogStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.CreateLogStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLogStream where
  toJSON CreateLogStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("logStreamName" Lude..= logStreamName)
          ]
      )

instance Lude.ToPath CreateLogStream where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLogStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLogStreamResponse' smart constructor.
data CreateLogStreamResponse = CreateLogStreamResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogStreamResponse' with the minimum fields required to make a request.
mkCreateLogStreamResponse ::
  CreateLogStreamResponse
mkCreateLogStreamResponse = CreateLogStreamResponse'
