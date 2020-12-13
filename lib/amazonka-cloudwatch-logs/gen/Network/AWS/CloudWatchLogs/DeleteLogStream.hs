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

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLogStream' smart constructor.
data DeleteLogStream = DeleteLogStream'
  { -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The name of the log stream.
    logStreamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogStream' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'logStreamName' - The name of the log stream.
mkDeleteLogStream ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'logStreamName'
  Lude.Text ->
  DeleteLogStream
mkDeleteLogStream pLogGroupName_ pLogStreamName_ =
  DeleteLogStream'
    { logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsLogGroupName :: Lens.Lens' DeleteLogStream Lude.Text
dlsLogGroupName = Lens.lens (logGroupName :: DeleteLogStream -> Lude.Text) (\s a -> s {logGroupName = a} :: DeleteLogStream)
{-# DEPRECATED dlsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsLogStreamName :: Lens.Lens' DeleteLogStream Lude.Text
dlsLogStreamName = Lens.lens (logStreamName :: DeleteLogStream -> Lude.Text) (\s a -> s {logStreamName = a} :: DeleteLogStream)
{-# DEPRECATED dlsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

instance Lude.AWSRequest DeleteLogStream where
  type Rs DeleteLogStream = DeleteLogStreamResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteLogStreamResponse'

instance Lude.ToHeaders DeleteLogStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteLogStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLogStream where
  toJSON DeleteLogStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("logStreamName" Lude..= logStreamName)
          ]
      )

instance Lude.ToPath DeleteLogStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLogStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse = DeleteLogStreamResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogStreamResponse' with the minimum fields required to make a request.
mkDeleteLogStreamResponse ::
  DeleteLogStreamResponse
mkDeleteLogStreamResponse = DeleteLogStreamResponse'
