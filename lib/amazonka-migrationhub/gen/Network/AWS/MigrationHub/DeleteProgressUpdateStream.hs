{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dpusDryRun,
    dpusProgressUpdateStreamName,

    -- * Destructuring the response
    DeleteProgressUpdateStreamResponse (..),
    mkDeleteProgressUpdateStreamResponse,

    -- ** Response lenses
    dpusrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProgressUpdateStream' smart constructor.
data DeleteProgressUpdateStream = DeleteProgressUpdateStream'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    progressUpdateStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProgressUpdateStream' with the minimum fields required to make a request.
--
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
-- * 'progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in this field./
mkDeleteProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Lude.Text ->
  DeleteProgressUpdateStream
mkDeleteProgressUpdateStream pProgressUpdateStreamName_ =
  DeleteProgressUpdateStream'
    { dryRun = Lude.Nothing,
      progressUpdateStreamName = pProgressUpdateStreamName_
    }

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusDryRun :: Lens.Lens' DeleteProgressUpdateStream (Lude.Maybe Lude.Bool)
dpusDryRun = Lens.lens (dryRun :: DeleteProgressUpdateStream -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteProgressUpdateStream)
{-# DEPRECATED dpusDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusProgressUpdateStreamName :: Lens.Lens' DeleteProgressUpdateStream Lude.Text
dpusProgressUpdateStreamName = Lens.lens (progressUpdateStreamName :: DeleteProgressUpdateStream -> Lude.Text) (\s a -> s {progressUpdateStreamName = a} :: DeleteProgressUpdateStream)
{-# DEPRECATED dpusProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

instance Lude.AWSRequest DeleteProgressUpdateStream where
  type
    Rs DeleteProgressUpdateStream =
      DeleteProgressUpdateStreamResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProgressUpdateStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProgressUpdateStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.DeleteProgressUpdateStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProgressUpdateStream where
  toJSON DeleteProgressUpdateStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just
              ("ProgressUpdateStreamName" Lude..= progressUpdateStreamName)
          ]
      )

instance Lude.ToPath DeleteProgressUpdateStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProgressUpdateStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProgressUpdateStreamResponse' smart constructor.
newtype DeleteProgressUpdateStreamResponse = DeleteProgressUpdateStreamResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProgressUpdateStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProgressUpdateStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProgressUpdateStreamResponse
mkDeleteProgressUpdateStreamResponse pResponseStatus_ =
  DeleteProgressUpdateStreamResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpusrsResponseStatus :: Lens.Lens' DeleteProgressUpdateStreamResponse Lude.Int
dpusrsResponseStatus = Lens.lens (responseStatus :: DeleteProgressUpdateStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProgressUpdateStreamResponse)
{-# DEPRECATED dpusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
