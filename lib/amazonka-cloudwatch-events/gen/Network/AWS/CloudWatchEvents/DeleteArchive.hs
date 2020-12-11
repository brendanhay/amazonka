{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive.
module Network.AWS.CloudWatchEvents.DeleteArchive
  ( -- * Creating a request
    DeleteArchive (..),
    mkDeleteArchive,

    -- ** Request lenses
    dArchiveName,

    -- * Destructuring the response
    DeleteArchiveResponse (..),
    mkDeleteArchiveResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteArchive' smart constructor.
newtype DeleteArchive = DeleteArchive' {archiveName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteArchive' with the minimum fields required to make a request.
--
-- * 'archiveName' - The name of the archive to delete.
mkDeleteArchive ::
  -- | 'archiveName'
  Lude.Text ->
  DeleteArchive
mkDeleteArchive pArchiveName_ =
  DeleteArchive' {archiveName = pArchiveName_}

-- | The name of the archive to delete.
--
-- /Note:/ Consider using 'archiveName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArchiveName :: Lens.Lens' DeleteArchive Lude.Text
dArchiveName = Lens.lens (archiveName :: DeleteArchive -> Lude.Text) (\s a -> s {archiveName = a} :: DeleteArchive)
{-# DEPRECATED dArchiveName "Use generic-lens or generic-optics with 'archiveName' instead." #-}

instance Lude.AWSRequest DeleteArchive where
  type Rs DeleteArchive = DeleteArchiveResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteArchiveResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteArchive where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DeleteArchive" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteArchive where
  toJSON DeleteArchive' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ArchiveName" Lude..= archiveName)])

instance Lude.ToPath DeleteArchive where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteArchive where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteArchiveResponse' smart constructor.
newtype DeleteArchiveResponse = DeleteArchiveResponse'
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

-- | Creates a value of 'DeleteArchiveResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteArchiveResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteArchiveResponse
mkDeleteArchiveResponse pResponseStatus_ =
  DeleteArchiveResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteArchiveResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteArchiveResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteArchiveResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
