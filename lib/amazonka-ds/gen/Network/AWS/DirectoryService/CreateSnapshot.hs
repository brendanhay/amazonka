{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS cloud.
module Network.AWS.DirectoryService.CreateSnapshot
  ( -- * Creating a request
    CreateSnapshot (..),
    mkCreateSnapshot,

    -- ** Request lenses
    csName,
    csDirectoryId,

    -- * Destructuring the response
    CreateSnapshotResponse (..),
    mkCreateSnapshotResponse,

    -- ** Response lenses
    csrsSnapshotId,
    csrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'CreateSnapshot' operation.
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { name :: Lude.Maybe Lude.Text,
    directoryId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory of which to take a snapshot.
-- * 'name' - The descriptive name to apply to the snapshot.
mkCreateSnapshot ::
  -- | 'directoryId'
  Lude.Text ->
  CreateSnapshot
mkCreateSnapshot pDirectoryId_ =
  CreateSnapshot' {name = Lude.Nothing, directoryId = pDirectoryId_}

-- | The descriptive name to apply to the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Text)
csName = Lens.lens (name :: CreateSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateSnapshot)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the directory of which to take a snapshot.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDirectoryId :: Lens.Lens' CreateSnapshot Lude.Text
csDirectoryId = Lens.lens (directoryId :: CreateSnapshot -> Lude.Text) (\s a -> s {directoryId = a} :: CreateSnapshot)
{-# DEPRECATED csDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Lude.<$> (x Lude..?> "SnapshotId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.CreateSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath CreateSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshot where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'CreateSnapshot' operation.
--
-- /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshotId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshotId' - The identifier of the snapshot that was created.
mkCreateSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotResponse
mkCreateSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    { snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the snapshot that was created.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSnapshotId :: Lens.Lens' CreateSnapshotResponse (Lude.Maybe Lude.Text)
csrsSnapshotId = Lens.lens (snapshotId :: CreateSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateSnapshotResponse)
{-# DEPRECATED csrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
