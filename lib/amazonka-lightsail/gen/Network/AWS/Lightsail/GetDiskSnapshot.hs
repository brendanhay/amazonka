{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk snapshot.
module Network.AWS.Lightsail.GetDiskSnapshot
  ( -- * Creating a request
    GetDiskSnapshot (..),
    mkGetDiskSnapshot,

    -- ** Request lenses
    gdsDiskSnapshotName,

    -- * Destructuring the response
    GetDiskSnapshotResponse (..),
    mkGetDiskSnapshotResponse,

    -- ** Response lenses
    gdsrsDiskSnapshot,
    gdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDiskSnapshot' smart constructor.
newtype GetDiskSnapshot = GetDiskSnapshot'
  { -- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
    diskSnapshotName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiskSnapshot' with the minimum fields required to make a request.
--
-- * 'diskSnapshotName' - The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
mkGetDiskSnapshot ::
  -- | 'diskSnapshotName'
  Lude.Text ->
  GetDiskSnapshot
mkGetDiskSnapshot pDiskSnapshotName_ =
  GetDiskSnapshot' {diskSnapshotName = pDiskSnapshotName_}

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDiskSnapshotName :: Lens.Lens' GetDiskSnapshot Lude.Text
gdsDiskSnapshotName = Lens.lens (diskSnapshotName :: GetDiskSnapshot -> Lude.Text) (\s a -> s {diskSnapshotName = a} :: GetDiskSnapshot)
{-# DEPRECATED gdsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

instance Lude.AWSRequest GetDiskSnapshot where
  type Rs GetDiskSnapshot = GetDiskSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDiskSnapshotResponse'
            Lude.<$> (x Lude..?> "diskSnapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDiskSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDiskSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDiskSnapshot where
  toJSON GetDiskSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("diskSnapshotName" Lude..= diskSnapshotName)]
      )

instance Lude.ToPath GetDiskSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDiskSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDiskSnapshotResponse' smart constructor.
data GetDiskSnapshotResponse = GetDiskSnapshotResponse'
  { -- | An object containing information about the disk snapshot.
    diskSnapshot :: Lude.Maybe DiskSnapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiskSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'diskSnapshot' - An object containing information about the disk snapshot.
-- * 'responseStatus' - The response status code.
mkGetDiskSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDiskSnapshotResponse
mkGetDiskSnapshotResponse pResponseStatus_ =
  GetDiskSnapshotResponse'
    { diskSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object containing information about the disk snapshot.
--
-- /Note:/ Consider using 'diskSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDiskSnapshot :: Lens.Lens' GetDiskSnapshotResponse (Lude.Maybe DiskSnapshot)
gdsrsDiskSnapshot = Lens.lens (diskSnapshot :: GetDiskSnapshotResponse -> Lude.Maybe DiskSnapshot) (\s a -> s {diskSnapshot = a} :: GetDiskSnapshotResponse)
{-# DEPRECATED gdsrsDiskSnapshot "Use generic-lens or generic-optics with 'diskSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDiskSnapshotResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDiskSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDiskSnapshotResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
