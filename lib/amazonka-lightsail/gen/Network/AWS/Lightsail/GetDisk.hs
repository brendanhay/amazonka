{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific block storage disk.
module Network.AWS.Lightsail.GetDisk
  ( -- * Creating a request
    GetDisk (..),
    mkGetDisk,

    -- ** Request lenses
    gdDiskName,

    -- * Destructuring the response
    GetDiskResponse (..),
    mkGetDiskResponse,

    -- ** Response lenses
    gdgrsDisk,
    gdgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDisk' smart constructor.
newtype GetDisk = GetDisk'
  { -- | The name of the disk (e.g., @my-disk@ ).
    diskName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDisk' with the minimum fields required to make a request.
--
-- * 'diskName' - The name of the disk (e.g., @my-disk@ ).
mkGetDisk ::
  -- | 'diskName'
  Lude.Text ->
  GetDisk
mkGetDisk pDiskName_ = GetDisk' {diskName = pDiskName_}

-- | The name of the disk (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDiskName :: Lens.Lens' GetDisk Lude.Text
gdDiskName = Lens.lens (diskName :: GetDisk -> Lude.Text) (\s a -> s {diskName = a} :: GetDisk)
{-# DEPRECATED gdDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

instance Lude.AWSRequest GetDisk where
  type Rs GetDisk = GetDiskResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDiskResponse'
            Lude.<$> (x Lude..?> "disk") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDisk where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDisk" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDisk where
  toJSON GetDisk' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("diskName" Lude..= diskName)])

instance Lude.ToPath GetDisk where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDisk where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDiskResponse' smart constructor.
data GetDiskResponse = GetDiskResponse'
  { -- | An object containing information about the disk.
    disk :: Lude.Maybe Disk,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiskResponse' with the minimum fields required to make a request.
--
-- * 'disk' - An object containing information about the disk.
-- * 'responseStatus' - The response status code.
mkGetDiskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDiskResponse
mkGetDiskResponse pResponseStatus_ =
  GetDiskResponse'
    { disk = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object containing information about the disk.
--
-- /Note:/ Consider using 'disk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsDisk :: Lens.Lens' GetDiskResponse (Lude.Maybe Disk)
gdgrsDisk = Lens.lens (disk :: GetDiskResponse -> Lude.Maybe Disk) (\s a -> s {disk = a} :: GetDiskResponse)
{-# DEPRECATED gdgrsDisk "Use generic-lens or generic-optics with 'disk' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrsResponseStatus :: Lens.Lens' GetDiskResponse Lude.Int
gdgrsResponseStatus = Lens.lens (responseStatus :: GetDiskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDiskResponse)
{-# DEPRECATED gdgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
