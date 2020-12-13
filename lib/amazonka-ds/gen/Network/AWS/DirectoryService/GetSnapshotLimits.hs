{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetSnapshotLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains the manual snapshot limits for a directory.
module Network.AWS.DirectoryService.GetSnapshotLimits
  ( -- * Creating a request
    GetSnapshotLimits (..),
    mkGetSnapshotLimits,

    -- ** Request lenses
    gslDirectoryId,

    -- * Destructuring the response
    GetSnapshotLimitsResponse (..),
    mkGetSnapshotLimitsResponse,

    -- ** Response lenses
    gslrsSnapshotLimits,
    gslrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'GetSnapshotLimits' operation.
--
-- /See:/ 'mkGetSnapshotLimits' smart constructor.
newtype GetSnapshotLimits = GetSnapshotLimits'
  { -- | Contains the identifier of the directory to obtain the limits for.
    directoryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSnapshotLimits' with the minimum fields required to make a request.
--
-- * 'directoryId' - Contains the identifier of the directory to obtain the limits for.
mkGetSnapshotLimits ::
  -- | 'directoryId'
  Lude.Text ->
  GetSnapshotLimits
mkGetSnapshotLimits pDirectoryId_ =
  GetSnapshotLimits' {directoryId = pDirectoryId_}

-- | Contains the identifier of the directory to obtain the limits for.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslDirectoryId :: Lens.Lens' GetSnapshotLimits Lude.Text
gslDirectoryId = Lens.lens (directoryId :: GetSnapshotLimits -> Lude.Text) (\s a -> s {directoryId = a} :: GetSnapshotLimits)
{-# DEPRECATED gslDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest GetSnapshotLimits where
  type Rs GetSnapshotLimits = GetSnapshotLimitsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSnapshotLimitsResponse'
            Lude.<$> (x Lude..?> "SnapshotLimits")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSnapshotLimits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.GetSnapshotLimits" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSnapshotLimits where
  toJSON GetSnapshotLimits' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DirectoryId" Lude..= directoryId)])

instance Lude.ToPath GetSnapshotLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSnapshotLimits where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'GetSnapshotLimits' operation.
--
-- /See:/ 'mkGetSnapshotLimitsResponse' smart constructor.
data GetSnapshotLimitsResponse = GetSnapshotLimitsResponse'
  { -- | A 'SnapshotLimits' object that contains the manual snapshot limits for the specified directory.
    snapshotLimits :: Lude.Maybe SnapshotLimits,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSnapshotLimitsResponse' with the minimum fields required to make a request.
--
-- * 'snapshotLimits' - A 'SnapshotLimits' object that contains the manual snapshot limits for the specified directory.
-- * 'responseStatus' - The response status code.
mkGetSnapshotLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSnapshotLimitsResponse
mkGetSnapshotLimitsResponse pResponseStatus_ =
  GetSnapshotLimitsResponse'
    { snapshotLimits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'SnapshotLimits' object that contains the manual snapshot limits for the specified directory.
--
-- /Note:/ Consider using 'snapshotLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrsSnapshotLimits :: Lens.Lens' GetSnapshotLimitsResponse (Lude.Maybe SnapshotLimits)
gslrsSnapshotLimits = Lens.lens (snapshotLimits :: GetSnapshotLimitsResponse -> Lude.Maybe SnapshotLimits) (\s a -> s {snapshotLimits = a} :: GetSnapshotLimitsResponse)
{-# DEPRECATED gslrsSnapshotLimits "Use generic-lens or generic-optics with 'snapshotLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrsResponseStatus :: Lens.Lens' GetSnapshotLimitsResponse Lude.Int
gslrsResponseStatus = Lens.lens (responseStatus :: GetSnapshotLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSnapshotLimitsResponse)
{-# DEPRECATED gslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
