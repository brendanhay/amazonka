{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific instance snapshot.
module Network.AWS.Lightsail.GetInstanceSnapshot
  ( -- * Creating a request
    GetInstanceSnapshot (..),
    mkGetInstanceSnapshot,

    -- ** Request lenses
    gisInstanceSnapshotName,

    -- * Destructuring the response
    GetInstanceSnapshotResponse (..),
    mkGetInstanceSnapshotResponse,

    -- ** Response lenses
    gisrsInstanceSnapshot,
    gisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceSnapshot' smart constructor.
newtype GetInstanceSnapshot = GetInstanceSnapshot'
  { instanceSnapshotName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceSnapshot' with the minimum fields required to make a request.
--
-- * 'instanceSnapshotName' - The name of the snapshot for which you are requesting information.
mkGetInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Lude.Text ->
  GetInstanceSnapshot
mkGetInstanceSnapshot pInstanceSnapshotName_ =
  GetInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot for which you are requesting information.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisInstanceSnapshotName :: Lens.Lens' GetInstanceSnapshot Lude.Text
gisInstanceSnapshotName = Lens.lens (instanceSnapshotName :: GetInstanceSnapshot -> Lude.Text) (\s a -> s {instanceSnapshotName = a} :: GetInstanceSnapshot)
{-# DEPRECATED gisInstanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead." #-}

instance Lude.AWSRequest GetInstanceSnapshot where
  type Rs GetInstanceSnapshot = GetInstanceSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotResponse'
            Lude.<$> (x Lude..?> "instanceSnapshot")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstanceSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceSnapshot where
  toJSON GetInstanceSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("instanceSnapshotName" Lude..= instanceSnapshotName)]
      )

instance Lude.ToPath GetInstanceSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceSnapshotResponse' smart constructor.
data GetInstanceSnapshotResponse = GetInstanceSnapshotResponse'
  { instanceSnapshot ::
      Lude.Maybe InstanceSnapshot,
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

-- | Creates a value of 'GetInstanceSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'instanceSnapshot' - An array of key-value pairs containing information about the results of your get instance snapshot request.
-- * 'responseStatus' - The response status code.
mkGetInstanceSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceSnapshotResponse
mkGetInstanceSnapshotResponse pResponseStatus_ =
  GetInstanceSnapshotResponse'
    { instanceSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about the results of your get instance snapshot request.
--
-- /Note:/ Consider using 'instanceSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsInstanceSnapshot :: Lens.Lens' GetInstanceSnapshotResponse (Lude.Maybe InstanceSnapshot)
gisrsInstanceSnapshot = Lens.lens (instanceSnapshot :: GetInstanceSnapshotResponse -> Lude.Maybe InstanceSnapshot) (\s a -> s {instanceSnapshot = a} :: GetInstanceSnapshotResponse)
{-# DEPRECATED gisrsInstanceSnapshot "Use generic-lens or generic-optics with 'instanceSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetInstanceSnapshotResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetInstanceSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceSnapshotResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
