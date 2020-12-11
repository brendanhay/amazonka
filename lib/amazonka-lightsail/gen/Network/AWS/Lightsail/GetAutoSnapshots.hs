{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetAutoSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the available automatic snapshots for an instance or disk. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.GetAutoSnapshots
  ( -- * Creating a request
    GetAutoSnapshots (..),
    mkGetAutoSnapshots,

    -- ** Request lenses
    gasResourceName,

    -- * Destructuring the response
    GetAutoSnapshotsResponse (..),
    mkGetAutoSnapshotsResponse,

    -- ** Response lenses
    gasrsResourceType,
    gasrsResourceName,
    gasrsAutoSnapshots,
    gasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAutoSnapshots' smart constructor.
newtype GetAutoSnapshots = GetAutoSnapshots'
  { resourceName ::
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

-- | Creates a value of 'GetAutoSnapshots' with the minimum fields required to make a request.
--
-- * 'resourceName' - The name of the source instance or disk from which to get automatic snapshot information.
mkGetAutoSnapshots ::
  -- | 'resourceName'
  Lude.Text ->
  GetAutoSnapshots
mkGetAutoSnapshots pResourceName_ =
  GetAutoSnapshots' {resourceName = pResourceName_}

-- | The name of the source instance or disk from which to get automatic snapshot information.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasResourceName :: Lens.Lens' GetAutoSnapshots Lude.Text
gasResourceName = Lens.lens (resourceName :: GetAutoSnapshots -> Lude.Text) (\s a -> s {resourceName = a} :: GetAutoSnapshots)
{-# DEPRECATED gasResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Lude.AWSRequest GetAutoSnapshots where
  type Rs GetAutoSnapshots = GetAutoSnapshotsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAutoSnapshotsResponse'
            Lude.<$> (x Lude..?> "resourceType")
            Lude.<*> (x Lude..?> "resourceName")
            Lude.<*> (x Lude..?> "autoSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAutoSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetAutoSnapshots" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAutoSnapshots where
  toJSON GetAutoSnapshots' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("resourceName" Lude..= resourceName)])

instance Lude.ToPath GetAutoSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAutoSnapshots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAutoSnapshotsResponse' smart constructor.
data GetAutoSnapshotsResponse = GetAutoSnapshotsResponse'
  { resourceType ::
      Lude.Maybe ResourceType,
    resourceName :: Lude.Maybe Lude.Text,
    autoSnapshots ::
      Lude.Maybe [AutoSnapshotDetails],
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

-- | Creates a value of 'GetAutoSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'autoSnapshots' - An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
-- * 'resourceName' - The name of the source instance or disk for the automatic snapshots.
-- * 'resourceType' - The resource type (e.g., @Instance@ or @Disk@ ).
-- * 'responseStatus' - The response status code.
mkGetAutoSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAutoSnapshotsResponse
mkGetAutoSnapshotsResponse pResponseStatus_ =
  GetAutoSnapshotsResponse'
    { resourceType = Lude.Nothing,
      resourceName = Lude.Nothing,
      autoSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource type (e.g., @Instance@ or @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResourceType :: Lens.Lens' GetAutoSnapshotsResponse (Lude.Maybe ResourceType)
gasrsResourceType = Lens.lens (resourceType :: GetAutoSnapshotsResponse -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: GetAutoSnapshotsResponse)
{-# DEPRECATED gasrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the source instance or disk for the automatic snapshots.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResourceName :: Lens.Lens' GetAutoSnapshotsResponse (Lude.Maybe Lude.Text)
gasrsResourceName = Lens.lens (resourceName :: GetAutoSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: GetAutoSnapshotsResponse)
{-# DEPRECATED gasrsResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | An array of objects that describe the automatic snapshots that are available for the specified source instance or disk.
--
-- /Note:/ Consider using 'autoSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsAutoSnapshots :: Lens.Lens' GetAutoSnapshotsResponse (Lude.Maybe [AutoSnapshotDetails])
gasrsAutoSnapshots = Lens.lens (autoSnapshots :: GetAutoSnapshotsResponse -> Lude.Maybe [AutoSnapshotDetails]) (\s a -> s {autoSnapshots = a} :: GetAutoSnapshotsResponse)
{-# DEPRECATED gasrsAutoSnapshots "Use generic-lens or generic-optics with 'autoSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasrsResponseStatus :: Lens.Lens' GetAutoSnapshotsResponse Lude.Int
gasrsResponseStatus = Lens.lens (responseStatus :: GetAutoSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAutoSnapshotsResponse)
{-# DEPRECATED gasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
