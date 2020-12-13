{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshots for the specified WorkSpace.
module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
  ( -- * Creating a request
    DescribeWorkspaceSnapshots (..),
    mkDescribeWorkspaceSnapshots,

    -- ** Request lenses
    dwsWorkspaceId,

    -- * Destructuring the response
    DescribeWorkspaceSnapshotsResponse (..),
    mkDescribeWorkspaceSnapshotsResponse,

    -- ** Response lenses
    dwsrsRestoreSnapshots,
    dwsrsRebuildSnapshots,
    dwsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeWorkspaceSnapshots' smart constructor.
newtype DescribeWorkspaceSnapshots = DescribeWorkspaceSnapshots'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceSnapshots' with the minimum fields required to make a request.
--
-- * 'workspaceId' - The identifier of the WorkSpace.
mkDescribeWorkspaceSnapshots ::
  -- | 'workspaceId'
  Lude.Text ->
  DescribeWorkspaceSnapshots
mkDescribeWorkspaceSnapshots pWorkspaceId_ =
  DescribeWorkspaceSnapshots' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsWorkspaceId :: Lens.Lens' DescribeWorkspaceSnapshots Lude.Text
dwsWorkspaceId = Lens.lens (workspaceId :: DescribeWorkspaceSnapshots -> Lude.Text) (\s a -> s {workspaceId = a} :: DescribeWorkspaceSnapshots)
{-# DEPRECATED dwsWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Lude.AWSRequest DescribeWorkspaceSnapshots where
  type
    Rs DescribeWorkspaceSnapshots =
      DescribeWorkspaceSnapshotsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkspaceSnapshotsResponse'
            Lude.<$> (x Lude..?> "RestoreSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "RebuildSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkspaceSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.DescribeWorkspaceSnapshots" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkspaceSnapshots where
  toJSON DescribeWorkspaceSnapshots' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WorkspaceId" Lude..= workspaceId)])

instance Lude.ToPath DescribeWorkspaceSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkspaceSnapshots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeWorkspaceSnapshotsResponse' smart constructor.
data DescribeWorkspaceSnapshotsResponse = DescribeWorkspaceSnapshotsResponse'
  { -- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
    restoreSnapshots :: Lude.Maybe [Snapshot],
    -- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
    rebuildSnapshots :: Lude.Maybe [Snapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkspaceSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'restoreSnapshots' - Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
-- * 'rebuildSnapshots' - Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
-- * 'responseStatus' - The response status code.
mkDescribeWorkspaceSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkspaceSnapshotsResponse
mkDescribeWorkspaceSnapshotsResponse pResponseStatus_ =
  DescribeWorkspaceSnapshotsResponse'
    { restoreSnapshots =
        Lude.Nothing,
      rebuildSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
--
-- /Note:/ Consider using 'restoreSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsRestoreSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Lude.Maybe [Snapshot])
dwsrsRestoreSnapshots = Lens.lens (restoreSnapshots :: DescribeWorkspaceSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {restoreSnapshots = a} :: DescribeWorkspaceSnapshotsResponse)
{-# DEPRECATED dwsrsRestoreSnapshots "Use generic-lens or generic-optics with 'restoreSnapshots' instead." #-}

-- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
--
-- /Note:/ Consider using 'rebuildSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsRebuildSnapshots :: Lens.Lens' DescribeWorkspaceSnapshotsResponse (Lude.Maybe [Snapshot])
dwsrsRebuildSnapshots = Lens.lens (rebuildSnapshots :: DescribeWorkspaceSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {rebuildSnapshots = a} :: DescribeWorkspaceSnapshotsResponse)
{-# DEPRECATED dwsrsRebuildSnapshots "Use generic-lens or generic-optics with 'rebuildSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwsrsResponseStatus :: Lens.Lens' DescribeWorkspaceSnapshotsResponse Lude.Int
dwsrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkspaceSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkspaceSnapshotsResponse)
{-# DEPRECATED dwsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
