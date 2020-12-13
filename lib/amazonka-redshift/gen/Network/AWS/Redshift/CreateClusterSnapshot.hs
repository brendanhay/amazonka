{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be in the @available@ state.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateClusterSnapshot
  ( -- * Creating a request
    CreateClusterSnapshot (..),
    mkCreateClusterSnapshot,

    -- ** Request lenses
    ccsManualSnapshotRetentionPeriod,
    ccsSnapshotIdentifier,
    ccsClusterIdentifier,
    ccsTags,

    -- * Destructuring the response
    CreateClusterSnapshotResponse (..),
    mkCreateClusterSnapshotResponse,

    -- ** Response lenses
    ccsfrsSnapshot,
    ccsfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
  { -- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    -- The default value is -1.
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
    --
    -- Constraints:
    --
    --     * Cannot be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 alphanumeric characters or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-snapshot-id@
    snapshotIdentifier :: Lude.Text,
    -- | The cluster identifier for which you want a snapshot.
    clusterIdentifier :: Lude.Text,
    -- | A list of tag instances.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
-- * 'snapshotIdentifier' - A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
-- * 'clusterIdentifier' - The cluster identifier for which you want a snapshot.
-- * 'tags' - A list of tag instances.
mkCreateClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  CreateClusterSnapshot
mkCreateClusterSnapshot pSnapshotIdentifier_ pClusterIdentifier_ =
  CreateClusterSnapshot'
    { manualSnapshotRetentionPeriod =
        Lude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_,
      clusterIdentifier = pClusterIdentifier_,
      tags = Lude.Nothing
    }

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsManualSnapshotRetentionPeriod :: Lens.Lens' CreateClusterSnapshot (Lude.Maybe Lude.Int)
ccsManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: CreateClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: CreateClusterSnapshot)
{-# DEPRECATED ccsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | A unique identifier for the snapshot that you are requesting. This identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-snapshot-id@
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSnapshotIdentifier :: Lens.Lens' CreateClusterSnapshot Lude.Text
ccsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: CreateClusterSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: CreateClusterSnapshot)
{-# DEPRECATED ccsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The cluster identifier for which you want a snapshot.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsClusterIdentifier :: Lens.Lens' CreateClusterSnapshot Lude.Text
ccsClusterIdentifier = Lens.lens (clusterIdentifier :: CreateClusterSnapshot -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: CreateClusterSnapshot)
{-# DEPRECATED ccsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateClusterSnapshot (Lude.Maybe [Tag])
ccsTags = Lens.lens (tags :: CreateClusterSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateClusterSnapshot)
{-# DEPRECATED ccsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateClusterSnapshot where
  type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateClusterSnapshotResult"
      ( \s h x ->
          CreateClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClusterSnapshot where
  toQuery CreateClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
  { snapshot :: Lude.Maybe Snapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'snapshot' -
-- * 'responseStatus' - The response status code.
mkCreateClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterSnapshotResponse
mkCreateClusterSnapshotResponse pResponseStatus_ =
  CreateClusterSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfrsSnapshot :: Lens.Lens' CreateClusterSnapshotResponse (Lude.Maybe Snapshot)
ccsfrsSnapshot = Lens.lens (snapshot :: CreateClusterSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: CreateClusterSnapshotResponse)
{-# DEPRECATED ccsfrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsfrsResponseStatus :: Lens.Lens' CreateClusterSnapshotResponse Lude.Int
ccsfrsResponseStatus = Lens.lens (responseStatus :: CreateClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterSnapshotResponse)
{-# DEPRECATED ccsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
