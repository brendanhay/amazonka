{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.CreateDBClusterSnapshot
  ( -- * Creating a request
    CreateDBClusterSnapshot (..),
    mkCreateDBClusterSnapshot,

    -- ** Request lenses
    cdcsDBClusterIdentifier,
    cdcsDBClusterSnapshotIdentifier,
    cdcsTags,

    -- * Destructuring the response
    CreateDBClusterSnapshotResponse (..),
    mkCreateDBClusterSnapshotResponse,

    -- ** Response lenses
    cdbcsrsDBClusterSnapshot,
    cdbcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateDBClusterSnapshot' smart constructor.
data CreateDBClusterSnapshot = CreateDBClusterSnapshot'
  { -- | The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBCluster.
    --
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Lude.Text,
    -- | The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @my-cluster1-snapshot1@
    dbClusterSnapshotIdentifier :: Lude.Text,
    -- | The tags to be assigned to the DB cluster snapshot.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- Example: @my-cluster1@
-- * 'dbClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1-snapshot1@
-- * 'tags' - The tags to be assigned to the DB cluster snapshot.
mkCreateDBClusterSnapshot ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'dbClusterSnapshotIdentifier'
  Lude.Text ->
  CreateDBClusterSnapshot
mkCreateDBClusterSnapshot
  pDBClusterIdentifier_
  pDBClusterSnapshotIdentifier_ =
    CreateDBClusterSnapshot'
      { dbClusterIdentifier =
          pDBClusterIdentifier_,
        dbClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_,
        tags = Lude.Nothing
      }

-- | The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcsDBClusterIdentifier :: Lens.Lens' CreateDBClusterSnapshot Lude.Text
cdcsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: CreateDBClusterSnapshot -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: CreateDBClusterSnapshot)
{-# DEPRECATED cdcsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1-snapshot1@
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcsDBClusterSnapshotIdentifier :: Lens.Lens' CreateDBClusterSnapshot Lude.Text
cdcsDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: CreateDBClusterSnapshot -> Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: CreateDBClusterSnapshot)
{-# DEPRECATED cdcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

-- | The tags to be assigned to the DB cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcsTags :: Lens.Lens' CreateDBClusterSnapshot (Lude.Maybe [Tag])
cdcsTags = Lens.lens (tags :: CreateDBClusterSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBClusterSnapshot)
{-# DEPRECATED cdcsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBClusterSnapshot where
  type Rs CreateDBClusterSnapshot = CreateDBClusterSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBClusterSnapshotResult"
      ( \s h x ->
          CreateDBClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "DBClusterSnapshot")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBClusterSnapshot where
  toQuery CreateDBClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "DBClusterSnapshotIdentifier" Lude.=: dbClusterSnapshotIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateDBClusterSnapshotResponse' smart constructor.
data CreateDBClusterSnapshotResponse = CreateDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Lude.Maybe DBClusterSnapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshot' -
-- * 'responseStatus' - The response status code.
mkCreateDBClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBClusterSnapshotResponse
mkCreateDBClusterSnapshotResponse pResponseStatus_ =
  CreateDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrsDBClusterSnapshot :: Lens.Lens' CreateDBClusterSnapshotResponse (Lude.Maybe DBClusterSnapshot)
cdbcsrsDBClusterSnapshot = Lens.lens (dbClusterSnapshot :: CreateDBClusterSnapshotResponse -> Lude.Maybe DBClusterSnapshot) (\s a -> s {dbClusterSnapshot = a} :: CreateDBClusterSnapshotResponse)
{-# DEPRECATED cdbcsrsDBClusterSnapshot "Use generic-lens or generic-optics with 'dbClusterSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrsResponseStatus :: Lens.Lens' CreateDBClusterSnapshotResponse Lude.Int
cdbcsrsResponseStatus = Lens.lens (responseStatus :: CreateDBClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBClusterSnapshotResponse)
{-# DEPRECATED cdbcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
