{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster. When you delete a DB cluster, all automated backups for that DB cluster are deleted and can't be recovered. Manual DB cluster snapshots of the specified DB cluster are not deleted.
--
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.DeleteDBCluster
  ( -- * Creating a request
    DeleteDBCluster (..),
    mkDeleteDBCluster,

    -- ** Request lenses
    ddbcFinalDBSnapshotIdentifier,
    ddbcSkipFinalSnapshot,
    ddbcDBClusterIdentifier,

    -- * Destructuring the response
    DeleteDBClusterResponse (..),
    mkDeleteDBClusterResponse,

    -- ** Response lenses
    ddbcrsDBCluster,
    ddbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBCluster' smart constructor.
data DeleteDBCluster = DeleteDBCluster'
  { finalDBSnapshotIdentifier ::
      Lude.Maybe Lude.Text,
    skipFinalSnapshot :: Lude.Maybe Lude.Bool,
    dbClusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing DBClusterIdentifier.
--
--
-- * 'finalDBSnapshotIdentifier' - The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is disabled.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- * 'skipFinalSnapshot' - A value that indicates whether to skip the creation of a final DB cluster snapshot before the DB cluster is deleted. If skip is specified, no DB cluster snapshot is created. If skip isn't specified, a DB cluster snapshot is created before the DB cluster is deleted. By default, skip isn't specified, and the DB cluster snapshot is created. By default, this parameter is disabled.
mkDeleteDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  DeleteDBCluster
mkDeleteDBCluster pDBClusterIdentifier_ =
  DeleteDBCluster'
    { finalDBSnapshotIdentifier = Lude.Nothing,
      skipFinalSnapshot = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is disabled.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'finalDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcFinalDBSnapshotIdentifier :: Lens.Lens' DeleteDBCluster (Lude.Maybe Lude.Text)
ddbcFinalDBSnapshotIdentifier = Lens.lens (finalDBSnapshotIdentifier :: DeleteDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBCluster)
{-# DEPRECATED ddbcFinalDBSnapshotIdentifier "Use generic-lens or generic-optics with 'finalDBSnapshotIdentifier' instead." #-}

-- | A value that indicates whether to skip the creation of a final DB cluster snapshot before the DB cluster is deleted. If skip is specified, no DB cluster snapshot is created. If skip isn't specified, a DB cluster snapshot is created before the DB cluster is deleted. By default, skip isn't specified, and the DB cluster snapshot is created. By default, this parameter is disabled.
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcSkipFinalSnapshot :: Lens.Lens' DeleteDBCluster (Lude.Maybe Lude.Bool)
ddbcSkipFinalSnapshot = Lens.lens (skipFinalSnapshot :: DeleteDBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {skipFinalSnapshot = a} :: DeleteDBCluster)
{-# DEPRECATED ddbcSkipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead." #-}

-- | The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcDBClusterIdentifier :: Lens.Lens' DeleteDBCluster Lude.Text
ddbcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DeleteDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DeleteDBCluster)
{-# DEPRECATED ddbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest DeleteDBCluster where
  type Rs DeleteDBCluster = DeleteDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBClusterResult"
      ( \s h x ->
          DeleteDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBCluster where
  toQuery DeleteDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "FinalDBSnapshotIdentifier" Lude.=: finalDBSnapshotIdentifier,
        "SkipFinalSnapshot" Lude.=: skipFinalSnapshot,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkDeleteDBClusterResponse' smart constructor.
data DeleteDBClusterResponse = DeleteDBClusterResponse'
  { dbCluster ::
      Lude.Maybe DBCluster,
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

-- | Creates a value of 'DeleteDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBClusterResponse
mkDeleteDBClusterResponse pResponseStatus_ =
  DeleteDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrsDBCluster :: Lens.Lens' DeleteDBClusterResponse (Lude.Maybe DBCluster)
ddbcrsDBCluster = Lens.lens (dbCluster :: DeleteDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: DeleteDBClusterResponse)
{-# DEPRECATED ddbcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrsResponseStatus :: Lens.Lens' DeleteDBClusterResponse Lude.Int
ddbcrsResponseStatus = Lens.lens (responseStatus :: DeleteDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBClusterResponse)
{-# DEPRECATED ddbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
