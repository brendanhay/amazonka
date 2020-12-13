{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy operation is terminated.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.DeleteDBClusterSnapshot
  ( -- * Creating a request
    DeleteDBClusterSnapshot (..),
    mkDeleteDBClusterSnapshot,

    -- ** Request lenses
    ddcsDBClusterSnapshotIdentifier,

    -- * Destructuring the response
    DeleteDBClusterSnapshotResponse (..),
    mkDeleteDBClusterSnapshotResponse,

    -- ** Response lenses
    ddcsrsDBClusterSnapshot,
    ddcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBClusterSnapshot' smart constructor.
newtype DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
  { -- | The identifier of the DB cluster snapshot to delete.
    --
    -- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
    dbClusterSnapshotIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
mkDeleteDBClusterSnapshot ::
  -- | 'dbClusterSnapshotIdentifier'
  Lude.Text ->
  DeleteDBClusterSnapshot
mkDeleteDBClusterSnapshot pDBClusterSnapshotIdentifier_ =
  DeleteDBClusterSnapshot'
    { dbClusterSnapshotIdentifier =
        pDBClusterSnapshotIdentifier_
    }

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsDBClusterSnapshotIdentifier :: Lens.Lens' DeleteDBClusterSnapshot Lude.Text
ddcsDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: DeleteDBClusterSnapshot -> Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: DeleteDBClusterSnapshot)
{-# DEPRECATED ddcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest DeleteDBClusterSnapshot where
  type Rs DeleteDBClusterSnapshot = DeleteDBClusterSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBClusterSnapshotResult"
      ( \s h x ->
          DeleteDBClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "DBClusterSnapshot")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBClusterSnapshot where
  toQuery DeleteDBClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterSnapshotIdentifier" Lude.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'mkDeleteDBClusterSnapshotResponse' smart constructor.
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Lude.Maybe DBClusterSnapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshot' -
-- * 'responseStatus' - The response status code.
mkDeleteDBClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBClusterSnapshotResponse
mkDeleteDBClusterSnapshotResponse pResponseStatus_ =
  DeleteDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrsDBClusterSnapshot :: Lens.Lens' DeleteDBClusterSnapshotResponse (Lude.Maybe DBClusterSnapshot)
ddcsrsDBClusterSnapshot = Lens.lens (dbClusterSnapshot :: DeleteDBClusterSnapshotResponse -> Lude.Maybe DBClusterSnapshot) (\s a -> s {dbClusterSnapshot = a} :: DeleteDBClusterSnapshotResponse)
{-# DEPRECATED ddcsrsDBClusterSnapshot "Use generic-lens or generic-optics with 'dbClusterSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsrsResponseStatus :: Lens.Lens' DeleteDBClusterSnapshotResponse Lude.Int
ddcsrsResponseStatus = Lens.lens (responseStatus :: DeleteDBClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBClusterSnapshotResponse)
{-# DEPRECATED ddcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
