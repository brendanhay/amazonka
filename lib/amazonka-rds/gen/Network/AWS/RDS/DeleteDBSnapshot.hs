{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB snapshot. If the snapshot is being copied, the copy operation is terminated.
module Network.AWS.RDS.DeleteDBSnapshot
  ( -- * Creating a request
    DeleteDBSnapshot (..),
    mkDeleteDBSnapshot,

    -- ** Request lenses
    ddbsDBSnapshotIdentifier,

    -- * Destructuring the response
    DeleteDBSnapshotResponse (..),
    mkDeleteDBSnapshotResponse,

    -- ** Response lenses
    ddbsrsDBSnapshot,
    ddbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteDBSnapshot' smart constructor.
newtype DeleteDBSnapshot = DeleteDBSnapshot'
  { dbSnapshotIdentifier ::
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

-- | Creates a value of 'DeleteDBSnapshot' with the minimum fields required to make a request.
--
-- * 'dbSnapshotIdentifier' - The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
mkDeleteDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  DeleteDBSnapshot
mkDeleteDBSnapshot pDBSnapshotIdentifier_ =
  DeleteDBSnapshot' {dbSnapshotIdentifier = pDBSnapshotIdentifier_}

-- | The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDBSnapshotIdentifier :: Lens.Lens' DeleteDBSnapshot Lude.Text
ddbsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: DeleteDBSnapshot -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: DeleteDBSnapshot)
{-# DEPRECATED ddbsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest DeleteDBSnapshot where
  type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBSnapshotResult"
      ( \s h x ->
          DeleteDBSnapshotResponse'
            Lude.<$> (x Lude..@? "DBSnapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBSnapshot where
  toQuery DeleteDBSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'mkDeleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { dbSnapshot ::
      Lude.Maybe DBSnapshot,
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

-- | Creates a value of 'DeleteDBSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshot' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteDBSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBSnapshotResponse
mkDeleteDBSnapshotResponse pResponseStatus_ =
  DeleteDBSnapshotResponse'
    { dbSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrsDBSnapshot :: Lens.Lens' DeleteDBSnapshotResponse (Lude.Maybe DBSnapshot)
ddbsrsDBSnapshot = Lens.lens (dbSnapshot :: DeleteDBSnapshotResponse -> Lude.Maybe DBSnapshot) (\s a -> s {dbSnapshot = a} :: DeleteDBSnapshotResponse)
{-# DEPRECATED ddbsrsDBSnapshot "Use generic-lens or generic-optics with 'dbSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrsResponseStatus :: Lens.Lens' DeleteDBSnapshotResponse Lude.Int
ddbsrsResponseStatus = Lens.lens (responseStatus :: DeleteDBSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBSnapshotResponse)
{-# DEPRECATED ddbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
