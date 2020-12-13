{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteSnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot copy grant.
module Network.AWS.Redshift.DeleteSnapshotCopyGrant
  ( -- * Creating a request
    DeleteSnapshotCopyGrant (..),
    mkDeleteSnapshotCopyGrant,

    -- ** Request lenses
    dSnapshotCopyGrantName,

    -- * Destructuring the response
    DeleteSnapshotCopyGrantResponse (..),
    mkDeleteSnapshotCopyGrantResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The result of the @DeleteSnapshotCopyGrant@ action.
--
-- /See:/ 'mkDeleteSnapshotCopyGrant' smart constructor.
newtype DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrant'
  { -- | The name of the snapshot copy grant to delete.
    snapshotCopyGrantName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotCopyGrant' with the minimum fields required to make a request.
--
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant to delete.
mkDeleteSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Lude.Text ->
  DeleteSnapshotCopyGrant
mkDeleteSnapshotCopyGrant pSnapshotCopyGrantName_ =
  DeleteSnapshotCopyGrant'
    { snapshotCopyGrantName =
        pSnapshotCopyGrantName_
    }

-- | The name of the snapshot copy grant to delete.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotCopyGrantName :: Lens.Lens' DeleteSnapshotCopyGrant Lude.Text
dSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: DeleteSnapshotCopyGrant -> Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: DeleteSnapshotCopyGrant)
{-# DEPRECATED dSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Lude.AWSRequest DeleteSnapshotCopyGrant where
  type Rs DeleteSnapshotCopyGrant = DeleteSnapshotCopyGrantResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteSnapshotCopyGrantResponse'

instance Lude.ToHeaders DeleteSnapshotCopyGrant where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSnapshotCopyGrant where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshotCopyGrant where
  toQuery DeleteSnapshotCopyGrant' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSnapshotCopyGrant" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotCopyGrantName" Lude.=: snapshotCopyGrantName
      ]

-- | /See:/ 'mkDeleteSnapshotCopyGrantResponse' smart constructor.
data DeleteSnapshotCopyGrantResponse = DeleteSnapshotCopyGrantResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshotCopyGrantResponse' with the minimum fields required to make a request.
mkDeleteSnapshotCopyGrantResponse ::
  DeleteSnapshotCopyGrantResponse
mkDeleteSnapshotCopyGrantResponse =
  DeleteSnapshotCopyGrantResponse'
