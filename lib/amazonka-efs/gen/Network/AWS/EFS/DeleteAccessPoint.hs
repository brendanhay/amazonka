{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteAccessPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified access point. After deletion is complete, new clients can no longer connect to the access points. Clients connected to the access point at the time of deletion will continue to function until they terminate their connection.
--
-- This operation requires permissions for the @elasticfilesystem:DeleteAccessPoint@ action.
module Network.AWS.EFS.DeleteAccessPoint
  ( -- * Creating a request
    DeleteAccessPoint (..),
    mkDeleteAccessPoint,

    -- ** Request lenses
    dAccessPointId,

    -- * Destructuring the response
    DeleteAccessPointResponse (..),
    mkDeleteAccessPointResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccessPoint' smart constructor.
newtype DeleteAccessPoint = DeleteAccessPoint'
  { accessPointId ::
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

-- | Creates a value of 'DeleteAccessPoint' with the minimum fields required to make a request.
--
-- * 'accessPointId' - The ID of the access point that you want to delete.
mkDeleteAccessPoint ::
  -- | 'accessPointId'
  Lude.Text ->
  DeleteAccessPoint
mkDeleteAccessPoint pAccessPointId_ =
  DeleteAccessPoint' {accessPointId = pAccessPointId_}

-- | The ID of the access point that you want to delete.
--
-- /Note:/ Consider using 'accessPointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessPointId :: Lens.Lens' DeleteAccessPoint Lude.Text
dAccessPointId = Lens.lens (accessPointId :: DeleteAccessPoint -> Lude.Text) (\s a -> s {accessPointId = a} :: DeleteAccessPoint)
{-# DEPRECATED dAccessPointId "Use generic-lens or generic-optics with 'accessPointId' instead." #-}

instance Lude.AWSRequest DeleteAccessPoint where
  type Rs DeleteAccessPoint = DeleteAccessPointResponse
  request = Req.delete efsService
  response = Res.receiveNull DeleteAccessPointResponse'

instance Lude.ToHeaders DeleteAccessPoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAccessPoint where
  toPath DeleteAccessPoint' {..} =
    Lude.mconcat
      ["/2015-02-01/access-points/", Lude.toBS accessPointId]

instance Lude.ToQuery DeleteAccessPoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAccessPointResponse' smart constructor.
data DeleteAccessPointResponse = DeleteAccessPointResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccessPointResponse' with the minimum fields required to make a request.
mkDeleteAccessPointResponse ::
  DeleteAccessPointResponse
mkDeleteAccessPointResponse = DeleteAccessPointResponse'
