{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster subnet group.
module Network.AWS.Redshift.DeleteClusterSubnetGroup
  ( -- * Creating a request
    DeleteClusterSubnetGroup (..),
    mkDeleteClusterSubnetGroup,

    -- ** Request lenses
    dcsgClusterSubnetGroupName,

    -- * Destructuring the response
    DeleteClusterSubnetGroupResponse (..),
    mkDeleteClusterSubnetGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteClusterSubnetGroup' smart constructor.
newtype DeleteClusterSubnetGroup = DeleteClusterSubnetGroup'
  { -- | The name of the cluster subnet group name to be deleted.
    clusterSubnetGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterSubnetGroup' with the minimum fields required to make a request.
--
-- * 'clusterSubnetGroupName' - The name of the cluster subnet group name to be deleted.
mkDeleteClusterSubnetGroup ::
  -- | 'clusterSubnetGroupName'
  Lude.Text ->
  DeleteClusterSubnetGroup
mkDeleteClusterSubnetGroup pClusterSubnetGroupName_ =
  DeleteClusterSubnetGroup'
    { clusterSubnetGroupName =
        pClusterSubnetGroupName_
    }

-- | The name of the cluster subnet group name to be deleted.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgClusterSubnetGroupName :: Lens.Lens' DeleteClusterSubnetGroup Lude.Text
dcsgClusterSubnetGroupName = Lens.lens (clusterSubnetGroupName :: DeleteClusterSubnetGroup -> Lude.Text) (\s a -> s {clusterSubnetGroupName = a} :: DeleteClusterSubnetGroup)
{-# DEPRECATED dcsgClusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead." #-}

instance Lude.AWSRequest DeleteClusterSubnetGroup where
  type Rs DeleteClusterSubnetGroup = DeleteClusterSubnetGroupResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteClusterSubnetGroupResponse'

instance Lude.ToHeaders DeleteClusterSubnetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClusterSubnetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClusterSubnetGroup where
  toQuery DeleteClusterSubnetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteClusterSubnetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterSubnetGroupName" Lude.=: clusterSubnetGroupName
      ]

-- | /See:/ 'mkDeleteClusterSubnetGroupResponse' smart constructor.
data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterSubnetGroupResponse' with the minimum fields required to make a request.
mkDeleteClusterSubnetGroupResponse ::
  DeleteClusterSubnetGroupResponse
mkDeleteClusterSubnetGroupResponse =
  DeleteClusterSubnetGroupResponse'
