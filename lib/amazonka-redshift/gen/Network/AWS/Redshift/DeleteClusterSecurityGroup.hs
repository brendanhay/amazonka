{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.DeleteClusterSecurityGroup
  ( -- * Creating a request
    DeleteClusterSecurityGroup (..),
    mkDeleteClusterSecurityGroup,

    -- ** Request lenses
    dClusterSecurityGroupName,

    -- * Destructuring the response
    DeleteClusterSecurityGroupResponse (..),
    mkDeleteClusterSecurityGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteClusterSecurityGroup' smart constructor.
newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
  { clusterSecurityGroupName ::
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

-- | Creates a value of 'DeleteClusterSecurityGroup' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroupName' - The name of the cluster security group to be deleted.
mkDeleteClusterSecurityGroup ::
  -- | 'clusterSecurityGroupName'
  Lude.Text ->
  DeleteClusterSecurityGroup
mkDeleteClusterSecurityGroup pClusterSecurityGroupName_ =
  DeleteClusterSecurityGroup'
    { clusterSecurityGroupName =
        pClusterSecurityGroupName_
    }

-- | The name of the cluster security group to be deleted.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterSecurityGroupName :: Lens.Lens' DeleteClusterSecurityGroup Lude.Text
dClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: DeleteClusterSecurityGroup -> Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: DeleteClusterSecurityGroup)
{-# DEPRECATED dClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

instance Lude.AWSRequest DeleteClusterSecurityGroup where
  type
    Rs DeleteClusterSecurityGroup =
      DeleteClusterSecurityGroupResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteClusterSecurityGroupResponse'

instance Lude.ToHeaders DeleteClusterSecurityGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClusterSecurityGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClusterSecurityGroup where
  toQuery DeleteClusterSecurityGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteClusterSecurityGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterSecurityGroupName" Lude.=: clusterSecurityGroupName
      ]

-- | /See:/ 'mkDeleteClusterSecurityGroupResponse' smart constructor.
data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterSecurityGroupResponse' with the minimum fields required to make a request.
mkDeleteClusterSecurityGroupResponse ::
  DeleteClusterSecurityGroupResponse
mkDeleteClusterSecurityGroupResponse =
  DeleteClusterSecurityGroupResponse'
