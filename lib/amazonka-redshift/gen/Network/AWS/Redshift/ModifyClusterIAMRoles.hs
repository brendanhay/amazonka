{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterIAMRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
-- A cluster can have up to 10 IAM roles associated at any time.
module Network.AWS.Redshift.ModifyClusterIAMRoles
  ( -- * Creating a request
    ModifyClusterIAMRoles (..),
    mkModifyClusterIAMRoles,

    -- ** Request lenses
    mcirRemoveIAMRoles,
    mcirClusterIdentifier,
    mcirAddIAMRoles,

    -- * Destructuring the response
    ModifyClusterIAMRolesResponse (..),
    mkModifyClusterIAMRolesResponse,

    -- ** Response lenses
    mcirrsCluster,
    mcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyClusterIAMRoles' smart constructor.
data ModifyClusterIAMRoles = ModifyClusterIAMRoles'
  { -- | Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
    removeIAMRoles :: Lude.Maybe [Lude.Text],
    -- | The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
    clusterIdentifier :: Lude.Text,
    -- | Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
    addIAMRoles :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterIAMRoles' with the minimum fields required to make a request.
--
-- * 'removeIAMRoles' - Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
-- * 'clusterIdentifier' - The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
-- * 'addIAMRoles' - Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
mkModifyClusterIAMRoles ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ModifyClusterIAMRoles
mkModifyClusterIAMRoles pClusterIdentifier_ =
  ModifyClusterIAMRoles'
    { removeIAMRoles = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      addIAMRoles = Lude.Nothing
    }

-- | Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
--
-- /Note:/ Consider using 'removeIAMRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirRemoveIAMRoles :: Lens.Lens' ModifyClusterIAMRoles (Lude.Maybe [Lude.Text])
mcirRemoveIAMRoles = Lens.lens (removeIAMRoles :: ModifyClusterIAMRoles -> Lude.Maybe [Lude.Text]) (\s a -> s {removeIAMRoles = a} :: ModifyClusterIAMRoles)
{-# DEPRECATED mcirRemoveIAMRoles "Use generic-lens or generic-optics with 'removeIAMRoles' instead." #-}

-- | The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirClusterIdentifier :: Lens.Lens' ModifyClusterIAMRoles Lude.Text
mcirClusterIdentifier = Lens.lens (clusterIdentifier :: ModifyClusterIAMRoles -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifyClusterIAMRoles)
{-# DEPRECATED mcirClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
--
-- /Note:/ Consider using 'addIAMRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirAddIAMRoles :: Lens.Lens' ModifyClusterIAMRoles (Lude.Maybe [Lude.Text])
mcirAddIAMRoles = Lens.lens (addIAMRoles :: ModifyClusterIAMRoles -> Lude.Maybe [Lude.Text]) (\s a -> s {addIAMRoles = a} :: ModifyClusterIAMRoles)
{-# DEPRECATED mcirAddIAMRoles "Use generic-lens or generic-optics with 'addIAMRoles' instead." #-}

instance Lude.AWSRequest ModifyClusterIAMRoles where
  type Rs ModifyClusterIAMRoles = ModifyClusterIAMRolesResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterIamRolesResult"
      ( \s h x ->
          ModifyClusterIAMRolesResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClusterIAMRoles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterIAMRoles where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterIAMRoles where
  toQuery ModifyClusterIAMRoles' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClusterIamRoles" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "RemoveIamRoles"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "IamRoleArn" Lude.<$> removeIAMRoles),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "AddIamRoles"
          Lude.=: Lude.toQuery (Lude.toQueryList "IamRoleArn" Lude.<$> addIAMRoles)
      ]

-- | /See:/ 'mkModifyClusterIAMRolesResponse' smart constructor.
data ModifyClusterIAMRolesResponse = ModifyClusterIAMRolesResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterIAMRolesResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkModifyClusterIAMRolesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterIAMRolesResponse
mkModifyClusterIAMRolesResponse pResponseStatus_ =
  ModifyClusterIAMRolesResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirrsCluster :: Lens.Lens' ModifyClusterIAMRolesResponse (Lude.Maybe Cluster)
mcirrsCluster = Lens.lens (cluster :: ModifyClusterIAMRolesResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifyClusterIAMRolesResponse)
{-# DEPRECATED mcirrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcirrsResponseStatus :: Lens.Lens' ModifyClusterIAMRolesResponse Lude.Int
mcirrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterIAMRolesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterIAMRolesResponse)
{-# DEPRECATED mcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
