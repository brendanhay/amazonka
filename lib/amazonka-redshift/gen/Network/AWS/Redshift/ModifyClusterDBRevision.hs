{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterDBRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the database revision of a cluster. The database revision is a unique revision of the database running in a cluster.
module Network.AWS.Redshift.ModifyClusterDBRevision
  ( -- * Creating a request
    ModifyClusterDBRevision (..),
    mkModifyClusterDBRevision,

    -- ** Request lenses
    mcdrRevisionTarget,
    mcdrClusterIdentifier,

    -- * Destructuring the response
    ModifyClusterDBRevisionResponse (..),
    mkModifyClusterDBRevisionResponse,

    -- ** Response lenses
    mcdrrsCluster,
    mcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyClusterDBRevision' smart constructor.
data ModifyClusterDBRevision = ModifyClusterDBRevision'
  { -- | The identifier of the database revision. You can retrieve this value from the response to the 'DescribeClusterDbRevisions' request.
    revisionTarget :: Lude.Text,
    -- | The unique identifier of a cluster whose database revision you want to modify.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterDBRevision' with the minimum fields required to make a request.
--
-- * 'revisionTarget' - The identifier of the database revision. You can retrieve this value from the response to the 'DescribeClusterDbRevisions' request.
-- * 'clusterIdentifier' - The unique identifier of a cluster whose database revision you want to modify.
--
-- Example: @examplecluster@
mkModifyClusterDBRevision ::
  -- | 'revisionTarget'
  Lude.Text ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  ModifyClusterDBRevision
mkModifyClusterDBRevision pRevisionTarget_ pClusterIdentifier_ =
  ModifyClusterDBRevision'
    { revisionTarget = pRevisionTarget_,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The identifier of the database revision. You can retrieve this value from the response to the 'DescribeClusterDbRevisions' request.
--
-- /Note:/ Consider using 'revisionTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrRevisionTarget :: Lens.Lens' ModifyClusterDBRevision Lude.Text
mcdrRevisionTarget = Lens.lens (revisionTarget :: ModifyClusterDBRevision -> Lude.Text) (\s a -> s {revisionTarget = a} :: ModifyClusterDBRevision)
{-# DEPRECATED mcdrRevisionTarget "Use generic-lens or generic-optics with 'revisionTarget' instead." #-}

-- | The unique identifier of a cluster whose database revision you want to modify.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrClusterIdentifier :: Lens.Lens' ModifyClusterDBRevision Lude.Text
mcdrClusterIdentifier = Lens.lens (clusterIdentifier :: ModifyClusterDBRevision -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifyClusterDBRevision)
{-# DEPRECATED mcdrClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest ModifyClusterDBRevision where
  type Rs ModifyClusterDBRevision = ModifyClusterDBRevisionResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterDbRevisionResult"
      ( \s h x ->
          ModifyClusterDBRevisionResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClusterDBRevision where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterDBRevision where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterDBRevision where
  toQuery ModifyClusterDBRevision' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClusterDbRevision" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "RevisionTarget" Lude.=: revisionTarget,
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkModifyClusterDBRevisionResponse' smart constructor.
data ModifyClusterDBRevisionResponse = ModifyClusterDBRevisionResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterDBRevisionResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkModifyClusterDBRevisionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterDBRevisionResponse
mkModifyClusterDBRevisionResponse pResponseStatus_ =
  ModifyClusterDBRevisionResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrrsCluster :: Lens.Lens' ModifyClusterDBRevisionResponse (Lude.Maybe Cluster)
mcdrrsCluster = Lens.lens (cluster :: ModifyClusterDBRevisionResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifyClusterDBRevisionResponse)
{-# DEPRECATED mcdrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdrrsResponseStatus :: Lens.Lens' ModifyClusterDBRevisionResponse Lude.Int
mcdrrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterDBRevisionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterDBRevisionResponse)
{-# DEPRECATED mcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
