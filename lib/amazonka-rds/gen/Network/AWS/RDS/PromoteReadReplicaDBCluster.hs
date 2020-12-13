{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PromoteReadReplicaDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB cluster to a standalone DB cluster.
module Network.AWS.RDS.PromoteReadReplicaDBCluster
  ( -- * Creating a request
    PromoteReadReplicaDBCluster (..),
    mkPromoteReadReplicaDBCluster,

    -- ** Request lenses
    prrdcDBClusterIdentifier,

    -- * Destructuring the response
    PromoteReadReplicaDBClusterResponse (..),
    mkPromoteReadReplicaDBClusterResponse,

    -- ** Response lenses
    prrdcrsDBCluster,
    prrdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkPromoteReadReplicaDBCluster' smart constructor.
newtype PromoteReadReplicaDBCluster = PromoteReadReplicaDBCluster'
  { -- | The identifier of the DB cluster read replica to promote. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DB cluster read replica.
    --
    --
    -- Example: @my-cluster-replica1@
    dbClusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PromoteReadReplicaDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The identifier of the DB cluster read replica to promote. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster read replica.
--
--
-- Example: @my-cluster-replica1@
mkPromoteReadReplicaDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  PromoteReadReplicaDBCluster
mkPromoteReadReplicaDBCluster pDBClusterIdentifier_ =
  PromoteReadReplicaDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | The identifier of the DB cluster read replica to promote. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster read replica.
--
--
-- Example: @my-cluster-replica1@
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdcDBClusterIdentifier :: Lens.Lens' PromoteReadReplicaDBCluster Lude.Text
prrdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: PromoteReadReplicaDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: PromoteReadReplicaDBCluster)
{-# DEPRECATED prrdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest PromoteReadReplicaDBCluster where
  type
    Rs PromoteReadReplicaDBCluster =
      PromoteReadReplicaDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "PromoteReadReplicaDBClusterResult"
      ( \s h x ->
          PromoteReadReplicaDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PromoteReadReplicaDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PromoteReadReplicaDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery PromoteReadReplicaDBCluster where
  toQuery PromoteReadReplicaDBCluster' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PromoteReadReplicaDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkPromoteReadReplicaDBClusterResponse' smart constructor.
data PromoteReadReplicaDBClusterResponse = PromoteReadReplicaDBClusterResponse'
  { dbCluster :: Lude.Maybe DBCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PromoteReadReplicaDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' -
-- * 'responseStatus' - The response status code.
mkPromoteReadReplicaDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PromoteReadReplicaDBClusterResponse
mkPromoteReadReplicaDBClusterResponse pResponseStatus_ =
  PromoteReadReplicaDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdcrsDBCluster :: Lens.Lens' PromoteReadReplicaDBClusterResponse (Lude.Maybe DBCluster)
prrdcrsDBCluster = Lens.lens (dbCluster :: PromoteReadReplicaDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: PromoteReadReplicaDBClusterResponse)
{-# DEPRECATED prrdcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdcrsResponseStatus :: Lens.Lens' PromoteReadReplicaDBClusterResponse Lude.Int
prrdcrsResponseStatus = Lens.lens (responseStatus :: PromoteReadReplicaDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PromoteReadReplicaDBClusterResponse)
{-# DEPRECATED prrdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
