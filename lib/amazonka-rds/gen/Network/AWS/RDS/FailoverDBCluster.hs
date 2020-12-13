{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.FailoverDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forces a failover for a DB cluster.
--
-- A failover for a DB cluster promotes one of the Aurora Replicas (read-only instances) in the DB cluster to be the primary instance (the cluster writer).
-- Amazon Aurora will automatically fail over to an Aurora Replica, if one exists, when the primary instance fails. You can force a failover when you want to simulate a failure of a primary instance for testing. Because each instance in a DB cluster has its own endpoint address, you will need to clean up and re-establish any existing connections that use those endpoint addresses when the failover is complete.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.FailoverDBCluster
  ( -- * Creating a request
    FailoverDBCluster (..),
    mkFailoverDBCluster,

    -- ** Request lenses
    fdcDBClusterIdentifier,
    fdcTargetDBInstanceIdentifier,

    -- * Destructuring the response
    FailoverDBClusterResponse (..),
    mkFailoverDBClusterResponse,

    -- ** Response lenses
    fdcrsDBCluster,
    fdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkFailoverDBCluster' smart constructor.
data FailoverDBCluster = FailoverDBCluster'
  { -- | A DB cluster identifier to force a failover for. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Lude.Text,
    -- | The name of the instance to promote to the primary instance.
    --
    -- You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
    targetDBInstanceIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - A DB cluster identifier to force a failover for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- * 'targetDBInstanceIdentifier' - The name of the instance to promote to the primary instance.
--
-- You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
mkFailoverDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  FailoverDBCluster
mkFailoverDBCluster pDBClusterIdentifier_ =
  FailoverDBCluster'
    { dbClusterIdentifier = pDBClusterIdentifier_,
      targetDBInstanceIdentifier = Lude.Nothing
    }

-- | A DB cluster identifier to force a failover for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcDBClusterIdentifier :: Lens.Lens' FailoverDBCluster Lude.Text
fdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: FailoverDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: FailoverDBCluster)
{-# DEPRECATED fdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The name of the instance to promote to the primary instance.
--
-- You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
--
-- /Note:/ Consider using 'targetDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcTargetDBInstanceIdentifier :: Lens.Lens' FailoverDBCluster (Lude.Maybe Lude.Text)
fdcTargetDBInstanceIdentifier = Lens.lens (targetDBInstanceIdentifier :: FailoverDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {targetDBInstanceIdentifier = a} :: FailoverDBCluster)
{-# DEPRECATED fdcTargetDBInstanceIdentifier "Use generic-lens or generic-optics with 'targetDBInstanceIdentifier' instead." #-}

instance Lude.AWSRequest FailoverDBCluster where
  type Rs FailoverDBCluster = FailoverDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "FailoverDBClusterResult"
      ( \s h x ->
          FailoverDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders FailoverDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath FailoverDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery FailoverDBCluster where
  toQuery FailoverDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("FailoverDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "TargetDBInstanceIdentifier" Lude.=: targetDBInstanceIdentifier
      ]

-- | /See:/ 'mkFailoverDBClusterResponse' smart constructor.
data FailoverDBClusterResponse = FailoverDBClusterResponse'
  { dbCluster :: Lude.Maybe DBCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' -
-- * 'responseStatus' - The response status code.
mkFailoverDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  FailoverDBClusterResponse
mkFailoverDBClusterResponse pResponseStatus_ =
  FailoverDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcrsDBCluster :: Lens.Lens' FailoverDBClusterResponse (Lude.Maybe DBCluster)
fdcrsDBCluster = Lens.lens (dbCluster :: FailoverDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: FailoverDBClusterResponse)
{-# DEPRECATED fdcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcrsResponseStatus :: Lens.Lens' FailoverDBClusterResponse Lude.Int
fdcrsResponseStatus = Lens.lens (responseStatus :: FailoverDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: FailoverDBClusterResponse)
{-# DEPRECATED fdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
