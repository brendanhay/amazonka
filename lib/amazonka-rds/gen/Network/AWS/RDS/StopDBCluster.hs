{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon Aurora DB cluster. When you stop a DB cluster, Aurora retains the DB cluster's metadata, including its endpoints and DB parameter groups. Aurora also retains the transaction logs so you can do a point-in-time restore if necessary.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.StopDBCluster
  ( -- * Creating a request
    StopDBCluster (..),
    mkStopDBCluster,

    -- ** Request lenses
    sdbcDBClusterIdentifier,

    -- * Destructuring the response
    StopDBClusterResponse (..),
    mkStopDBClusterResponse,

    -- ** Response lenses
    sdbcrsDBCluster,
    sdbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopDBCluster' smart constructor.
newtype StopDBCluster = StopDBCluster'
  { dbClusterIdentifier ::
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

-- | Creates a value of 'StopDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
mkStopDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  StopDBCluster
mkStopDBCluster pDBClusterIdentifier_ =
  StopDBCluster' {dbClusterIdentifier = pDBClusterIdentifier_}

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcDBClusterIdentifier :: Lens.Lens' StopDBCluster Lude.Text
sdbcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: StopDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: StopDBCluster)
{-# DEPRECATED sdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest StopDBCluster where
  type Rs StopDBCluster = StopDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StopDBClusterResult"
      ( \s h x ->
          StopDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StopDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDBCluster where
  toQuery StopDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StopDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkStopDBClusterResponse' smart constructor.
data StopDBClusterResponse = StopDBClusterResponse'
  { dbCluster ::
      Lude.Maybe DBCluster,
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

-- | Creates a value of 'StopDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkStopDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDBClusterResponse
mkStopDBClusterResponse pResponseStatus_ =
  StopDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrsDBCluster :: Lens.Lens' StopDBClusterResponse (Lude.Maybe DBCluster)
sdbcrsDBCluster = Lens.lens (dbCluster :: StopDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: StopDBClusterResponse)
{-# DEPRECATED sdbcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrsResponseStatus :: Lens.Lens' StopDBClusterResponse Lude.Int
sdbcrsResponseStatus = Lens.lens (responseStatus :: StopDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDBClusterResponse)
{-# DEPRECATED sdbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
