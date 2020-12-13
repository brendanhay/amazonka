{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon Aurora DB cluster that was stopped using the AWS console, the stop-db-cluster AWS CLI command, or the StopDBCluster action.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.StartDBCluster
  ( -- * Creating a request
    StartDBCluster (..),
    mkStartDBCluster,

    -- ** Request lenses
    sdbcDBClusterIdentifier,

    -- * Destructuring the response
    StartDBClusterResponse (..),
    mkStartDBClusterResponse,

    -- ** Response lenses
    sdcrsDBCluster,
    sdcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartDBCluster' smart constructor.
newtype StartDBCluster = StartDBCluster'
  { -- | The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
mkStartDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  StartDBCluster
mkStartDBCluster pDBClusterIdentifier_ =
  StartDBCluster' {dbClusterIdentifier = pDBClusterIdentifier_}

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcDBClusterIdentifier :: Lens.Lens' StartDBCluster Lude.Text
sdbcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: StartDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: StartDBCluster)
{-# DEPRECATED sdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest StartDBCluster where
  type Rs StartDBCluster = StartDBClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StartDBClusterResult"
      ( \s h x ->
          StartDBClusterResponse'
            Lude.<$> (x Lude..@? "DBCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery StartDBCluster where
  toQuery StartDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkStartDBClusterResponse' smart constructor.
data StartDBClusterResponse = StartDBClusterResponse'
  { dbCluster :: Lude.Maybe DBCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartDBClusterResponse' with the minimum fields required to make a request.
--
-- * 'dbCluster' -
-- * 'responseStatus' - The response status code.
mkStartDBClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartDBClusterResponse
mkStartDBClusterResponse pResponseStatus_ =
  StartDBClusterResponse'
    { dbCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcrsDBCluster :: Lens.Lens' StartDBClusterResponse (Lude.Maybe DBCluster)
sdcrsDBCluster = Lens.lens (dbCluster :: StartDBClusterResponse -> Lude.Maybe DBCluster) (\s a -> s {dbCluster = a} :: StartDBClusterResponse)
{-# DEPRECATED sdcrsDBCluster "Use generic-lens or generic-optics with 'dbCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcrsResponseStatus :: Lens.Lens' StartDBClusterResponse Lude.Int
sdcrsResponseStatus = Lens.lens (responseStatus :: StartDBClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartDBClusterResponse)
{-# DEPRECATED sdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
