{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveFromGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Aurora secondary cluster from an Aurora global database cluster. The cluster becomes a standalone cluster with read-write capability instead of being read-only and receiving data from a primary cluster in a different region.
module Network.AWS.RDS.RemoveFromGlobalCluster
  ( -- * Creating a request
    RemoveFromGlobalCluster (..),
    mkRemoveFromGlobalCluster,

    -- ** Request lenses
    rfgcDBClusterIdentifier,
    rfgcGlobalClusterIdentifier,

    -- * Destructuring the response
    RemoveFromGlobalClusterResponse (..),
    mkRemoveFromGlobalClusterResponse,

    -- ** Response lenses
    rfgcrsGlobalCluster,
    rfgcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveFromGlobalCluster' smart constructor.
data RemoveFromGlobalCluster = RemoveFromGlobalCluster'
  { -- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The cluster identifier to detach from the Aurora global database cluster.
    globalClusterIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveFromGlobalCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
-- * 'globalClusterIdentifier' - The cluster identifier to detach from the Aurora global database cluster.
mkRemoveFromGlobalCluster ::
  RemoveFromGlobalCluster
mkRemoveFromGlobalCluster =
  RemoveFromGlobalCluster'
    { dbClusterIdentifier = Lude.Nothing,
      globalClusterIdentifier = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcDBClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Lude.Maybe Lude.Text)
rfgcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RemoveFromGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RemoveFromGlobalCluster)
{-# DEPRECATED rfgcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The cluster identifier to detach from the Aurora global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcGlobalClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Lude.Maybe Lude.Text)
rfgcGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: RemoveFromGlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: RemoveFromGlobalCluster)
{-# DEPRECATED rfgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

instance Lude.AWSRequest RemoveFromGlobalCluster where
  type Rs RemoveFromGlobalCluster = RemoveFromGlobalClusterResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RemoveFromGlobalClusterResult"
      ( \s h x ->
          RemoveFromGlobalClusterResponse'
            Lude.<$> (x Lude..@? "GlobalCluster")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveFromGlobalCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveFromGlobalCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveFromGlobalCluster where
  toQuery RemoveFromGlobalCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveFromGlobalCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DbClusterIdentifier" Lude.=: dbClusterIdentifier,
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier
      ]

-- | /See:/ 'mkRemoveFromGlobalClusterResponse' smart constructor.
data RemoveFromGlobalClusterResponse = RemoveFromGlobalClusterResponse'
  { globalCluster :: Lude.Maybe GlobalCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveFromGlobalClusterResponse' with the minimum fields required to make a request.
--
-- * 'globalCluster' -
-- * 'responseStatus' - The response status code.
mkRemoveFromGlobalClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveFromGlobalClusterResponse
mkRemoveFromGlobalClusterResponse pResponseStatus_ =
  RemoveFromGlobalClusterResponse'
    { globalCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrsGlobalCluster :: Lens.Lens' RemoveFromGlobalClusterResponse (Lude.Maybe GlobalCluster)
rfgcrsGlobalCluster = Lens.lens (globalCluster :: RemoveFromGlobalClusterResponse -> Lude.Maybe GlobalCluster) (\s a -> s {globalCluster = a} :: RemoveFromGlobalClusterResponse)
{-# DEPRECATED rfgcrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrsResponseStatus :: Lens.Lens' RemoveFromGlobalClusterResponse Lude.Int
rfgcrsResponseStatus = Lens.lens (responseStatus :: RemoveFromGlobalClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveFromGlobalClusterResponse)
{-# DEPRECATED rfgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
