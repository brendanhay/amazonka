{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey
  ( -- * Creating a request
    RotateEncryptionKey (..),
    mkRotateEncryptionKey,

    -- ** Request lenses
    rekClusterIdentifier,

    -- * Destructuring the response
    RotateEncryptionKeyResponse (..),
    mkRotateEncryptionKeyResponse,

    -- ** Response lenses
    rekrsCluster,
    rekrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRotateEncryptionKey' smart constructor.
newtype RotateEncryptionKey = RotateEncryptionKey'
  { -- | The unique identifier of the cluster that you want to rotate the encryption keys for.
    --
    -- Constraints: Must be the name of valid cluster that has encryption enabled.
    clusterIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotateEncryptionKey' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
mkRotateEncryptionKey ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  RotateEncryptionKey
mkRotateEncryptionKey pClusterIdentifier_ =
  RotateEncryptionKey' {clusterIdentifier = pClusterIdentifier_}

-- | The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekClusterIdentifier :: Lens.Lens' RotateEncryptionKey Lude.Text
rekClusterIdentifier = Lens.lens (clusterIdentifier :: RotateEncryptionKey -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RotateEncryptionKey)
{-# DEPRECATED rekClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest RotateEncryptionKey where
  type Rs RotateEncryptionKey = RotateEncryptionKeyResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RotateEncryptionKeyResult"
      ( \s h x ->
          RotateEncryptionKeyResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RotateEncryptionKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RotateEncryptionKey where
  toPath = Lude.const "/"

instance Lude.ToQuery RotateEncryptionKey where
  toQuery RotateEncryptionKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RotateEncryptionKey" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]

-- | /See:/ 'mkRotateEncryptionKeyResponse' smart constructor.
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotateEncryptionKeyResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkRotateEncryptionKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RotateEncryptionKeyResponse
mkRotateEncryptionKeyResponse pResponseStatus_ =
  RotateEncryptionKeyResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrsCluster :: Lens.Lens' RotateEncryptionKeyResponse (Lude.Maybe Cluster)
rekrsCluster = Lens.lens (cluster :: RotateEncryptionKeyResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: RotateEncryptionKeyResponse)
{-# DEPRECATED rekrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrsResponseStatus :: Lens.Lens' RotateEncryptionKeyResponse Lude.Int
rekrsResponseStatus = Lens.lens (responseStatus :: RotateEncryptionKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RotateEncryptionKeyResponse)
{-# DEPRECATED rekrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
