{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.InitializeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Claims an AWS CloudHSM cluster by submitting the cluster certificate issued by your issuing certificate authority (CA) and the CA's root certificate. Before you can claim a cluster, you must sign the cluster's certificate signing request (CSR) with your issuing CA. To get the cluster's CSR, use 'DescribeClusters' .
module Network.AWS.CloudHSMv2.InitializeCluster
  ( -- * Creating a request
    InitializeCluster (..),
    mkInitializeCluster,

    -- ** Request lenses
    icTrustAnchor,
    icClusterId,
    icSignedCert,

    -- * Destructuring the response
    InitializeClusterResponse (..),
    mkInitializeClusterResponse,

    -- ** Response lenses
    icrsStateMessage,
    icrsState,
    icrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInitializeCluster' smart constructor.
data InitializeCluster = InitializeCluster'
  { -- | The issuing certificate of the issuing certificate authority (CA) that issued (signed) the cluster certificate. You must use a self-signed certificate. The certificate used to sign the HSM CSR must be directly available, and thus must be the root certificate. The certificate must be in PEM format and can contain a maximum of 5000 characters.
    trustAnchor :: Lude.Text,
    -- | The identifier (ID) of the cluster that you are claiming. To find the cluster ID, use 'DescribeClusters' .
    clusterId :: Lude.Text,
    -- | The cluster certificate issued (signed) by your issuing certificate authority (CA). The certificate must be in PEM format and can contain a maximum of 5000 characters.
    signedCert :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitializeCluster' with the minimum fields required to make a request.
--
-- * 'trustAnchor' - The issuing certificate of the issuing certificate authority (CA) that issued (signed) the cluster certificate. You must use a self-signed certificate. The certificate used to sign the HSM CSR must be directly available, and thus must be the root certificate. The certificate must be in PEM format and can contain a maximum of 5000 characters.
-- * 'clusterId' - The identifier (ID) of the cluster that you are claiming. To find the cluster ID, use 'DescribeClusters' .
-- * 'signedCert' - The cluster certificate issued (signed) by your issuing certificate authority (CA). The certificate must be in PEM format and can contain a maximum of 5000 characters.
mkInitializeCluster ::
  -- | 'trustAnchor'
  Lude.Text ->
  -- | 'clusterId'
  Lude.Text ->
  -- | 'signedCert'
  Lude.Text ->
  InitializeCluster
mkInitializeCluster pTrustAnchor_ pClusterId_ pSignedCert_ =
  InitializeCluster'
    { trustAnchor = pTrustAnchor_,
      clusterId = pClusterId_,
      signedCert = pSignedCert_
    }

-- | The issuing certificate of the issuing certificate authority (CA) that issued (signed) the cluster certificate. You must use a self-signed certificate. The certificate used to sign the HSM CSR must be directly available, and thus must be the root certificate. The certificate must be in PEM format and can contain a maximum of 5000 characters.
--
-- /Note:/ Consider using 'trustAnchor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTrustAnchor :: Lens.Lens' InitializeCluster Lude.Text
icTrustAnchor = Lens.lens (trustAnchor :: InitializeCluster -> Lude.Text) (\s a -> s {trustAnchor = a} :: InitializeCluster)
{-# DEPRECATED icTrustAnchor "Use generic-lens or generic-optics with 'trustAnchor' instead." #-}

-- | The identifier (ID) of the cluster that you are claiming. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icClusterId :: Lens.Lens' InitializeCluster Lude.Text
icClusterId = Lens.lens (clusterId :: InitializeCluster -> Lude.Text) (\s a -> s {clusterId = a} :: InitializeCluster)
{-# DEPRECATED icClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The cluster certificate issued (signed) by your issuing certificate authority (CA). The certificate must be in PEM format and can contain a maximum of 5000 characters.
--
-- /Note:/ Consider using 'signedCert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSignedCert :: Lens.Lens' InitializeCluster Lude.Text
icSignedCert = Lens.lens (signedCert :: InitializeCluster -> Lude.Text) (\s a -> s {signedCert = a} :: InitializeCluster)
{-# DEPRECATED icSignedCert "Use generic-lens or generic-optics with 'signedCert' instead." #-}

instance Lude.AWSRequest InitializeCluster where
  type Rs InitializeCluster = InitializeClusterResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          InitializeClusterResponse'
            Lude.<$> (x Lude..?> "StateMessage")
            Lude.<*> (x Lude..?> "State")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitializeCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.InitializeCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InitializeCluster where
  toJSON InitializeCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TrustAnchor" Lude..= trustAnchor),
            Lude.Just ("ClusterId" Lude..= clusterId),
            Lude.Just ("SignedCert" Lude..= signedCert)
          ]
      )

instance Lude.ToPath InitializeCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery InitializeCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInitializeClusterResponse' smart constructor.
data InitializeClusterResponse = InitializeClusterResponse'
  { -- | A description of the cluster's state.
    stateMessage :: Lude.Maybe Lude.Text,
    -- | The cluster's state.
    state :: Lude.Maybe ClusterState,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitializeClusterResponse' with the minimum fields required to make a request.
--
-- * 'stateMessage' - A description of the cluster's state.
-- * 'state' - The cluster's state.
-- * 'responseStatus' - The response status code.
mkInitializeClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitializeClusterResponse
mkInitializeClusterResponse pResponseStatus_ =
  InitializeClusterResponse'
    { stateMessage = Lude.Nothing,
      state = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the cluster's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsStateMessage :: Lens.Lens' InitializeClusterResponse (Lude.Maybe Lude.Text)
icrsStateMessage = Lens.lens (stateMessage :: InitializeClusterResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: InitializeClusterResponse)
{-# DEPRECATED icrsStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The cluster's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsState :: Lens.Lens' InitializeClusterResponse (Lude.Maybe ClusterState)
icrsState = Lens.lens (state :: InitializeClusterResponse -> Lude.Maybe ClusterState) (\s a -> s {state = a} :: InitializeClusterResponse)
{-# DEPRECATED icrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsResponseStatus :: Lens.Lens' InitializeClusterResponse Lude.Int
icrsResponseStatus = Lens.lens (responseStatus :: InitializeClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitializeClusterResponse)
{-# DEPRECATED icrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
