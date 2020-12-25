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
    icClusterId,
    icSignedCert,
    icTrustAnchor,

    -- * Destructuring the response
    InitializeClusterResponse (..),
    mkInitializeClusterResponse,

    -- ** Response lenses
    icrrsState,
    icrrsStateMessage,
    icrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInitializeCluster' smart constructor.
data InitializeCluster = InitializeCluster'
  { -- | The identifier (ID) of the cluster that you are claiming. To find the cluster ID, use 'DescribeClusters' .
    clusterId :: Types.ClusterId,
    -- | The cluster certificate issued (signed) by your issuing certificate authority (CA). The certificate must be in PEM format and can contain a maximum of 5000 characters.
    signedCert :: Types.Cert,
    -- | The issuing certificate of the issuing certificate authority (CA) that issued (signed) the cluster certificate. You must use a self-signed certificate. The certificate used to sign the HSM CSR must be directly available, and thus must be the root certificate. The certificate must be in PEM format and can contain a maximum of 5000 characters.
    trustAnchor :: Types.Cert
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitializeCluster' value with any optional fields omitted.
mkInitializeCluster ::
  -- | 'clusterId'
  Types.ClusterId ->
  -- | 'signedCert'
  Types.Cert ->
  -- | 'trustAnchor'
  Types.Cert ->
  InitializeCluster
mkInitializeCluster clusterId signedCert trustAnchor =
  InitializeCluster' {clusterId, signedCert, trustAnchor}

-- | The identifier (ID) of the cluster that you are claiming. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icClusterId :: Lens.Lens' InitializeCluster Types.ClusterId
icClusterId = Lens.field @"clusterId"
{-# DEPRECATED icClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The cluster certificate issued (signed) by your issuing certificate authority (CA). The certificate must be in PEM format and can contain a maximum of 5000 characters.
--
-- /Note:/ Consider using 'signedCert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSignedCert :: Lens.Lens' InitializeCluster Types.Cert
icSignedCert = Lens.field @"signedCert"
{-# DEPRECATED icSignedCert "Use generic-lens or generic-optics with 'signedCert' instead." #-}

-- | The issuing certificate of the issuing certificate authority (CA) that issued (signed) the cluster certificate. You must use a self-signed certificate. The certificate used to sign the HSM CSR must be directly available, and thus must be the root certificate. The certificate must be in PEM format and can contain a maximum of 5000 characters.
--
-- /Note:/ Consider using 'trustAnchor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTrustAnchor :: Lens.Lens' InitializeCluster Types.Cert
icTrustAnchor = Lens.field @"trustAnchor"
{-# DEPRECATED icTrustAnchor "Use generic-lens or generic-optics with 'trustAnchor' instead." #-}

instance Core.FromJSON InitializeCluster where
  toJSON InitializeCluster {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("SignedCert" Core..= signedCert),
            Core.Just ("TrustAnchor" Core..= trustAnchor)
          ]
      )

instance Core.AWSRequest InitializeCluster where
  type Rs InitializeCluster = InitializeClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "BaldrApiService.InitializeCluster")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          InitializeClusterResponse'
            Core.<$> (x Core..:? "State")
            Core.<*> (x Core..:? "StateMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkInitializeClusterResponse' smart constructor.
data InitializeClusterResponse = InitializeClusterResponse'
  { -- | The cluster's state.
    state :: Core.Maybe Types.ClusterState,
    -- | A description of the cluster's state.
    stateMessage :: Core.Maybe Types.StateMessage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitializeClusterResponse' value with any optional fields omitted.
mkInitializeClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  InitializeClusterResponse
mkInitializeClusterResponse responseStatus =
  InitializeClusterResponse'
    { state = Core.Nothing,
      stateMessage = Core.Nothing,
      responseStatus
    }

-- | The cluster's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsState :: Lens.Lens' InitializeClusterResponse (Core.Maybe Types.ClusterState)
icrrsState = Lens.field @"state"
{-# DEPRECATED icrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A description of the cluster's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsStateMessage :: Lens.Lens' InitializeClusterResponse (Core.Maybe Types.StateMessage)
icrrsStateMessage = Lens.field @"stateMessage"
{-# DEPRECATED icrrsStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsResponseStatus :: Lens.Lens' InitializeClusterResponse Core.Int
icrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED icrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
