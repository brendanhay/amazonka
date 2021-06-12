{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.InitializeCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Claims an AWS CloudHSM cluster by submitting the cluster certificate
-- issued by your issuing certificate authority (CA) and the CA\'s root
-- certificate. Before you can claim a cluster, you must sign the
-- cluster\'s certificate signing request (CSR) with your issuing CA. To
-- get the cluster\'s CSR, use DescribeClusters.
module Network.AWS.CloudHSMv2.InitializeCluster
  ( -- * Creating a Request
    InitializeCluster (..),
    newInitializeCluster,

    -- * Request Lenses
    initializeCluster_clusterId,
    initializeCluster_signedCert,
    initializeCluster_trustAnchor,

    -- * Destructuring the Response
    InitializeClusterResponse (..),
    newInitializeClusterResponse,

    -- * Response Lenses
    initializeClusterResponse_stateMessage,
    initializeClusterResponse_state,
    initializeClusterResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInitializeCluster' smart constructor.
data InitializeCluster = InitializeCluster'
  { -- | The identifier (ID) of the cluster that you are claiming. To find the
    -- cluster ID, use DescribeClusters.
    clusterId :: Core.Text,
    -- | The cluster certificate issued (signed) by your issuing certificate
    -- authority (CA). The certificate must be in PEM format and can contain a
    -- maximum of 5000 characters.
    signedCert :: Core.Text,
    -- | The issuing certificate of the issuing certificate authority (CA) that
    -- issued (signed) the cluster certificate. You must use a self-signed
    -- certificate. The certificate used to sign the HSM CSR must be directly
    -- available, and thus must be the root certificate. The certificate must
    -- be in PEM format and can contain a maximum of 5000 characters.
    trustAnchor :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitializeCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'initializeCluster_clusterId' - The identifier (ID) of the cluster that you are claiming. To find the
-- cluster ID, use DescribeClusters.
--
-- 'signedCert', 'initializeCluster_signedCert' - The cluster certificate issued (signed) by your issuing certificate
-- authority (CA). The certificate must be in PEM format and can contain a
-- maximum of 5000 characters.
--
-- 'trustAnchor', 'initializeCluster_trustAnchor' - The issuing certificate of the issuing certificate authority (CA) that
-- issued (signed) the cluster certificate. You must use a self-signed
-- certificate. The certificate used to sign the HSM CSR must be directly
-- available, and thus must be the root certificate. The certificate must
-- be in PEM format and can contain a maximum of 5000 characters.
newInitializeCluster ::
  -- | 'clusterId'
  Core.Text ->
  -- | 'signedCert'
  Core.Text ->
  -- | 'trustAnchor'
  Core.Text ->
  InitializeCluster
newInitializeCluster
  pClusterId_
  pSignedCert_
  pTrustAnchor_ =
    InitializeCluster'
      { clusterId = pClusterId_,
        signedCert = pSignedCert_,
        trustAnchor = pTrustAnchor_
      }

-- | The identifier (ID) of the cluster that you are claiming. To find the
-- cluster ID, use DescribeClusters.
initializeCluster_clusterId :: Lens.Lens' InitializeCluster Core.Text
initializeCluster_clusterId = Lens.lens (\InitializeCluster' {clusterId} -> clusterId) (\s@InitializeCluster' {} a -> s {clusterId = a} :: InitializeCluster)

-- | The cluster certificate issued (signed) by your issuing certificate
-- authority (CA). The certificate must be in PEM format and can contain a
-- maximum of 5000 characters.
initializeCluster_signedCert :: Lens.Lens' InitializeCluster Core.Text
initializeCluster_signedCert = Lens.lens (\InitializeCluster' {signedCert} -> signedCert) (\s@InitializeCluster' {} a -> s {signedCert = a} :: InitializeCluster)

-- | The issuing certificate of the issuing certificate authority (CA) that
-- issued (signed) the cluster certificate. You must use a self-signed
-- certificate. The certificate used to sign the HSM CSR must be directly
-- available, and thus must be the root certificate. The certificate must
-- be in PEM format and can contain a maximum of 5000 characters.
initializeCluster_trustAnchor :: Lens.Lens' InitializeCluster Core.Text
initializeCluster_trustAnchor = Lens.lens (\InitializeCluster' {trustAnchor} -> trustAnchor) (\s@InitializeCluster' {} a -> s {trustAnchor = a} :: InitializeCluster)

instance Core.AWSRequest InitializeCluster where
  type
    AWSResponse InitializeCluster =
      InitializeClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InitializeClusterResponse'
            Core.<$> (x Core..?> "StateMessage")
            Core.<*> (x Core..?> "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InitializeCluster

instance Core.NFData InitializeCluster

instance Core.ToHeaders InitializeCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.InitializeCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON InitializeCluster where
  toJSON InitializeCluster' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("SignedCert" Core..= signedCert),
            Core.Just ("TrustAnchor" Core..= trustAnchor)
          ]
      )

instance Core.ToPath InitializeCluster where
  toPath = Core.const "/"

instance Core.ToQuery InitializeCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newInitializeClusterResponse' smart constructor.
data InitializeClusterResponse = InitializeClusterResponse'
  { -- | A description of the cluster\'s state.
    stateMessage :: Core.Maybe Core.Text,
    -- | The cluster\'s state.
    state :: Core.Maybe ClusterState,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitializeClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMessage', 'initializeClusterResponse_stateMessage' - A description of the cluster\'s state.
--
-- 'state', 'initializeClusterResponse_state' - The cluster\'s state.
--
-- 'httpStatus', 'initializeClusterResponse_httpStatus' - The response's http status code.
newInitializeClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InitializeClusterResponse
newInitializeClusterResponse pHttpStatus_ =
  InitializeClusterResponse'
    { stateMessage =
        Core.Nothing,
      state = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the cluster\'s state.
initializeClusterResponse_stateMessage :: Lens.Lens' InitializeClusterResponse (Core.Maybe Core.Text)
initializeClusterResponse_stateMessage = Lens.lens (\InitializeClusterResponse' {stateMessage} -> stateMessage) (\s@InitializeClusterResponse' {} a -> s {stateMessage = a} :: InitializeClusterResponse)

-- | The cluster\'s state.
initializeClusterResponse_state :: Lens.Lens' InitializeClusterResponse (Core.Maybe ClusterState)
initializeClusterResponse_state = Lens.lens (\InitializeClusterResponse' {state} -> state) (\s@InitializeClusterResponse' {} a -> s {state = a} :: InitializeClusterResponse)

-- | The response's http status code.
initializeClusterResponse_httpStatus :: Lens.Lens' InitializeClusterResponse Core.Int
initializeClusterResponse_httpStatus = Lens.lens (\InitializeClusterResponse' {httpStatus} -> httpStatus) (\s@InitializeClusterResponse' {} a -> s {httpStatus = a} :: InitializeClusterResponse)

instance Core.NFData InitializeClusterResponse
