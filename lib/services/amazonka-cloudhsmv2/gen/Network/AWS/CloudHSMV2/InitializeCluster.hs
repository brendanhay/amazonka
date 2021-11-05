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
-- Module      : Amazonka.CloudHSMV2.InitializeCluster
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
module Amazonka.CloudHSMV2.InitializeCluster
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

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInitializeCluster' smart constructor.
data InitializeCluster = InitializeCluster'
  { -- | The identifier (ID) of the cluster that you are claiming. To find the
    -- cluster ID, use DescribeClusters.
    clusterId :: Prelude.Text,
    -- | The cluster certificate issued (signed) by your issuing certificate
    -- authority (CA). The certificate must be in PEM format and can contain a
    -- maximum of 5000 characters.
    signedCert :: Prelude.Text,
    -- | The issuing certificate of the issuing certificate authority (CA) that
    -- issued (signed) the cluster certificate. You must use a self-signed
    -- certificate. The certificate used to sign the HSM CSR must be directly
    -- available, and thus must be the root certificate. The certificate must
    -- be in PEM format and can contain a maximum of 5000 characters.
    trustAnchor :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'signedCert'
  Prelude.Text ->
  -- | 'trustAnchor'
  Prelude.Text ->
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
initializeCluster_clusterId :: Lens.Lens' InitializeCluster Prelude.Text
initializeCluster_clusterId = Lens.lens (\InitializeCluster' {clusterId} -> clusterId) (\s@InitializeCluster' {} a -> s {clusterId = a} :: InitializeCluster)

-- | The cluster certificate issued (signed) by your issuing certificate
-- authority (CA). The certificate must be in PEM format and can contain a
-- maximum of 5000 characters.
initializeCluster_signedCert :: Lens.Lens' InitializeCluster Prelude.Text
initializeCluster_signedCert = Lens.lens (\InitializeCluster' {signedCert} -> signedCert) (\s@InitializeCluster' {} a -> s {signedCert = a} :: InitializeCluster)

-- | The issuing certificate of the issuing certificate authority (CA) that
-- issued (signed) the cluster certificate. You must use a self-signed
-- certificate. The certificate used to sign the HSM CSR must be directly
-- available, and thus must be the root certificate. The certificate must
-- be in PEM format and can contain a maximum of 5000 characters.
initializeCluster_trustAnchor :: Lens.Lens' InitializeCluster Prelude.Text
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
            Prelude.<$> (x Core..?> "StateMessage")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitializeCluster

instance Prelude.NFData InitializeCluster

instance Core.ToHeaders InitializeCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.InitializeCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON InitializeCluster where
  toJSON InitializeCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Core..= clusterId),
            Prelude.Just ("SignedCert" Core..= signedCert),
            Prelude.Just ("TrustAnchor" Core..= trustAnchor)
          ]
      )

instance Core.ToPath InitializeCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery InitializeCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInitializeClusterResponse' smart constructor.
data InitializeClusterResponse = InitializeClusterResponse'
  { -- | A description of the cluster\'s state.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s state.
    state :: Prelude.Maybe ClusterState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  InitializeClusterResponse
newInitializeClusterResponse pHttpStatus_ =
  InitializeClusterResponse'
    { stateMessage =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the cluster\'s state.
initializeClusterResponse_stateMessage :: Lens.Lens' InitializeClusterResponse (Prelude.Maybe Prelude.Text)
initializeClusterResponse_stateMessage = Lens.lens (\InitializeClusterResponse' {stateMessage} -> stateMessage) (\s@InitializeClusterResponse' {} a -> s {stateMessage = a} :: InitializeClusterResponse)

-- | The cluster\'s state.
initializeClusterResponse_state :: Lens.Lens' InitializeClusterResponse (Prelude.Maybe ClusterState)
initializeClusterResponse_state = Lens.lens (\InitializeClusterResponse' {state} -> state) (\s@InitializeClusterResponse' {} a -> s {state = a} :: InitializeClusterResponse)

-- | The response's http status code.
initializeClusterResponse_httpStatus :: Lens.Lens' InitializeClusterResponse Prelude.Int
initializeClusterResponse_httpStatus = Lens.lens (\InitializeClusterResponse' {httpStatus} -> httpStatus) (\s@InitializeClusterResponse' {} a -> s {httpStatus = a} :: InitializeClusterResponse)

instance Prelude.NFData InitializeClusterResponse
