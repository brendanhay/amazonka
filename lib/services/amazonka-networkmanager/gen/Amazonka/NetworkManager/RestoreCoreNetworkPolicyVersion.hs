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
-- Module      : Amazonka.NetworkManager.RestoreCoreNetworkPolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a previous policy version as a new, immutable version of a core
-- network policy. A subsequent change set is created showing the
-- differences between the LIVE policy and restored policy.
module Amazonka.NetworkManager.RestoreCoreNetworkPolicyVersion
  ( -- * Creating a Request
    RestoreCoreNetworkPolicyVersion (..),
    newRestoreCoreNetworkPolicyVersion,

    -- * Request Lenses
    restoreCoreNetworkPolicyVersion_coreNetworkId,
    restoreCoreNetworkPolicyVersion_policyVersionId,

    -- * Destructuring the Response
    RestoreCoreNetworkPolicyVersionResponse (..),
    newRestoreCoreNetworkPolicyVersionResponse,

    -- * Response Lenses
    restoreCoreNetworkPolicyVersionResponse_coreNetworkPolicy,
    restoreCoreNetworkPolicyVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreCoreNetworkPolicyVersion' smart constructor.
data RestoreCoreNetworkPolicyVersion = RestoreCoreNetworkPolicyVersion'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Text,
    -- | The ID of the policy version to restore.
    policyVersionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreCoreNetworkPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'restoreCoreNetworkPolicyVersion_coreNetworkId' - The ID of a core network.
--
-- 'policyVersionId', 'restoreCoreNetworkPolicyVersion_policyVersionId' - The ID of the policy version to restore.
newRestoreCoreNetworkPolicyVersion ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Int ->
  RestoreCoreNetworkPolicyVersion
newRestoreCoreNetworkPolicyVersion
  pCoreNetworkId_
  pPolicyVersionId_ =
    RestoreCoreNetworkPolicyVersion'
      { coreNetworkId =
          pCoreNetworkId_,
        policyVersionId = pPolicyVersionId_
      }

-- | The ID of a core network.
restoreCoreNetworkPolicyVersion_coreNetworkId :: Lens.Lens' RestoreCoreNetworkPolicyVersion Prelude.Text
restoreCoreNetworkPolicyVersion_coreNetworkId = Lens.lens (\RestoreCoreNetworkPolicyVersion' {coreNetworkId} -> coreNetworkId) (\s@RestoreCoreNetworkPolicyVersion' {} a -> s {coreNetworkId = a} :: RestoreCoreNetworkPolicyVersion)

-- | The ID of the policy version to restore.
restoreCoreNetworkPolicyVersion_policyVersionId :: Lens.Lens' RestoreCoreNetworkPolicyVersion Prelude.Int
restoreCoreNetworkPolicyVersion_policyVersionId = Lens.lens (\RestoreCoreNetworkPolicyVersion' {policyVersionId} -> policyVersionId) (\s@RestoreCoreNetworkPolicyVersion' {} a -> s {policyVersionId = a} :: RestoreCoreNetworkPolicyVersion)

instance
  Core.AWSRequest
    RestoreCoreNetworkPolicyVersion
  where
  type
    AWSResponse RestoreCoreNetworkPolicyVersion =
      RestoreCoreNetworkPolicyVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreCoreNetworkPolicyVersionResponse'
            Prelude.<$> (x Data..?> "CoreNetworkPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreCoreNetworkPolicyVersion
  where
  hashWithSalt
    _salt
    RestoreCoreNetworkPolicyVersion' {..} =
      _salt `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` policyVersionId

instance
  Prelude.NFData
    RestoreCoreNetworkPolicyVersion
  where
  rnf RestoreCoreNetworkPolicyVersion' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf policyVersionId

instance
  Data.ToHeaders
    RestoreCoreNetworkPolicyVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreCoreNetworkPolicyVersion where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RestoreCoreNetworkPolicyVersion where
  toPath RestoreCoreNetworkPolicyVersion' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-policy-versions/",
        Data.toBS policyVersionId,
        "/restore"
      ]

instance Data.ToQuery RestoreCoreNetworkPolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreCoreNetworkPolicyVersionResponse' smart constructor.
data RestoreCoreNetworkPolicyVersionResponse = RestoreCoreNetworkPolicyVersionResponse'
  { -- | Describes the restored core network policy.
    coreNetworkPolicy :: Prelude.Maybe CoreNetworkPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreCoreNetworkPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkPolicy', 'restoreCoreNetworkPolicyVersionResponse_coreNetworkPolicy' - Describes the restored core network policy.
--
-- 'httpStatus', 'restoreCoreNetworkPolicyVersionResponse_httpStatus' - The response's http status code.
newRestoreCoreNetworkPolicyVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreCoreNetworkPolicyVersionResponse
newRestoreCoreNetworkPolicyVersionResponse
  pHttpStatus_ =
    RestoreCoreNetworkPolicyVersionResponse'
      { coreNetworkPolicy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Describes the restored core network policy.
restoreCoreNetworkPolicyVersionResponse_coreNetworkPolicy :: Lens.Lens' RestoreCoreNetworkPolicyVersionResponse (Prelude.Maybe CoreNetworkPolicy)
restoreCoreNetworkPolicyVersionResponse_coreNetworkPolicy = Lens.lens (\RestoreCoreNetworkPolicyVersionResponse' {coreNetworkPolicy} -> coreNetworkPolicy) (\s@RestoreCoreNetworkPolicyVersionResponse' {} a -> s {coreNetworkPolicy = a} :: RestoreCoreNetworkPolicyVersionResponse)

-- | The response's http status code.
restoreCoreNetworkPolicyVersionResponse_httpStatus :: Lens.Lens' RestoreCoreNetworkPolicyVersionResponse Prelude.Int
restoreCoreNetworkPolicyVersionResponse_httpStatus = Lens.lens (\RestoreCoreNetworkPolicyVersionResponse' {httpStatus} -> httpStatus) (\s@RestoreCoreNetworkPolicyVersionResponse' {} a -> s {httpStatus = a} :: RestoreCoreNetworkPolicyVersionResponse)

instance
  Prelude.NFData
    RestoreCoreNetworkPolicyVersionResponse
  where
  rnf RestoreCoreNetworkPolicyVersionResponse' {..} =
    Prelude.rnf coreNetworkPolicy
      `Prelude.seq` Prelude.rnf httpStatus
