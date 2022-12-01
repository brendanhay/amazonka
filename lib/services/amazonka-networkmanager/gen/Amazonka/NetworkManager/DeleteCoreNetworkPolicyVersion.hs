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
-- Module      : Amazonka.NetworkManager.DeleteCoreNetworkPolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a policy version from a core network. You can\'t delete the
-- current LIVE policy.
module Amazonka.NetworkManager.DeleteCoreNetworkPolicyVersion
  ( -- * Creating a Request
    DeleteCoreNetworkPolicyVersion (..),
    newDeleteCoreNetworkPolicyVersion,

    -- * Request Lenses
    deleteCoreNetworkPolicyVersion_coreNetworkId,
    deleteCoreNetworkPolicyVersion_policyVersionId,

    -- * Destructuring the Response
    DeleteCoreNetworkPolicyVersionResponse (..),
    newDeleteCoreNetworkPolicyVersionResponse,

    -- * Response Lenses
    deleteCoreNetworkPolicyVersionResponse_coreNetworkPolicy,
    deleteCoreNetworkPolicyVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoreNetworkPolicyVersion' smart constructor.
data DeleteCoreNetworkPolicyVersion = DeleteCoreNetworkPolicyVersion'
  { -- | The ID of a core network for the deleted policy.
    coreNetworkId :: Prelude.Text,
    -- | The version ID of the deleted policy.
    policyVersionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreNetworkPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'deleteCoreNetworkPolicyVersion_coreNetworkId' - The ID of a core network for the deleted policy.
--
-- 'policyVersionId', 'deleteCoreNetworkPolicyVersion_policyVersionId' - The version ID of the deleted policy.
newDeleteCoreNetworkPolicyVersion ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Int ->
  DeleteCoreNetworkPolicyVersion
newDeleteCoreNetworkPolicyVersion
  pCoreNetworkId_
  pPolicyVersionId_ =
    DeleteCoreNetworkPolicyVersion'
      { coreNetworkId =
          pCoreNetworkId_,
        policyVersionId = pPolicyVersionId_
      }

-- | The ID of a core network for the deleted policy.
deleteCoreNetworkPolicyVersion_coreNetworkId :: Lens.Lens' DeleteCoreNetworkPolicyVersion Prelude.Text
deleteCoreNetworkPolicyVersion_coreNetworkId = Lens.lens (\DeleteCoreNetworkPolicyVersion' {coreNetworkId} -> coreNetworkId) (\s@DeleteCoreNetworkPolicyVersion' {} a -> s {coreNetworkId = a} :: DeleteCoreNetworkPolicyVersion)

-- | The version ID of the deleted policy.
deleteCoreNetworkPolicyVersion_policyVersionId :: Lens.Lens' DeleteCoreNetworkPolicyVersion Prelude.Int
deleteCoreNetworkPolicyVersion_policyVersionId = Lens.lens (\DeleteCoreNetworkPolicyVersion' {policyVersionId} -> policyVersionId) (\s@DeleteCoreNetworkPolicyVersion' {} a -> s {policyVersionId = a} :: DeleteCoreNetworkPolicyVersion)

instance
  Core.AWSRequest
    DeleteCoreNetworkPolicyVersion
  where
  type
    AWSResponse DeleteCoreNetworkPolicyVersion =
      DeleteCoreNetworkPolicyVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCoreNetworkPolicyVersionResponse'
            Prelude.<$> (x Core..?> "CoreNetworkPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCoreNetworkPolicyVersion
  where
  hashWithSalt
    _salt
    DeleteCoreNetworkPolicyVersion' {..} =
      _salt `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` policyVersionId

instance
  Prelude.NFData
    DeleteCoreNetworkPolicyVersion
  where
  rnf DeleteCoreNetworkPolicyVersion' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf policyVersionId

instance
  Core.ToHeaders
    DeleteCoreNetworkPolicyVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCoreNetworkPolicyVersion where
  toPath DeleteCoreNetworkPolicyVersion' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Core.toBS coreNetworkId,
        "/core-network-policy-versions/",
        Core.toBS policyVersionId
      ]

instance Core.ToQuery DeleteCoreNetworkPolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCoreNetworkPolicyVersionResponse' smart constructor.
data DeleteCoreNetworkPolicyVersionResponse = DeleteCoreNetworkPolicyVersionResponse'
  { -- | Returns information about the deleted policy version.
    coreNetworkPolicy :: Prelude.Maybe CoreNetworkPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreNetworkPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkPolicy', 'deleteCoreNetworkPolicyVersionResponse_coreNetworkPolicy' - Returns information about the deleted policy version.
--
-- 'httpStatus', 'deleteCoreNetworkPolicyVersionResponse_httpStatus' - The response's http status code.
newDeleteCoreNetworkPolicyVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCoreNetworkPolicyVersionResponse
newDeleteCoreNetworkPolicyVersionResponse
  pHttpStatus_ =
    DeleteCoreNetworkPolicyVersionResponse'
      { coreNetworkPolicy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns information about the deleted policy version.
deleteCoreNetworkPolicyVersionResponse_coreNetworkPolicy :: Lens.Lens' DeleteCoreNetworkPolicyVersionResponse (Prelude.Maybe CoreNetworkPolicy)
deleteCoreNetworkPolicyVersionResponse_coreNetworkPolicy = Lens.lens (\DeleteCoreNetworkPolicyVersionResponse' {coreNetworkPolicy} -> coreNetworkPolicy) (\s@DeleteCoreNetworkPolicyVersionResponse' {} a -> s {coreNetworkPolicy = a} :: DeleteCoreNetworkPolicyVersionResponse)

-- | The response's http status code.
deleteCoreNetworkPolicyVersionResponse_httpStatus :: Lens.Lens' DeleteCoreNetworkPolicyVersionResponse Prelude.Int
deleteCoreNetworkPolicyVersionResponse_httpStatus = Lens.lens (\DeleteCoreNetworkPolicyVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteCoreNetworkPolicyVersionResponse' {} a -> s {httpStatus = a} :: DeleteCoreNetworkPolicyVersionResponse)

instance
  Prelude.NFData
    DeleteCoreNetworkPolicyVersionResponse
  where
  rnf DeleteCoreNetworkPolicyVersionResponse' {..} =
    Prelude.rnf coreNetworkPolicy
      `Prelude.seq` Prelude.rnf httpStatus
