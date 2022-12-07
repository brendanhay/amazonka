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
-- Module      : Amazonka.IoTSiteWise.DeleteAccessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access policy that grants the specified identity access to
-- the specified IoT SiteWise Monitor resource. You can use this operation
-- to revoke access to an IoT SiteWise Monitor resource.
module Amazonka.IoTSiteWise.DeleteAccessPolicy
  ( -- * Creating a Request
    DeleteAccessPolicy (..),
    newDeleteAccessPolicy,

    -- * Request Lenses
    deleteAccessPolicy_clientToken,
    deleteAccessPolicy_accessPolicyId,

    -- * Destructuring the Response
    DeleteAccessPolicyResponse (..),
    newDeleteAccessPolicyResponse,

    -- * Response Lenses
    deleteAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessPolicy' smart constructor.
data DeleteAccessPolicy = DeleteAccessPolicy'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the access policy to be deleted.
    accessPolicyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAccessPolicy_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'accessPolicyId', 'deleteAccessPolicy_accessPolicyId' - The ID of the access policy to be deleted.
newDeleteAccessPolicy ::
  -- | 'accessPolicyId'
  Prelude.Text ->
  DeleteAccessPolicy
newDeleteAccessPolicy pAccessPolicyId_ =
  DeleteAccessPolicy'
    { clientToken = Prelude.Nothing,
      accessPolicyId = pAccessPolicyId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deleteAccessPolicy_clientToken :: Lens.Lens' DeleteAccessPolicy (Prelude.Maybe Prelude.Text)
deleteAccessPolicy_clientToken = Lens.lens (\DeleteAccessPolicy' {clientToken} -> clientToken) (\s@DeleteAccessPolicy' {} a -> s {clientToken = a} :: DeleteAccessPolicy)

-- | The ID of the access policy to be deleted.
deleteAccessPolicy_accessPolicyId :: Lens.Lens' DeleteAccessPolicy Prelude.Text
deleteAccessPolicy_accessPolicyId = Lens.lens (\DeleteAccessPolicy' {accessPolicyId} -> accessPolicyId) (\s@DeleteAccessPolicy' {} a -> s {accessPolicyId = a} :: DeleteAccessPolicy)

instance Core.AWSRequest DeleteAccessPolicy where
  type
    AWSResponse DeleteAccessPolicy =
      DeleteAccessPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccessPolicy where
  hashWithSalt _salt DeleteAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` accessPolicyId

instance Prelude.NFData DeleteAccessPolicy where
  rnf DeleteAccessPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf accessPolicyId

instance Data.ToHeaders DeleteAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAccessPolicy where
  toPath DeleteAccessPolicy' {..} =
    Prelude.mconcat
      ["/access-policies/", Data.toBS accessPolicyId]

instance Data.ToQuery DeleteAccessPolicy where
  toQuery DeleteAccessPolicy' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteAccessPolicyResponse' smart constructor.
data DeleteAccessPolicyResponse = DeleteAccessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessPolicyResponse_httpStatus' - The response's http status code.
newDeleteAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessPolicyResponse
newDeleteAccessPolicyResponse pHttpStatus_ =
  DeleteAccessPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccessPolicyResponse_httpStatus :: Lens.Lens' DeleteAccessPolicyResponse Prelude.Int
deleteAccessPolicyResponse_httpStatus = Lens.lens (\DeleteAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessPolicyResponse' {} a -> s {httpStatus = a} :: DeleteAccessPolicyResponse)

instance Prelude.NFData DeleteAccessPolicyResponse where
  rnf DeleteAccessPolicyResponse' {..} =
    Prelude.rnf httpStatus
