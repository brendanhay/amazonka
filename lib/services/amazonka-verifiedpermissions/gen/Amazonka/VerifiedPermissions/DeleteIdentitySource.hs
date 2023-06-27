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
-- Module      : Amazonka.VerifiedPermissions.DeleteIdentitySource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity source that references an identity provider (IdP)
-- such as Amazon Cognito. After you delete the identity source, you can no
-- longer use tokens for identities from that identity source to represent
-- principals in authorization queries made using
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>.
-- operations.
module Amazonka.VerifiedPermissions.DeleteIdentitySource
  ( -- * Creating a Request
    DeleteIdentitySource (..),
    newDeleteIdentitySource,

    -- * Request Lenses
    deleteIdentitySource_policyStoreId,
    deleteIdentitySource_identitySourceId,

    -- * Destructuring the Response
    DeleteIdentitySourceResponse (..),
    newDeleteIdentitySourceResponse,

    -- * Response Lenses
    deleteIdentitySourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newDeleteIdentitySource' smart constructor.
data DeleteIdentitySource = DeleteIdentitySource'
  { -- | Specifies the ID of the policy store that contains the identity source
    -- that you want to delete.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the identity source that you want to delete.
    identitySourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentitySource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'deleteIdentitySource_policyStoreId' - Specifies the ID of the policy store that contains the identity source
-- that you want to delete.
--
-- 'identitySourceId', 'deleteIdentitySource_identitySourceId' - Specifies the ID of the identity source that you want to delete.
newDeleteIdentitySource ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'identitySourceId'
  Prelude.Text ->
  DeleteIdentitySource
newDeleteIdentitySource
  pPolicyStoreId_
  pIdentitySourceId_ =
    DeleteIdentitySource'
      { policyStoreId =
          pPolicyStoreId_,
        identitySourceId = pIdentitySourceId_
      }

-- | Specifies the ID of the policy store that contains the identity source
-- that you want to delete.
deleteIdentitySource_policyStoreId :: Lens.Lens' DeleteIdentitySource Prelude.Text
deleteIdentitySource_policyStoreId = Lens.lens (\DeleteIdentitySource' {policyStoreId} -> policyStoreId) (\s@DeleteIdentitySource' {} a -> s {policyStoreId = a} :: DeleteIdentitySource)

-- | Specifies the ID of the identity source that you want to delete.
deleteIdentitySource_identitySourceId :: Lens.Lens' DeleteIdentitySource Prelude.Text
deleteIdentitySource_identitySourceId = Lens.lens (\DeleteIdentitySource' {identitySourceId} -> identitySourceId) (\s@DeleteIdentitySource' {} a -> s {identitySourceId = a} :: DeleteIdentitySource)

instance Core.AWSRequest DeleteIdentitySource where
  type
    AWSResponse DeleteIdentitySource =
      DeleteIdentitySourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIdentitySourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIdentitySource where
  hashWithSalt _salt DeleteIdentitySource' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` identitySourceId

instance Prelude.NFData DeleteIdentitySource where
  rnf DeleteIdentitySource' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf identitySourceId

instance Data.ToHeaders DeleteIdentitySource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.DeleteIdentitySource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIdentitySource where
  toJSON DeleteIdentitySource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("identitySourceId" Data..= identitySourceId)
          ]
      )

instance Data.ToPath DeleteIdentitySource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIdentitySource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIdentitySourceResponse' smart constructor.
data DeleteIdentitySourceResponse = DeleteIdentitySourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentitySourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIdentitySourceResponse_httpStatus' - The response's http status code.
newDeleteIdentitySourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIdentitySourceResponse
newDeleteIdentitySourceResponse pHttpStatus_ =
  DeleteIdentitySourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteIdentitySourceResponse_httpStatus :: Lens.Lens' DeleteIdentitySourceResponse Prelude.Int
deleteIdentitySourceResponse_httpStatus = Lens.lens (\DeleteIdentitySourceResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentitySourceResponse' {} a -> s {httpStatus = a} :: DeleteIdentitySourceResponse)

instance Prelude.NFData DeleteIdentitySourceResponse where
  rnf DeleteIdentitySourceResponse' {..} =
    Prelude.rnf httpStatus
