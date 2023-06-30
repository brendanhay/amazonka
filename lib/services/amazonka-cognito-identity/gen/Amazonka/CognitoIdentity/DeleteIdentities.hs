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
-- Module      : Amazonka.CognitoIdentity.DeleteIdentities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes identities from an identity pool. You can specify a list of 1-60
-- identities that you want to delete.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.DeleteIdentities
  ( -- * Creating a Request
    DeleteIdentities (..),
    newDeleteIdentities,

    -- * Request Lenses
    deleteIdentities_identityIdsToDelete,

    -- * Destructuring the Response
    DeleteIdentitiesResponse (..),
    newDeleteIdentitiesResponse,

    -- * Response Lenses
    deleteIdentitiesResponse_unprocessedIdentityIds,
    deleteIdentitiesResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the @DeleteIdentities@ action.
--
-- /See:/ 'newDeleteIdentities' smart constructor.
data DeleteIdentities = DeleteIdentities'
  { -- | A list of 1-60 identities that you want to delete.
    identityIdsToDelete :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityIdsToDelete', 'deleteIdentities_identityIdsToDelete' - A list of 1-60 identities that you want to delete.
newDeleteIdentities ::
  -- | 'identityIdsToDelete'
  Prelude.NonEmpty Prelude.Text ->
  DeleteIdentities
newDeleteIdentities pIdentityIdsToDelete_ =
  DeleteIdentities'
    { identityIdsToDelete =
        Lens.coerced Lens.# pIdentityIdsToDelete_
    }

-- | A list of 1-60 identities that you want to delete.
deleteIdentities_identityIdsToDelete :: Lens.Lens' DeleteIdentities (Prelude.NonEmpty Prelude.Text)
deleteIdentities_identityIdsToDelete = Lens.lens (\DeleteIdentities' {identityIdsToDelete} -> identityIdsToDelete) (\s@DeleteIdentities' {} a -> s {identityIdsToDelete = a} :: DeleteIdentities) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteIdentities where
  type
    AWSResponse DeleteIdentities =
      DeleteIdentitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteIdentitiesResponse'
            Prelude.<$> ( x
                            Data..?> "UnprocessedIdentityIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIdentities where
  hashWithSalt _salt DeleteIdentities' {..} =
    _salt `Prelude.hashWithSalt` identityIdsToDelete

instance Prelude.NFData DeleteIdentities where
  rnf DeleteIdentities' {..} =
    Prelude.rnf identityIdsToDelete

instance Data.ToHeaders DeleteIdentities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.DeleteIdentities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIdentities where
  toJSON DeleteIdentities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityIdsToDelete" Data..= identityIdsToDelete)
          ]
      )

instance Data.ToPath DeleteIdentities where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIdentities where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful @DeleteIdentities@ operation.
--
-- /See:/ 'newDeleteIdentitiesResponse' smart constructor.
data DeleteIdentitiesResponse = DeleteIdentitiesResponse'
  { -- | An array of UnprocessedIdentityId objects, each of which contains an
    -- ErrorCode and IdentityId.
    unprocessedIdentityIds :: Prelude.Maybe [UnprocessedIdentityId],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedIdentityIds', 'deleteIdentitiesResponse_unprocessedIdentityIds' - An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
--
-- 'httpStatus', 'deleteIdentitiesResponse_httpStatus' - The response's http status code.
newDeleteIdentitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIdentitiesResponse
newDeleteIdentitiesResponse pHttpStatus_ =
  DeleteIdentitiesResponse'
    { unprocessedIdentityIds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
deleteIdentitiesResponse_unprocessedIdentityIds :: Lens.Lens' DeleteIdentitiesResponse (Prelude.Maybe [UnprocessedIdentityId])
deleteIdentitiesResponse_unprocessedIdentityIds = Lens.lens (\DeleteIdentitiesResponse' {unprocessedIdentityIds} -> unprocessedIdentityIds) (\s@DeleteIdentitiesResponse' {} a -> s {unprocessedIdentityIds = a} :: DeleteIdentitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteIdentitiesResponse_httpStatus :: Lens.Lens' DeleteIdentitiesResponse Prelude.Int
deleteIdentitiesResponse_httpStatus = Lens.lens (\DeleteIdentitiesResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentitiesResponse' {} a -> s {httpStatus = a} :: DeleteIdentitiesResponse)

instance Prelude.NFData DeleteIdentitiesResponse where
  rnf DeleteIdentitiesResponse' {..} =
    Prelude.rnf unprocessedIdentityIds
      `Prelude.seq` Prelude.rnf httpStatus
