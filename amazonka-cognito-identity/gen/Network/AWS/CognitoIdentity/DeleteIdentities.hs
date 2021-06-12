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
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes identities from an identity pool. You can specify a list of 1-60
-- identities that you want to delete.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentities
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

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @DeleteIdentities@ action.
--
-- /See:/ 'newDeleteIdentities' smart constructor.
data DeleteIdentities = DeleteIdentities'
  { -- | A list of 1-60 identities that you want to delete.
    identityIdsToDelete :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DeleteIdentities
newDeleteIdentities pIdentityIdsToDelete_ =
  DeleteIdentities'
    { identityIdsToDelete =
        Lens._Coerce Lens.# pIdentityIdsToDelete_
    }

-- | A list of 1-60 identities that you want to delete.
deleteIdentities_identityIdsToDelete :: Lens.Lens' DeleteIdentities (Core.NonEmpty Core.Text)
deleteIdentities_identityIdsToDelete = Lens.lens (\DeleteIdentities' {identityIdsToDelete} -> identityIdsToDelete) (\s@DeleteIdentities' {} a -> s {identityIdsToDelete = a} :: DeleteIdentities) Core.. Lens._Coerce

instance Core.AWSRequest DeleteIdentities where
  type
    AWSResponse DeleteIdentities =
      DeleteIdentitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteIdentitiesResponse'
            Core.<$> ( x Core..?> "UnprocessedIdentityIds"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteIdentities

instance Core.NFData DeleteIdentities

instance Core.ToHeaders DeleteIdentities where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.DeleteIdentities" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteIdentities where
  toJSON DeleteIdentities' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("IdentityIdsToDelete" Core..= identityIdsToDelete)
          ]
      )

instance Core.ToPath DeleteIdentities where
  toPath = Core.const "/"

instance Core.ToQuery DeleteIdentities where
  toQuery = Core.const Core.mempty

-- | Returned in response to a successful @DeleteIdentities@ operation.
--
-- /See:/ 'newDeleteIdentitiesResponse' smart constructor.
data DeleteIdentitiesResponse = DeleteIdentitiesResponse'
  { -- | An array of UnprocessedIdentityId objects, each of which contains an
    -- ErrorCode and IdentityId.
    unprocessedIdentityIds :: Core.Maybe [UnprocessedIdentityId],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteIdentitiesResponse
newDeleteIdentitiesResponse pHttpStatus_ =
  DeleteIdentitiesResponse'
    { unprocessedIdentityIds =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of UnprocessedIdentityId objects, each of which contains an
-- ErrorCode and IdentityId.
deleteIdentitiesResponse_unprocessedIdentityIds :: Lens.Lens' DeleteIdentitiesResponse (Core.Maybe [UnprocessedIdentityId])
deleteIdentitiesResponse_unprocessedIdentityIds = Lens.lens (\DeleteIdentitiesResponse' {unprocessedIdentityIds} -> unprocessedIdentityIds) (\s@DeleteIdentitiesResponse' {} a -> s {unprocessedIdentityIds = a} :: DeleteIdentitiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteIdentitiesResponse_httpStatus :: Lens.Lens' DeleteIdentitiesResponse Core.Int
deleteIdentitiesResponse_httpStatus = Lens.lens (\DeleteIdentitiesResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentitiesResponse' {} a -> s {httpStatus = a} :: DeleteIdentitiesResponse)

instance Core.NFData DeleteIdentitiesResponse
