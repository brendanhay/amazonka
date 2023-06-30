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
-- Module      : Amazonka.WorkMail.DeleteMailboxPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions granted to a member (user or group).
module Amazonka.WorkMail.DeleteMailboxPermissions
  ( -- * Creating a Request
    DeleteMailboxPermissions (..),
    newDeleteMailboxPermissions,

    -- * Request Lenses
    deleteMailboxPermissions_organizationId,
    deleteMailboxPermissions_entityId,
    deleteMailboxPermissions_granteeId,

    -- * Destructuring the Response
    DeleteMailboxPermissionsResponse (..),
    newDeleteMailboxPermissionsResponse,

    -- * Response Lenses
    deleteMailboxPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteMailboxPermissions' smart constructor.
data DeleteMailboxPermissions = DeleteMailboxPermissions'
  { -- | The identifier of the organization under which the member (user or
    -- group) exists.
    organizationId :: Prelude.Text,
    -- | The identifier of the member (user or group) that owns the mailbox.
    entityId :: Prelude.Text,
    -- | The identifier of the member (user or group) for which to delete granted
    -- permissions.
    granteeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMailboxPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteMailboxPermissions_organizationId' - The identifier of the organization under which the member (user or
-- group) exists.
--
-- 'entityId', 'deleteMailboxPermissions_entityId' - The identifier of the member (user or group) that owns the mailbox.
--
-- 'granteeId', 'deleteMailboxPermissions_granteeId' - The identifier of the member (user or group) for which to delete granted
-- permissions.
newDeleteMailboxPermissions ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'granteeId'
  Prelude.Text ->
  DeleteMailboxPermissions
newDeleteMailboxPermissions
  pOrganizationId_
  pEntityId_
  pGranteeId_ =
    DeleteMailboxPermissions'
      { organizationId =
          pOrganizationId_,
        entityId = pEntityId_,
        granteeId = pGranteeId_
      }

-- | The identifier of the organization under which the member (user or
-- group) exists.
deleteMailboxPermissions_organizationId :: Lens.Lens' DeleteMailboxPermissions Prelude.Text
deleteMailboxPermissions_organizationId = Lens.lens (\DeleteMailboxPermissions' {organizationId} -> organizationId) (\s@DeleteMailboxPermissions' {} a -> s {organizationId = a} :: DeleteMailboxPermissions)

-- | The identifier of the member (user or group) that owns the mailbox.
deleteMailboxPermissions_entityId :: Lens.Lens' DeleteMailboxPermissions Prelude.Text
deleteMailboxPermissions_entityId = Lens.lens (\DeleteMailboxPermissions' {entityId} -> entityId) (\s@DeleteMailboxPermissions' {} a -> s {entityId = a} :: DeleteMailboxPermissions)

-- | The identifier of the member (user or group) for which to delete granted
-- permissions.
deleteMailboxPermissions_granteeId :: Lens.Lens' DeleteMailboxPermissions Prelude.Text
deleteMailboxPermissions_granteeId = Lens.lens (\DeleteMailboxPermissions' {granteeId} -> granteeId) (\s@DeleteMailboxPermissions' {} a -> s {granteeId = a} :: DeleteMailboxPermissions)

instance Core.AWSRequest DeleteMailboxPermissions where
  type
    AWSResponse DeleteMailboxPermissions =
      DeleteMailboxPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMailboxPermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMailboxPermissions where
  hashWithSalt _salt DeleteMailboxPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` granteeId

instance Prelude.NFData DeleteMailboxPermissions where
  rnf DeleteMailboxPermissions' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf granteeId

instance Data.ToHeaders DeleteMailboxPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteMailboxPermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMailboxPermissions where
  toJSON DeleteMailboxPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("GranteeId" Data..= granteeId)
          ]
      )

instance Data.ToPath DeleteMailboxPermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMailboxPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMailboxPermissionsResponse' smart constructor.
data DeleteMailboxPermissionsResponse = DeleteMailboxPermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMailboxPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMailboxPermissionsResponse_httpStatus' - The response's http status code.
newDeleteMailboxPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMailboxPermissionsResponse
newDeleteMailboxPermissionsResponse pHttpStatus_ =
  DeleteMailboxPermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMailboxPermissionsResponse_httpStatus :: Lens.Lens' DeleteMailboxPermissionsResponse Prelude.Int
deleteMailboxPermissionsResponse_httpStatus = Lens.lens (\DeleteMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@DeleteMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: DeleteMailboxPermissionsResponse)

instance
  Prelude.NFData
    DeleteMailboxPermissionsResponse
  where
  rnf DeleteMailboxPermissionsResponse' {..} =
    Prelude.rnf httpStatus
