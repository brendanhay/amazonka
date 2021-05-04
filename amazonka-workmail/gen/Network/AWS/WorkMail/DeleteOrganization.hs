{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkMail.DeleteOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon WorkMail organization and all underlying AWS resources
-- managed by Amazon WorkMail as part of the organization. You can choose
-- whether to delete the associated directory. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/remove_organization.html Removing an organization>
-- in the /Amazon WorkMail Administrator Guide/.
module Network.AWS.WorkMail.DeleteOrganization
  ( -- * Creating a Request
    DeleteOrganization (..),
    newDeleteOrganization,

    -- * Request Lenses
    deleteOrganization_clientToken,
    deleteOrganization_organizationId,
    deleteOrganization_deleteDirectory,

    -- * Destructuring the Response
    DeleteOrganizationResponse (..),
    newDeleteOrganizationResponse,

    -- * Response Lenses
    deleteOrganizationResponse_organizationId,
    deleteOrganizationResponse_state,
    deleteOrganizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  { -- | The idempotency token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The organization ID.
    organizationId :: Prelude.Text,
    -- | If true, deletes the AWS Directory Service directory associated with the
    -- organization.
    deleteDirectory :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteOrganization_clientToken' - The idempotency token associated with the request.
--
-- 'organizationId', 'deleteOrganization_organizationId' - The organization ID.
--
-- 'deleteDirectory', 'deleteOrganization_deleteDirectory' - If true, deletes the AWS Directory Service directory associated with the
-- organization.
newDeleteOrganization ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'deleteDirectory'
  Prelude.Bool ->
  DeleteOrganization
newDeleteOrganization
  pOrganizationId_
  pDeleteDirectory_ =
    DeleteOrganization'
      { clientToken = Prelude.Nothing,
        organizationId = pOrganizationId_,
        deleteDirectory = pDeleteDirectory_
      }

-- | The idempotency token associated with the request.
deleteOrganization_clientToken :: Lens.Lens' DeleteOrganization (Prelude.Maybe Prelude.Text)
deleteOrganization_clientToken = Lens.lens (\DeleteOrganization' {clientToken} -> clientToken) (\s@DeleteOrganization' {} a -> s {clientToken = a} :: DeleteOrganization)

-- | The organization ID.
deleteOrganization_organizationId :: Lens.Lens' DeleteOrganization Prelude.Text
deleteOrganization_organizationId = Lens.lens (\DeleteOrganization' {organizationId} -> organizationId) (\s@DeleteOrganization' {} a -> s {organizationId = a} :: DeleteOrganization)

-- | If true, deletes the AWS Directory Service directory associated with the
-- organization.
deleteOrganization_deleteDirectory :: Lens.Lens' DeleteOrganization Prelude.Bool
deleteOrganization_deleteDirectory = Lens.lens (\DeleteOrganization' {deleteDirectory} -> deleteDirectory) (\s@DeleteOrganization' {} a -> s {deleteDirectory = a} :: DeleteOrganization)

instance Prelude.AWSRequest DeleteOrganization where
  type
    Rs DeleteOrganization =
      DeleteOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteOrganizationResponse'
            Prelude.<$> (x Prelude..?> "OrganizationId")
            Prelude.<*> (x Prelude..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOrganization

instance Prelude.NFData DeleteOrganization

instance Prelude.ToHeaders DeleteOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DeleteOrganization" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteOrganization where
  toJSON DeleteOrganization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClientToken" Prelude..=) Prelude.<$> clientToken,
            Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just
              ("DeleteDirectory" Prelude..= deleteDirectory)
          ]
      )

instance Prelude.ToPath DeleteOrganization where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  { -- | The organization ID.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the organization.
    state :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteOrganizationResponse_organizationId' - The organization ID.
--
-- 'state', 'deleteOrganizationResponse_state' - The state of the organization.
--
-- 'httpStatus', 'deleteOrganizationResponse_httpStatus' - The response's http status code.
newDeleteOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOrganizationResponse
newDeleteOrganizationResponse pHttpStatus_ =
  DeleteOrganizationResponse'
    { organizationId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The organization ID.
deleteOrganizationResponse_organizationId :: Lens.Lens' DeleteOrganizationResponse (Prelude.Maybe Prelude.Text)
deleteOrganizationResponse_organizationId = Lens.lens (\DeleteOrganizationResponse' {organizationId} -> organizationId) (\s@DeleteOrganizationResponse' {} a -> s {organizationId = a} :: DeleteOrganizationResponse)

-- | The state of the organization.
deleteOrganizationResponse_state :: Lens.Lens' DeleteOrganizationResponse (Prelude.Maybe Prelude.Text)
deleteOrganizationResponse_state = Lens.lens (\DeleteOrganizationResponse' {state} -> state) (\s@DeleteOrganizationResponse' {} a -> s {state = a} :: DeleteOrganizationResponse)

-- | The response's http status code.
deleteOrganizationResponse_httpStatus :: Lens.Lens' DeleteOrganizationResponse Prelude.Int
deleteOrganizationResponse_httpStatus = Lens.lens (\DeleteOrganizationResponse' {httpStatus} -> httpStatus) (\s@DeleteOrganizationResponse' {} a -> s {httpStatus = a} :: DeleteOrganizationResponse)

instance Prelude.NFData DeleteOrganizationResponse
