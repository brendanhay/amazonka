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
-- Module      : Amazonka.WorkMail.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy from the specified organization.
module Amazonka.WorkMail.DeleteRetentionPolicy
  ( -- * Creating a Request
    DeleteRetentionPolicy (..),
    newDeleteRetentionPolicy,

    -- * Request Lenses
    deleteRetentionPolicy_organizationId,
    deleteRetentionPolicy_id,

    -- * Destructuring the Response
    DeleteRetentionPolicyResponse (..),
    newDeleteRetentionPolicyResponse,

    -- * Response Lenses
    deleteRetentionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Prelude.Text,
    -- | The retention policy ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteRetentionPolicy_organizationId' - The organization ID.
--
-- 'id', 'deleteRetentionPolicy_id' - The retention policy ID.
newDeleteRetentionPolicy ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteRetentionPolicy
newDeleteRetentionPolicy pOrganizationId_ pId_ =
  DeleteRetentionPolicy'
    { organizationId =
        pOrganizationId_,
      id = pId_
    }

-- | The organization ID.
deleteRetentionPolicy_organizationId :: Lens.Lens' DeleteRetentionPolicy Prelude.Text
deleteRetentionPolicy_organizationId = Lens.lens (\DeleteRetentionPolicy' {organizationId} -> organizationId) (\s@DeleteRetentionPolicy' {} a -> s {organizationId = a} :: DeleteRetentionPolicy)

-- | The retention policy ID.
deleteRetentionPolicy_id :: Lens.Lens' DeleteRetentionPolicy Prelude.Text
deleteRetentionPolicy_id = Lens.lens (\DeleteRetentionPolicy' {id} -> id) (\s@DeleteRetentionPolicy' {} a -> s {id = a} :: DeleteRetentionPolicy)

instance Core.AWSRequest DeleteRetentionPolicy where
  type
    AWSResponse DeleteRetentionPolicy =
      DeleteRetentionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRetentionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRetentionPolicy where
  hashWithSalt _salt DeleteRetentionPolicy' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteRetentionPolicy where
  rnf DeleteRetentionPolicy' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteRetentionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeleteRetentionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRetentionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRetentionPolicyResponse_httpStatus' - The response's http status code.
newDeleteRetentionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRetentionPolicyResponse
newDeleteRetentionPolicyResponse pHttpStatus_ =
  DeleteRetentionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRetentionPolicyResponse_httpStatus :: Lens.Lens' DeleteRetentionPolicyResponse Prelude.Int
deleteRetentionPolicyResponse_httpStatus = Lens.lens (\DeleteRetentionPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteRetentionPolicyResponse' {} a -> s {httpStatus = a} :: DeleteRetentionPolicyResponse)

instance Prelude.NFData DeleteRetentionPolicyResponse where
  rnf DeleteRetentionPolicyResponse' {..} =
    Prelude.rnf httpStatus
