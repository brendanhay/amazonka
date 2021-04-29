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
-- Module      : Network.AWS.WorkMail.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy from the specified organization.
module Network.AWS.WorkMail.DeleteRetentionPolicy
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteRetentionPolicy' smart constructor.
data DeleteRetentionPolicy = DeleteRetentionPolicy'
  { -- | The organization ID.
    organizationId :: Prelude.Text,
    -- | The retention policy ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteRetentionPolicy where
  type
    Rs DeleteRetentionPolicy =
      DeleteRetentionPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRetentionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRetentionPolicy

instance Prelude.NFData DeleteRetentionPolicy

instance Prelude.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.DeleteRetentionPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DeleteRetentionPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRetentionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteRetentionPolicyResponse
