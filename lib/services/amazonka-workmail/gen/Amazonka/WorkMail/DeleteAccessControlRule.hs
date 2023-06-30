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
-- Module      : Amazonka.WorkMail.DeleteAccessControlRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control rule for the specified WorkMail organization.
--
-- Deleting already deleted and non-existing rules does not produce an
-- error. In those cases, the service sends back an HTTP 200 response with
-- an empty HTTP body.
module Amazonka.WorkMail.DeleteAccessControlRule
  ( -- * Creating a Request
    DeleteAccessControlRule (..),
    newDeleteAccessControlRule,

    -- * Request Lenses
    deleteAccessControlRule_organizationId,
    deleteAccessControlRule_name,

    -- * Destructuring the Response
    DeleteAccessControlRuleResponse (..),
    newDeleteAccessControlRuleResponse,

    -- * Response Lenses
    deleteAccessControlRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteAccessControlRule' smart constructor.
data DeleteAccessControlRule = DeleteAccessControlRule'
  { -- | The identifier for the organization.
    organizationId :: Prelude.Text,
    -- | The name of the access control rule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessControlRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteAccessControlRule_organizationId' - The identifier for the organization.
--
-- 'name', 'deleteAccessControlRule_name' - The name of the access control rule.
newDeleteAccessControlRule ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteAccessControlRule
newDeleteAccessControlRule pOrganizationId_ pName_ =
  DeleteAccessControlRule'
    { organizationId =
        pOrganizationId_,
      name = pName_
    }

-- | The identifier for the organization.
deleteAccessControlRule_organizationId :: Lens.Lens' DeleteAccessControlRule Prelude.Text
deleteAccessControlRule_organizationId = Lens.lens (\DeleteAccessControlRule' {organizationId} -> organizationId) (\s@DeleteAccessControlRule' {} a -> s {organizationId = a} :: DeleteAccessControlRule)

-- | The name of the access control rule.
deleteAccessControlRule_name :: Lens.Lens' DeleteAccessControlRule Prelude.Text
deleteAccessControlRule_name = Lens.lens (\DeleteAccessControlRule' {name} -> name) (\s@DeleteAccessControlRule' {} a -> s {name = a} :: DeleteAccessControlRule)

instance Core.AWSRequest DeleteAccessControlRule where
  type
    AWSResponse DeleteAccessControlRule =
      DeleteAccessControlRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessControlRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccessControlRule where
  hashWithSalt _salt DeleteAccessControlRule' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAccessControlRule where
  rnf DeleteAccessControlRule' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteAccessControlRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteAccessControlRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccessControlRule where
  toJSON DeleteAccessControlRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DeleteAccessControlRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccessControlRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessControlRuleResponse' smart constructor.
data DeleteAccessControlRuleResponse = DeleteAccessControlRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessControlRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessControlRuleResponse_httpStatus' - The response's http status code.
newDeleteAccessControlRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessControlRuleResponse
newDeleteAccessControlRuleResponse pHttpStatus_ =
  DeleteAccessControlRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccessControlRuleResponse_httpStatus :: Lens.Lens' DeleteAccessControlRuleResponse Prelude.Int
deleteAccessControlRuleResponse_httpStatus = Lens.lens (\DeleteAccessControlRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessControlRuleResponse' {} a -> s {httpStatus = a} :: DeleteAccessControlRuleResponse)

instance
  Prelude.NFData
    DeleteAccessControlRuleResponse
  where
  rnf DeleteAccessControlRuleResponse' {..} =
    Prelude.rnf httpStatus
