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
-- Module      : Network.AWS.WorkMail.DeleteAccessControlRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control rule for the specified WorkMail organization.
module Network.AWS.WorkMail.DeleteAccessControlRule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteAccessControlRule' smart constructor.
data DeleteAccessControlRule = DeleteAccessControlRule'
  { -- | The identifier for the organization.
    organizationId :: Core.Text,
    -- | The name of the access control rule.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  DeleteAccessControlRule
newDeleteAccessControlRule pOrganizationId_ pName_ =
  DeleteAccessControlRule'
    { organizationId =
        pOrganizationId_,
      name = pName_
    }

-- | The identifier for the organization.
deleteAccessControlRule_organizationId :: Lens.Lens' DeleteAccessControlRule Core.Text
deleteAccessControlRule_organizationId = Lens.lens (\DeleteAccessControlRule' {organizationId} -> organizationId) (\s@DeleteAccessControlRule' {} a -> s {organizationId = a} :: DeleteAccessControlRule)

-- | The name of the access control rule.
deleteAccessControlRule_name :: Lens.Lens' DeleteAccessControlRule Core.Text
deleteAccessControlRule_name = Lens.lens (\DeleteAccessControlRule' {name} -> name) (\s@DeleteAccessControlRule' {} a -> s {name = a} :: DeleteAccessControlRule)

instance Core.AWSRequest DeleteAccessControlRule where
  type
    AWSResponse DeleteAccessControlRule =
      DeleteAccessControlRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessControlRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAccessControlRule

instance Core.NFData DeleteAccessControlRule

instance Core.ToHeaders DeleteAccessControlRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DeleteAccessControlRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAccessControlRule where
  toJSON DeleteAccessControlRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DeleteAccessControlRule where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAccessControlRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAccessControlRuleResponse' smart constructor.
data DeleteAccessControlRuleResponse = DeleteAccessControlRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteAccessControlRuleResponse
newDeleteAccessControlRuleResponse pHttpStatus_ =
  DeleteAccessControlRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccessControlRuleResponse_httpStatus :: Lens.Lens' DeleteAccessControlRuleResponse Core.Int
deleteAccessControlRuleResponse_httpStatus = Lens.lens (\DeleteAccessControlRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessControlRuleResponse' {} a -> s {httpStatus = a} :: DeleteAccessControlRuleResponse)

instance Core.NFData DeleteAccessControlRuleResponse
