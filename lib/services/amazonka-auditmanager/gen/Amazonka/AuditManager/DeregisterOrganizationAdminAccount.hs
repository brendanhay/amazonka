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
-- Module      : Amazonka.AuditManager.DeregisterOrganizationAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Amazon Web Services account as a delegated
-- administrator for Audit Manager.
--
-- When you remove a delegated administrator from your Audit Manager
-- settings, you continue to have access to the evidence that you
-- previously collected under that account. This is also the case when you
-- deregister a delegated administrator from Organizations. However, Audit
-- Manager will stop collecting and attaching evidence to that delegated
-- administrator account moving forward.
--
-- Keep in mind the following cleanup task if you use evidence finder:
--
-- Before you use your management account to remove a delegated
-- administrator, make sure that the current delegated administrator
-- account signs in to Audit Manager and disables evidence finder first.
-- Disabling evidence finder automatically deletes the event data store
-- that was created in their account when they enabled evidence finder. If
-- this task isn’t completed, the event data store remains in their
-- account. In this case, we recommend that the original delegated
-- administrator goes to CloudTrail Lake and manually
-- <https://docs.aws.amazon.com/userguide/awscloudtrail/latest/userguide/query-eds-disable-termination.html deletes the event data store>.
--
-- This cleanup task is necessary to ensure that you don\'t end up with
-- multiple event data stores. Audit Manager will ignore an unused event
-- data store after you remove or change a delegated administrator account.
-- However, the unused event data store continues to incur storage costs
-- from CloudTrail Lake if you don\'t delete it.
--
-- When you deregister a delegated administrator account for Audit Manager,
-- the data for that account isn’t deleted. If you want to delete resource
-- data for a delegated administrator account, you must perform that task
-- separately before you deregister the account. Either, you can do this in
-- the Audit Manager console. Or, you can use one of the delete API
-- operations that are provided by Audit Manager.
--
-- To delete your Audit Manager resource data, see the following
-- instructions:
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeleteAssessment.html DeleteAssessment>
--     (see also:
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/delete-assessment.html Deleting an assessment>
--     in the /Audit Manager User Guide/)
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeleteAssessmentFramework.html DeleteAssessmentFramework>
--     (see also:
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/delete-custom-framework.html Deleting a custom framework>
--     in the /Audit Manager User Guide/)
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeleteAssessmentFrameworkShare.html DeleteAssessmentFrameworkShare>
--     (see also:
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/deleting-shared-framework-requests.html Deleting a share request>
--     in the /Audit Manager User Guide/)
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeleteAssessmentReport.html DeleteAssessmentReport>
--     (see also:
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/generate-assessment-report.html#delete-assessment-report-steps Deleting an assessment report>
--     in the /Audit Manager User Guide/)
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeleteControl.html DeleteControl>
--     (see also:
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/delete-controls.html Deleting a custom control>
--     in the /Audit Manager User Guide/)
--
-- At this time, Audit Manager doesn\'t provide an option to delete
-- evidence. All available delete operations are listed above.
module Amazonka.AuditManager.DeregisterOrganizationAdminAccount
  ( -- * Creating a Request
    DeregisterOrganizationAdminAccount (..),
    newDeregisterOrganizationAdminAccount,

    -- * Request Lenses
    deregisterOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    DeregisterOrganizationAdminAccountResponse (..),
    newDeregisterOrganizationAdminAccountResponse,

    -- * Response Lenses
    deregisterOrganizationAdminAccountResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterOrganizationAdminAccount' smart constructor.
data DeregisterOrganizationAdminAccount = DeregisterOrganizationAdminAccount'
  { -- | The identifier for the administrator account.
    adminAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'deregisterOrganizationAdminAccount_adminAccountId' - The identifier for the administrator account.
newDeregisterOrganizationAdminAccount ::
  DeregisterOrganizationAdminAccount
newDeregisterOrganizationAdminAccount =
  DeregisterOrganizationAdminAccount'
    { adminAccountId =
        Prelude.Nothing
    }

-- | The identifier for the administrator account.
deregisterOrganizationAdminAccount_adminAccountId :: Lens.Lens' DeregisterOrganizationAdminAccount (Prelude.Maybe Prelude.Text)
deregisterOrganizationAdminAccount_adminAccountId = Lens.lens (\DeregisterOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@DeregisterOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: DeregisterOrganizationAdminAccount)

instance
  Core.AWSRequest
    DeregisterOrganizationAdminAccount
  where
  type
    AWSResponse DeregisterOrganizationAdminAccount =
      DeregisterOrganizationAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    DeregisterOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` adminAccountId

instance
  Prelude.NFData
    DeregisterOrganizationAdminAccount
  where
  rnf DeregisterOrganizationAdminAccount' {..} =
    Prelude.rnf adminAccountId

instance
  Core.ToHeaders
    DeregisterOrganizationAdminAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeregisterOrganizationAdminAccount
  where
  toJSON DeregisterOrganizationAdminAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("adminAccountId" Core..=)
              Prelude.<$> adminAccountId
          ]
      )

instance
  Core.ToPath
    DeregisterOrganizationAdminAccount
  where
  toPath =
    Prelude.const
      "/account/deregisterOrganizationAdminAccount"

instance
  Core.ToQuery
    DeregisterOrganizationAdminAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterOrganizationAdminAccountResponse' smart constructor.
data DeregisterOrganizationAdminAccountResponse = DeregisterOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newDeregisterOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterOrganizationAdminAccountResponse
newDeregisterOrganizationAdminAccountResponse
  pHttpStatus_ =
    DeregisterOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' DeregisterOrganizationAdminAccountResponse Prelude.Int
deregisterOrganizationAdminAccountResponse_httpStatus = Lens.lens (\DeregisterOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DeregisterOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: DeregisterOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    DeregisterOrganizationAdminAccountResponse
  where
  rnf DeregisterOrganizationAdminAccountResponse' {..} =
    Prelude.rnf httpStatus
