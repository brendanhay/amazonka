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
-- Module      : Amazonka.WorkMail.DeleteMobileDeviceAccessRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a mobile device access rule for the specified WorkMail
-- organization.
--
-- Deleting already deleted and non-existing rules does not produce an
-- error. In those cases, the service sends back an HTTP 200 response with
-- an empty HTTP body.
module Amazonka.WorkMail.DeleteMobileDeviceAccessRule
  ( -- * Creating a Request
    DeleteMobileDeviceAccessRule (..),
    newDeleteMobileDeviceAccessRule,

    -- * Request Lenses
    deleteMobileDeviceAccessRule_organizationId,
    deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId,

    -- * Destructuring the Response
    DeleteMobileDeviceAccessRuleResponse (..),
    newDeleteMobileDeviceAccessRuleResponse,

    -- * Response Lenses
    deleteMobileDeviceAccessRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteMobileDeviceAccessRule' smart constructor.
data DeleteMobileDeviceAccessRule = DeleteMobileDeviceAccessRule'
  { -- | The WorkMail organization under which the rule will be deleted.
    organizationId :: Prelude.Text,
    -- | The identifier of the rule to be deleted.
    mobileDeviceAccessRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMobileDeviceAccessRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteMobileDeviceAccessRule_organizationId' - The WorkMail organization under which the rule will be deleted.
--
-- 'mobileDeviceAccessRuleId', 'deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId' - The identifier of the rule to be deleted.
newDeleteMobileDeviceAccessRule ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'mobileDeviceAccessRuleId'
  Prelude.Text ->
  DeleteMobileDeviceAccessRule
newDeleteMobileDeviceAccessRule
  pOrganizationId_
  pMobileDeviceAccessRuleId_ =
    DeleteMobileDeviceAccessRule'
      { organizationId =
          pOrganizationId_,
        mobileDeviceAccessRuleId =
          pMobileDeviceAccessRuleId_
      }

-- | The WorkMail organization under which the rule will be deleted.
deleteMobileDeviceAccessRule_organizationId :: Lens.Lens' DeleteMobileDeviceAccessRule Prelude.Text
deleteMobileDeviceAccessRule_organizationId = Lens.lens (\DeleteMobileDeviceAccessRule' {organizationId} -> organizationId) (\s@DeleteMobileDeviceAccessRule' {} a -> s {organizationId = a} :: DeleteMobileDeviceAccessRule)

-- | The identifier of the rule to be deleted.
deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId :: Lens.Lens' DeleteMobileDeviceAccessRule Prelude.Text
deleteMobileDeviceAccessRule_mobileDeviceAccessRuleId = Lens.lens (\DeleteMobileDeviceAccessRule' {mobileDeviceAccessRuleId} -> mobileDeviceAccessRuleId) (\s@DeleteMobileDeviceAccessRule' {} a -> s {mobileDeviceAccessRuleId = a} :: DeleteMobileDeviceAccessRule)

instance Core.AWSRequest DeleteMobileDeviceAccessRule where
  type
    AWSResponse DeleteMobileDeviceAccessRule =
      DeleteMobileDeviceAccessRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMobileDeviceAccessRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteMobileDeviceAccessRule
  where
  hashWithSalt _salt DeleteMobileDeviceAccessRule' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` mobileDeviceAccessRuleId

instance Prelude.NFData DeleteMobileDeviceAccessRule where
  rnf DeleteMobileDeviceAccessRule' {..} =
    Prelude.rnf organizationId `Prelude.seq`
      Prelude.rnf mobileDeviceAccessRuleId

instance Data.ToHeaders DeleteMobileDeviceAccessRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteMobileDeviceAccessRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMobileDeviceAccessRule where
  toJSON DeleteMobileDeviceAccessRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just
              ( "MobileDeviceAccessRuleId"
                  Data..= mobileDeviceAccessRuleId
              )
          ]
      )

instance Data.ToPath DeleteMobileDeviceAccessRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMobileDeviceAccessRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMobileDeviceAccessRuleResponse' smart constructor.
data DeleteMobileDeviceAccessRuleResponse = DeleteMobileDeviceAccessRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMobileDeviceAccessRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMobileDeviceAccessRuleResponse_httpStatus' - The response's http status code.
newDeleteMobileDeviceAccessRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMobileDeviceAccessRuleResponse
newDeleteMobileDeviceAccessRuleResponse pHttpStatus_ =
  DeleteMobileDeviceAccessRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMobileDeviceAccessRuleResponse_httpStatus :: Lens.Lens' DeleteMobileDeviceAccessRuleResponse Prelude.Int
deleteMobileDeviceAccessRuleResponse_httpStatus = Lens.lens (\DeleteMobileDeviceAccessRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteMobileDeviceAccessRuleResponse' {} a -> s {httpStatus = a} :: DeleteMobileDeviceAccessRuleResponse)

instance
  Prelude.NFData
    DeleteMobileDeviceAccessRuleResponse
  where
  rnf DeleteMobileDeviceAccessRuleResponse' {..} =
    Prelude.rnf httpStatus
