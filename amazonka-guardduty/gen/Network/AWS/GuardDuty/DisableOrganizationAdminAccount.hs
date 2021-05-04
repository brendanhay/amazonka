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
-- Module      : Network.AWS.GuardDuty.DisableOrganizationAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an AWS account within the Organization as the GuardDuty
-- delegated administrator.
module Network.AWS.GuardDuty.DisableOrganizationAdminAccount
  ( -- * Creating a Request
    DisableOrganizationAdminAccount (..),
    newDisableOrganizationAdminAccount,

    -- * Request Lenses
    disableOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    DisableOrganizationAdminAccountResponse (..),
    newDisableOrganizationAdminAccountResponse,

    -- * Response Lenses
    disableOrganizationAdminAccountResponse_httpStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableOrganizationAdminAccount' smart constructor.
data DisableOrganizationAdminAccount = DisableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organizations account to be disabled as a
    -- GuardDuty delegated administrator.
    adminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'disableOrganizationAdminAccount_adminAccountId' - The AWS Account ID for the organizations account to be disabled as a
-- GuardDuty delegated administrator.
newDisableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Prelude.Text ->
  DisableOrganizationAdminAccount
newDisableOrganizationAdminAccount pAdminAccountId_ =
  DisableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organizations account to be disabled as a
-- GuardDuty delegated administrator.
disableOrganizationAdminAccount_adminAccountId :: Lens.Lens' DisableOrganizationAdminAccount Prelude.Text
disableOrganizationAdminAccount_adminAccountId = Lens.lens (\DisableOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@DisableOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: DisableOrganizationAdminAccount)

instance
  Prelude.AWSRequest
    DisableOrganizationAdminAccount
  where
  type
    Rs DisableOrganizationAdminAccount =
      DisableOrganizationAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableOrganizationAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableOrganizationAdminAccount

instance
  Prelude.NFData
    DisableOrganizationAdminAccount

instance
  Prelude.ToHeaders
    DisableOrganizationAdminAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisableOrganizationAdminAccount
  where
  toJSON DisableOrganizationAdminAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("adminAccountId" Prelude..= adminAccountId)
          ]
      )

instance
  Prelude.ToPath
    DisableOrganizationAdminAccount
  where
  toPath = Prelude.const "/admin/disable"

instance
  Prelude.ToQuery
    DisableOrganizationAdminAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableOrganizationAdminAccountResponse' smart constructor.
data DisableOrganizationAdminAccountResponse = DisableOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newDisableOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableOrganizationAdminAccountResponse
newDisableOrganizationAdminAccountResponse
  pHttpStatus_ =
    DisableOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disableOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' DisableOrganizationAdminAccountResponse Prelude.Int
disableOrganizationAdminAccountResponse_httpStatus = Lens.lens (\DisableOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DisableOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: DisableOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    DisableOrganizationAdminAccountResponse
