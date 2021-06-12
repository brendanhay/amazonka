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
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty
-- delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    EnableOrganizationAdminAccount (..),
    newEnableOrganizationAdminAccount,

    -- * Request Lenses
    enableOrganizationAdminAccount_adminAccountId,

    -- * Destructuring the Response
    EnableOrganizationAdminAccountResponse (..),
    newEnableOrganizationAdminAccountResponse,

    -- * Response Lenses
    enableOrganizationAdminAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableOrganizationAdminAccount' smart constructor.
data EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organization account to be enabled as a
    -- GuardDuty delegated administrator.
    adminAccountId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccountId', 'enableOrganizationAdminAccount_adminAccountId' - The AWS Account ID for the organization account to be enabled as a
-- GuardDuty delegated administrator.
newEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Core.Text ->
  EnableOrganizationAdminAccount
newEnableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organization account to be enabled as a
-- GuardDuty delegated administrator.
enableOrganizationAdminAccount_adminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Core.Text
enableOrganizationAdminAccount_adminAccountId = Lens.lens (\EnableOrganizationAdminAccount' {adminAccountId} -> adminAccountId) (\s@EnableOrganizationAdminAccount' {} a -> s {adminAccountId = a} :: EnableOrganizationAdminAccount)

instance
  Core.AWSRequest
    EnableOrganizationAdminAccount
  where
  type
    AWSResponse EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableOrganizationAdminAccount

instance Core.NFData EnableOrganizationAdminAccount

instance
  Core.ToHeaders
    EnableOrganizationAdminAccount
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("adminAccountId" Core..= adminAccountId)
          ]
      )

instance Core.ToPath EnableOrganizationAdminAccount where
  toPath = Core.const "/admin/enable"

instance Core.ToQuery EnableOrganizationAdminAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableOrganizationAdminAccountResponse' smart constructor.
data EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newEnableOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableOrganizationAdminAccountResponse
newEnableOrganizationAdminAccountResponse
  pHttpStatus_ =
    EnableOrganizationAdminAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
enableOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Core.Int
enableOrganizationAdminAccountResponse_httpStatus = Lens.lens (\EnableOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@EnableOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: EnableOrganizationAdminAccountResponse)

instance
  Core.NFData
    EnableOrganizationAdminAccountResponse
