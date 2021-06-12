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
-- Module      : Network.AWS.FMS.GetAdminAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the AWS Organizations master account that is associated with AWS
-- Firewall Manager as the AWS Firewall Manager administrator.
module Network.AWS.FMS.GetAdminAccount
  ( -- * Creating a Request
    GetAdminAccount (..),
    newGetAdminAccount,

    -- * Destructuring the Response
    GetAdminAccountResponse (..),
    newGetAdminAccountResponse,

    -- * Response Lenses
    getAdminAccountResponse_adminAccount,
    getAdminAccountResponse_roleStatus,
    getAdminAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAdminAccount' smart constructor.
data GetAdminAccount = GetAdminAccount'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAdminAccount ::
  GetAdminAccount
newGetAdminAccount = GetAdminAccount'

instance Core.AWSRequest GetAdminAccount where
  type
    AWSResponse GetAdminAccount =
      GetAdminAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdminAccountResponse'
            Core.<$> (x Core..?> "AdminAccount")
            Core.<*> (x Core..?> "RoleStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAdminAccount

instance Core.NFData GetAdminAccount

instance Core.ToHeaders GetAdminAccount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetAdminAccount" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAdminAccount where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetAdminAccount where
  toPath = Core.const "/"

instance Core.ToQuery GetAdminAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { -- | The AWS account that is set as the AWS Firewall Manager administrator.
    adminAccount :: Core.Maybe Core.Text,
    -- | The status of the AWS account that you set as the AWS Firewall Manager
    -- administrator.
    roleStatus :: Core.Maybe AccountRoleStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'getAdminAccountResponse_adminAccount' - The AWS account that is set as the AWS Firewall Manager administrator.
--
-- 'roleStatus', 'getAdminAccountResponse_roleStatus' - The status of the AWS account that you set as the AWS Firewall Manager
-- administrator.
--
-- 'httpStatus', 'getAdminAccountResponse_httpStatus' - The response's http status code.
newGetAdminAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAdminAccountResponse
newGetAdminAccountResponse pHttpStatus_ =
  GetAdminAccountResponse'
    { adminAccount =
        Core.Nothing,
      roleStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AWS account that is set as the AWS Firewall Manager administrator.
getAdminAccountResponse_adminAccount :: Lens.Lens' GetAdminAccountResponse (Core.Maybe Core.Text)
getAdminAccountResponse_adminAccount = Lens.lens (\GetAdminAccountResponse' {adminAccount} -> adminAccount) (\s@GetAdminAccountResponse' {} a -> s {adminAccount = a} :: GetAdminAccountResponse)

-- | The status of the AWS account that you set as the AWS Firewall Manager
-- administrator.
getAdminAccountResponse_roleStatus :: Lens.Lens' GetAdminAccountResponse (Core.Maybe AccountRoleStatus)
getAdminAccountResponse_roleStatus = Lens.lens (\GetAdminAccountResponse' {roleStatus} -> roleStatus) (\s@GetAdminAccountResponse' {} a -> s {roleStatus = a} :: GetAdminAccountResponse)

-- | The response's http status code.
getAdminAccountResponse_httpStatus :: Lens.Lens' GetAdminAccountResponse Core.Int
getAdminAccountResponse_httpStatus = Lens.lens (\GetAdminAccountResponse' {httpStatus} -> httpStatus) (\s@GetAdminAccountResponse' {} a -> s {httpStatus = a} :: GetAdminAccountResponse)

instance Core.NFData GetAdminAccountResponse
