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
-- Module      : Amazonka.FMS.GetAdminScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified account\'s administrative scope.
-- The admistrative scope defines the resources that an Firewall Manager
-- administrator can manage.
module Amazonka.FMS.GetAdminScope
  ( -- * Creating a Request
    GetAdminScope (..),
    newGetAdminScope,

    -- * Request Lenses
    getAdminScope_adminAccount,

    -- * Destructuring the Response
    GetAdminScopeResponse (..),
    newGetAdminScopeResponse,

    -- * Response Lenses
    getAdminScopeResponse_adminScope,
    getAdminScopeResponse_status,
    getAdminScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAdminScope' smart constructor.
data GetAdminScope = GetAdminScope'
  { -- | The administator account that you want to get the details for.
    adminAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdminScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'getAdminScope_adminAccount' - The administator account that you want to get the details for.
newGetAdminScope ::
  -- | 'adminAccount'
  Prelude.Text ->
  GetAdminScope
newGetAdminScope pAdminAccount_ =
  GetAdminScope' {adminAccount = pAdminAccount_}

-- | The administator account that you want to get the details for.
getAdminScope_adminAccount :: Lens.Lens' GetAdminScope Prelude.Text
getAdminScope_adminAccount = Lens.lens (\GetAdminScope' {adminAccount} -> adminAccount) (\s@GetAdminScope' {} a -> s {adminAccount = a} :: GetAdminScope)

instance Core.AWSRequest GetAdminScope where
  type
    AWSResponse GetAdminScope =
      GetAdminScopeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdminScopeResponse'
            Prelude.<$> (x Data..?> "AdminScope")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAdminScope where
  hashWithSalt _salt GetAdminScope' {..} =
    _salt `Prelude.hashWithSalt` adminAccount

instance Prelude.NFData GetAdminScope where
  rnf GetAdminScope' {..} = Prelude.rnf adminAccount

instance Data.ToHeaders GetAdminScope where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetAdminScope" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAdminScope where
  toJSON GetAdminScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AdminAccount" Data..= adminAccount)]
      )

instance Data.ToPath GetAdminScope where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAdminScope where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAdminScopeResponse' smart constructor.
data GetAdminScopeResponse = GetAdminScopeResponse'
  { -- | Contains details about the administrative scope of the requested
    -- account.
    adminScope :: Prelude.Maybe AdminScope,
    -- | The current status of the request to onboard a member account as an
    -- Firewall Manager administator.
    --
    -- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
    --     administrator.
    --
    -- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
    --     Firewall Manager as an administrator, and can perform actions on the
    --     resources defined in their AdminScope.
    --
    -- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
    --     administrator.
    --
    -- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
    --     Manager administrator.
    status :: Prelude.Maybe OrganizationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdminScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminScope', 'getAdminScopeResponse_adminScope' - Contains details about the administrative scope of the requested
-- account.
--
-- 'status', 'getAdminScopeResponse_status' - The current status of the request to onboard a member account as an
-- Firewall Manager administator.
--
-- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
--     administrator.
--
-- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
--     Firewall Manager as an administrator, and can perform actions on the
--     resources defined in their AdminScope.
--
-- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
--     administrator.
--
-- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
--     Manager administrator.
--
-- 'httpStatus', 'getAdminScopeResponse_httpStatus' - The response's http status code.
newGetAdminScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAdminScopeResponse
newGetAdminScopeResponse pHttpStatus_ =
  GetAdminScopeResponse'
    { adminScope =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains details about the administrative scope of the requested
-- account.
getAdminScopeResponse_adminScope :: Lens.Lens' GetAdminScopeResponse (Prelude.Maybe AdminScope)
getAdminScopeResponse_adminScope = Lens.lens (\GetAdminScopeResponse' {adminScope} -> adminScope) (\s@GetAdminScopeResponse' {} a -> s {adminScope = a} :: GetAdminScopeResponse)

-- | The current status of the request to onboard a member account as an
-- Firewall Manager administator.
--
-- -   @ONBOARDING@ - The account is onboarding to Firewall Manager as an
--     administrator.
--
-- -   @ONBOARDING_COMPLETE@ - Firewall Manager The account is onboarded to
--     Firewall Manager as an administrator, and can perform actions on the
--     resources defined in their AdminScope.
--
-- -   @OFFBOARDING@ - The account is being removed as an Firewall Manager
--     administrator.
--
-- -   @OFFBOARDING_COMPLETE@ - The account has been removed as an Firewall
--     Manager administrator.
getAdminScopeResponse_status :: Lens.Lens' GetAdminScopeResponse (Prelude.Maybe OrganizationStatus)
getAdminScopeResponse_status = Lens.lens (\GetAdminScopeResponse' {status} -> status) (\s@GetAdminScopeResponse' {} a -> s {status = a} :: GetAdminScopeResponse)

-- | The response's http status code.
getAdminScopeResponse_httpStatus :: Lens.Lens' GetAdminScopeResponse Prelude.Int
getAdminScopeResponse_httpStatus = Lens.lens (\GetAdminScopeResponse' {httpStatus} -> httpStatus) (\s@GetAdminScopeResponse' {} a -> s {httpStatus = a} :: GetAdminScopeResponse)

instance Prelude.NFData GetAdminScopeResponse where
  rnf GetAdminScopeResponse' {..} =
    Prelude.rnf adminScope
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
