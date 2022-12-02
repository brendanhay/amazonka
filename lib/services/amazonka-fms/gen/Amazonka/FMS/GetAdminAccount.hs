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
-- Module      : Amazonka.FMS.GetAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Organizations account that is associated with Firewall
-- Manager as the Firewall Manager administrator.
module Amazonka.FMS.GetAdminAccount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAdminAccount' smart constructor.
data GetAdminAccount = GetAdminAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdminAccountResponse'
            Prelude.<$> (x Data..?> "AdminAccount")
            Prelude.<*> (x Data..?> "RoleStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAdminAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAdminAccount where
  rnf _ = ()

instance Data.ToHeaders GetAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetAdminAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAdminAccount where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetAdminAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { -- | The Amazon Web Services account that is set as the Firewall Manager
    -- administrator.
    adminAccount :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon Web Services account that you set as the
    -- Firewall Manager administrator.
    roleStatus :: Prelude.Maybe AccountRoleStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminAccount', 'getAdminAccountResponse_adminAccount' - The Amazon Web Services account that is set as the Firewall Manager
-- administrator.
--
-- 'roleStatus', 'getAdminAccountResponse_roleStatus' - The status of the Amazon Web Services account that you set as the
-- Firewall Manager administrator.
--
-- 'httpStatus', 'getAdminAccountResponse_httpStatus' - The response's http status code.
newGetAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAdminAccountResponse
newGetAdminAccountResponse pHttpStatus_ =
  GetAdminAccountResponse'
    { adminAccount =
        Prelude.Nothing,
      roleStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services account that is set as the Firewall Manager
-- administrator.
getAdminAccountResponse_adminAccount :: Lens.Lens' GetAdminAccountResponse (Prelude.Maybe Prelude.Text)
getAdminAccountResponse_adminAccount = Lens.lens (\GetAdminAccountResponse' {adminAccount} -> adminAccount) (\s@GetAdminAccountResponse' {} a -> s {adminAccount = a} :: GetAdminAccountResponse)

-- | The status of the Amazon Web Services account that you set as the
-- Firewall Manager administrator.
getAdminAccountResponse_roleStatus :: Lens.Lens' GetAdminAccountResponse (Prelude.Maybe AccountRoleStatus)
getAdminAccountResponse_roleStatus = Lens.lens (\GetAdminAccountResponse' {roleStatus} -> roleStatus) (\s@GetAdminAccountResponse' {} a -> s {roleStatus = a} :: GetAdminAccountResponse)

-- | The response's http status code.
getAdminAccountResponse_httpStatus :: Lens.Lens' GetAdminAccountResponse Prelude.Int
getAdminAccountResponse_httpStatus = Lens.lens (\GetAdminAccountResponse' {httpStatus} -> httpStatus) (\s@GetAdminAccountResponse' {} a -> s {httpStatus = a} :: GetAdminAccountResponse)

instance Prelude.NFData GetAdminAccountResponse where
  rnf GetAdminAccountResponse' {..} =
    Prelude.rnf adminAccount
      `Prelude.seq` Prelude.rnf roleStatus
      `Prelude.seq` Prelude.rnf httpStatus
