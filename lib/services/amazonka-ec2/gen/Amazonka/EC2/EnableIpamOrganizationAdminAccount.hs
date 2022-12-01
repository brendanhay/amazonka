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
-- Module      : Amazonka.EC2.EnableIpamOrganizationAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable an Organizations member account as the IPAM admin account. You
-- cannot select the Organizations management account as the IPAM admin
-- account. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/enable-integ-ipam.html Enable integration with Organizations>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.EnableIpamOrganizationAdminAccount
  ( -- * Creating a Request
    EnableIpamOrganizationAdminAccount (..),
    newEnableIpamOrganizationAdminAccount,

    -- * Request Lenses
    enableIpamOrganizationAdminAccount_dryRun,
    enableIpamOrganizationAdminAccount_delegatedAdminAccountId,

    -- * Destructuring the Response
    EnableIpamOrganizationAdminAccountResponse (..),
    newEnableIpamOrganizationAdminAccountResponse,

    -- * Response Lenses
    enableIpamOrganizationAdminAccountResponse_success,
    enableIpamOrganizationAdminAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableIpamOrganizationAdminAccount' smart constructor.
data EnableIpamOrganizationAdminAccount = EnableIpamOrganizationAdminAccount'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Organizations member account ID that you want to enable as the IPAM
    -- account.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableIpamOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableIpamOrganizationAdminAccount_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'delegatedAdminAccountId', 'enableIpamOrganizationAdminAccount_delegatedAdminAccountId' - The Organizations member account ID that you want to enable as the IPAM
-- account.
newEnableIpamOrganizationAdminAccount ::
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  EnableIpamOrganizationAdminAccount
newEnableIpamOrganizationAdminAccount
  pDelegatedAdminAccountId_ =
    EnableIpamOrganizationAdminAccount'
      { dryRun =
          Prelude.Nothing,
        delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
enableIpamOrganizationAdminAccount_dryRun :: Lens.Lens' EnableIpamOrganizationAdminAccount (Prelude.Maybe Prelude.Bool)
enableIpamOrganizationAdminAccount_dryRun = Lens.lens (\EnableIpamOrganizationAdminAccount' {dryRun} -> dryRun) (\s@EnableIpamOrganizationAdminAccount' {} a -> s {dryRun = a} :: EnableIpamOrganizationAdminAccount)

-- | The Organizations member account ID that you want to enable as the IPAM
-- account.
enableIpamOrganizationAdminAccount_delegatedAdminAccountId :: Lens.Lens' EnableIpamOrganizationAdminAccount Prelude.Text
enableIpamOrganizationAdminAccount_delegatedAdminAccountId = Lens.lens (\EnableIpamOrganizationAdminAccount' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@EnableIpamOrganizationAdminAccount' {} a -> s {delegatedAdminAccountId = a} :: EnableIpamOrganizationAdminAccount)

instance
  Core.AWSRequest
    EnableIpamOrganizationAdminAccount
  where
  type
    AWSResponse EnableIpamOrganizationAdminAccount =
      EnableIpamOrganizationAdminAccountResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableIpamOrganizationAdminAccountResponse'
            Prelude.<$> (x Core..@? "success")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableIpamOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    EnableIpamOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` delegatedAdminAccountId

instance
  Prelude.NFData
    EnableIpamOrganizationAdminAccount
  where
  rnf EnableIpamOrganizationAdminAccount' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf delegatedAdminAccountId

instance
  Core.ToHeaders
    EnableIpamOrganizationAdminAccount
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    EnableIpamOrganizationAdminAccount
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    EnableIpamOrganizationAdminAccount
  where
  toQuery EnableIpamOrganizationAdminAccount' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "EnableIpamOrganizationAdminAccount" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "DelegatedAdminAccountId"
          Core.=: delegatedAdminAccountId
      ]

-- | /See:/ 'newEnableIpamOrganizationAdminAccountResponse' smart constructor.
data EnableIpamOrganizationAdminAccountResponse = EnableIpamOrganizationAdminAccountResponse'
  { -- | The result of enabling the IPAM account.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableIpamOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'enableIpamOrganizationAdminAccountResponse_success' - The result of enabling the IPAM account.
--
-- 'httpStatus', 'enableIpamOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newEnableIpamOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableIpamOrganizationAdminAccountResponse
newEnableIpamOrganizationAdminAccountResponse
  pHttpStatus_ =
    EnableIpamOrganizationAdminAccountResponse'
      { success =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The result of enabling the IPAM account.
enableIpamOrganizationAdminAccountResponse_success :: Lens.Lens' EnableIpamOrganizationAdminAccountResponse (Prelude.Maybe Prelude.Bool)
enableIpamOrganizationAdminAccountResponse_success = Lens.lens (\EnableIpamOrganizationAdminAccountResponse' {success} -> success) (\s@EnableIpamOrganizationAdminAccountResponse' {} a -> s {success = a} :: EnableIpamOrganizationAdminAccountResponse)

-- | The response's http status code.
enableIpamOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' EnableIpamOrganizationAdminAccountResponse Prelude.Int
enableIpamOrganizationAdminAccountResponse_httpStatus = Lens.lens (\EnableIpamOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@EnableIpamOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: EnableIpamOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    EnableIpamOrganizationAdminAccountResponse
  where
  rnf EnableIpamOrganizationAdminAccountResponse' {..} =
    Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
