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
-- Module      : Amazonka.EC2.DisableIpamOrganizationAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable the IPAM account. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/enable-integ-ipam.html Enable integration with Organizations>
-- in the /Amazon VPC IPAM User Guide/.
module Amazonka.EC2.DisableIpamOrganizationAdminAccount
  ( -- * Creating a Request
    DisableIpamOrganizationAdminAccount (..),
    newDisableIpamOrganizationAdminAccount,

    -- * Request Lenses
    disableIpamOrganizationAdminAccount_dryRun,
    disableIpamOrganizationAdminAccount_delegatedAdminAccountId,

    -- * Destructuring the Response
    DisableIpamOrganizationAdminAccountResponse (..),
    newDisableIpamOrganizationAdminAccountResponse,

    -- * Response Lenses
    disableIpamOrganizationAdminAccountResponse_success,
    disableIpamOrganizationAdminAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableIpamOrganizationAdminAccount' smart constructor.
data DisableIpamOrganizationAdminAccount = DisableIpamOrganizationAdminAccount'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Organizations member account ID that you want to disable as IPAM
    -- account.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableIpamOrganizationAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableIpamOrganizationAdminAccount_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'delegatedAdminAccountId', 'disableIpamOrganizationAdminAccount_delegatedAdminAccountId' - The Organizations member account ID that you want to disable as IPAM
-- account.
newDisableIpamOrganizationAdminAccount ::
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  DisableIpamOrganizationAdminAccount
newDisableIpamOrganizationAdminAccount
  pDelegatedAdminAccountId_ =
    DisableIpamOrganizationAdminAccount'
      { dryRun =
          Prelude.Nothing,
        delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
disableIpamOrganizationAdminAccount_dryRun :: Lens.Lens' DisableIpamOrganizationAdminAccount (Prelude.Maybe Prelude.Bool)
disableIpamOrganizationAdminAccount_dryRun = Lens.lens (\DisableIpamOrganizationAdminAccount' {dryRun} -> dryRun) (\s@DisableIpamOrganizationAdminAccount' {} a -> s {dryRun = a} :: DisableIpamOrganizationAdminAccount)

-- | The Organizations member account ID that you want to disable as IPAM
-- account.
disableIpamOrganizationAdminAccount_delegatedAdminAccountId :: Lens.Lens' DisableIpamOrganizationAdminAccount Prelude.Text
disableIpamOrganizationAdminAccount_delegatedAdminAccountId = Lens.lens (\DisableIpamOrganizationAdminAccount' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@DisableIpamOrganizationAdminAccount' {} a -> s {delegatedAdminAccountId = a} :: DisableIpamOrganizationAdminAccount)

instance
  Core.AWSRequest
    DisableIpamOrganizationAdminAccount
  where
  type
    AWSResponse DisableIpamOrganizationAdminAccount =
      DisableIpamOrganizationAdminAccountResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableIpamOrganizationAdminAccountResponse'
            Prelude.<$> (x Core..@? "success")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableIpamOrganizationAdminAccount
  where
  hashWithSalt
    _salt
    DisableIpamOrganizationAdminAccount' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` delegatedAdminAccountId

instance
  Prelude.NFData
    DisableIpamOrganizationAdminAccount
  where
  rnf DisableIpamOrganizationAdminAccount' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf delegatedAdminAccountId

instance
  Core.ToHeaders
    DisableIpamOrganizationAdminAccount
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DisableIpamOrganizationAdminAccount
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DisableIpamOrganizationAdminAccount
  where
  toQuery DisableIpamOrganizationAdminAccount' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DisableIpamOrganizationAdminAccount" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "DelegatedAdminAccountId"
          Core.=: delegatedAdminAccountId
      ]

-- | /See:/ 'newDisableIpamOrganizationAdminAccountResponse' smart constructor.
data DisableIpamOrganizationAdminAccountResponse = DisableIpamOrganizationAdminAccountResponse'
  { -- | The result of disabling the IPAM account.
    success :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableIpamOrganizationAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'success', 'disableIpamOrganizationAdminAccountResponse_success' - The result of disabling the IPAM account.
--
-- 'httpStatus', 'disableIpamOrganizationAdminAccountResponse_httpStatus' - The response's http status code.
newDisableIpamOrganizationAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableIpamOrganizationAdminAccountResponse
newDisableIpamOrganizationAdminAccountResponse
  pHttpStatus_ =
    DisableIpamOrganizationAdminAccountResponse'
      { success =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The result of disabling the IPAM account.
disableIpamOrganizationAdminAccountResponse_success :: Lens.Lens' DisableIpamOrganizationAdminAccountResponse (Prelude.Maybe Prelude.Bool)
disableIpamOrganizationAdminAccountResponse_success = Lens.lens (\DisableIpamOrganizationAdminAccountResponse' {success} -> success) (\s@DisableIpamOrganizationAdminAccountResponse' {} a -> s {success = a} :: DisableIpamOrganizationAdminAccountResponse)

-- | The response's http status code.
disableIpamOrganizationAdminAccountResponse_httpStatus :: Lens.Lens' DisableIpamOrganizationAdminAccountResponse Prelude.Int
disableIpamOrganizationAdminAccountResponse_httpStatus = Lens.lens (\DisableIpamOrganizationAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DisableIpamOrganizationAdminAccountResponse' {} a -> s {httpStatus = a} :: DisableIpamOrganizationAdminAccountResponse)

instance
  Prelude.NFData
    DisableIpamOrganizationAdminAccountResponse
  where
  rnf DisableIpamOrganizationAdminAccountResponse' {..} =
    Prelude.rnf success
      `Prelude.seq` Prelude.rnf httpStatus
