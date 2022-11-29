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
-- Module      : Amazonka.Inspector2.Enable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Amazon Inspector scans for one or more Amazon Web Services
-- accounts.
module Amazonka.Inspector2.Enable
  ( -- * Creating a Request
    Enable (..),
    newEnable,

    -- * Request Lenses
    enable_accountIds,
    enable_clientToken,
    enable_resourceTypes,

    -- * Destructuring the Response
    EnableResponse (..),
    newEnableResponse,

    -- * Response Lenses
    enableResponse_failedAccounts,
    enableResponse_httpStatus,
    enableResponse_accounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnable' smart constructor.
data Enable = Enable'
  { -- | A list of account IDs you want to enable Amazon Inspector scans for.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The idempotency token for the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The resource scan types you want to enable.
    resourceTypes :: Prelude.NonEmpty ResourceScanType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Enable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'enable_accountIds' - A list of account IDs you want to enable Amazon Inspector scans for.
--
-- 'clientToken', 'enable_clientToken' - The idempotency token for the request.
--
-- 'resourceTypes', 'enable_resourceTypes' - The resource scan types you want to enable.
newEnable ::
  -- | 'resourceTypes'
  Prelude.NonEmpty ResourceScanType ->
  Enable
newEnable pResourceTypes_ =
  Enable'
    { accountIds = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      resourceTypes = Lens.coerced Lens.# pResourceTypes_
    }

-- | A list of account IDs you want to enable Amazon Inspector scans for.
enable_accountIds :: Lens.Lens' Enable (Prelude.Maybe [Prelude.Text])
enable_accountIds = Lens.lens (\Enable' {accountIds} -> accountIds) (\s@Enable' {} a -> s {accountIds = a} :: Enable) Prelude.. Lens.mapping Lens.coerced

-- | The idempotency token for the request.
enable_clientToken :: Lens.Lens' Enable (Prelude.Maybe Prelude.Text)
enable_clientToken = Lens.lens (\Enable' {clientToken} -> clientToken) (\s@Enable' {} a -> s {clientToken = a} :: Enable)

-- | The resource scan types you want to enable.
enable_resourceTypes :: Lens.Lens' Enable (Prelude.NonEmpty ResourceScanType)
enable_resourceTypes = Lens.lens (\Enable' {resourceTypes} -> resourceTypes) (\s@Enable' {} a -> s {resourceTypes = a} :: Enable) Prelude.. Lens.coerced

instance Core.AWSRequest Enable where
  type AWSResponse Enable = EnableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableResponse'
            Prelude.<$> (x Core..?> "failedAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "accounts" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable Enable where
  hashWithSalt _salt Enable' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData Enable where
  rnf Enable' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceTypes

instance Core.ToHeaders Enable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON Enable where
  toJSON Enable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accountIds" Core..=) Prelude.<$> accountIds,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("resourceTypes" Core..= resourceTypes)
          ]
      )

instance Core.ToPath Enable where
  toPath = Prelude.const "/enable"

instance Core.ToQuery Enable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableResponse' smart constructor.
data EnableResponse = EnableResponse'
  { -- | Information on any accounts for which Amazon Inspector scans could not
    -- be enabled. Details are provided for each account.
    failedAccounts :: Prelude.Maybe [FailedAccount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information on the accounts that have had Amazon Inspector scans
    -- successfully enabled. Details are provided for each account.
    accounts :: [Account]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedAccounts', 'enableResponse_failedAccounts' - Information on any accounts for which Amazon Inspector scans could not
-- be enabled. Details are provided for each account.
--
-- 'httpStatus', 'enableResponse_httpStatus' - The response's http status code.
--
-- 'accounts', 'enableResponse_accounts' - Information on the accounts that have had Amazon Inspector scans
-- successfully enabled. Details are provided for each account.
newEnableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableResponse
newEnableResponse pHttpStatus_ =
  EnableResponse'
    { failedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accounts = Prelude.mempty
    }

-- | Information on any accounts for which Amazon Inspector scans could not
-- be enabled. Details are provided for each account.
enableResponse_failedAccounts :: Lens.Lens' EnableResponse (Prelude.Maybe [FailedAccount])
enableResponse_failedAccounts = Lens.lens (\EnableResponse' {failedAccounts} -> failedAccounts) (\s@EnableResponse' {} a -> s {failedAccounts = a} :: EnableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
enableResponse_httpStatus :: Lens.Lens' EnableResponse Prelude.Int
enableResponse_httpStatus = Lens.lens (\EnableResponse' {httpStatus} -> httpStatus) (\s@EnableResponse' {} a -> s {httpStatus = a} :: EnableResponse)

-- | Information on the accounts that have had Amazon Inspector scans
-- successfully enabled. Details are provided for each account.
enableResponse_accounts :: Lens.Lens' EnableResponse [Account]
enableResponse_accounts = Lens.lens (\EnableResponse' {accounts} -> accounts) (\s@EnableResponse' {} a -> s {accounts = a} :: EnableResponse) Prelude.. Lens.coerced

instance Prelude.NFData EnableResponse where
  rnf EnableResponse' {..} =
    Prelude.rnf failedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accounts
