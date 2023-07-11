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
-- Module      : Amazonka.Inspector2.Disable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables Amazon Inspector scans for one or more Amazon Web Services
-- accounts. Disabling all scan types in an account disables the Amazon
-- Inspector service.
module Amazonka.Inspector2.Disable
  ( -- * Creating a Request
    Disable (..),
    newDisable,

    -- * Request Lenses
    disable_accountIds,
    disable_resourceTypes,

    -- * Destructuring the Response
    DisableResponse (..),
    newDisableResponse,

    -- * Response Lenses
    disableResponse_failedAccounts,
    disableResponse_httpStatus,
    disableResponse_accounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisable' smart constructor.
data Disable = Disable'
  { -- | An array of account IDs you want to disable Amazon Inspector scans for.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The resource scan types you want to disable.
    resourceTypes :: Prelude.Maybe [ResourceScanType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Disable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'disable_accountIds' - An array of account IDs you want to disable Amazon Inspector scans for.
--
-- 'resourceTypes', 'disable_resourceTypes' - The resource scan types you want to disable.
newDisable ::
  Disable
newDisable =
  Disable'
    { accountIds = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | An array of account IDs you want to disable Amazon Inspector scans for.
disable_accountIds :: Lens.Lens' Disable (Prelude.Maybe [Prelude.Text])
disable_accountIds = Lens.lens (\Disable' {accountIds} -> accountIds) (\s@Disable' {} a -> s {accountIds = a} :: Disable) Prelude.. Lens.mapping Lens.coerced

-- | The resource scan types you want to disable.
disable_resourceTypes :: Lens.Lens' Disable (Prelude.Maybe [ResourceScanType])
disable_resourceTypes = Lens.lens (\Disable' {resourceTypes} -> resourceTypes) (\s@Disable' {} a -> s {resourceTypes = a} :: Disable) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest Disable where
  type AWSResponse Disable = DisableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableResponse'
            Prelude.<$> (x Data..?> "failedAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "accounts" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable Disable where
  hashWithSalt _salt Disable' {..} =
    _salt
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData Disable where
  rnf Disable' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf resourceTypes

instance Data.ToHeaders Disable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Disable where
  toJSON Disable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("resourceTypes" Data..=) Prelude.<$> resourceTypes
          ]
      )

instance Data.ToPath Disable where
  toPath = Prelude.const "/disable"

instance Data.ToQuery Disable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableResponse' smart constructor.
data DisableResponse = DisableResponse'
  { -- | Information on any accounts for which Amazon Inspector scans could not
    -- be disabled. Details are provided for each account.
    failedAccounts :: Prelude.Maybe [FailedAccount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information on the accounts that have had Amazon Inspector scans
    -- successfully disabled. Details are provided for each account.
    accounts :: [Account]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedAccounts', 'disableResponse_failedAccounts' - Information on any accounts for which Amazon Inspector scans could not
-- be disabled. Details are provided for each account.
--
-- 'httpStatus', 'disableResponse_httpStatus' - The response's http status code.
--
-- 'accounts', 'disableResponse_accounts' - Information on the accounts that have had Amazon Inspector scans
-- successfully disabled. Details are provided for each account.
newDisableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableResponse
newDisableResponse pHttpStatus_ =
  DisableResponse'
    { failedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accounts = Prelude.mempty
    }

-- | Information on any accounts for which Amazon Inspector scans could not
-- be disabled. Details are provided for each account.
disableResponse_failedAccounts :: Lens.Lens' DisableResponse (Prelude.Maybe [FailedAccount])
disableResponse_failedAccounts = Lens.lens (\DisableResponse' {failedAccounts} -> failedAccounts) (\s@DisableResponse' {} a -> s {failedAccounts = a} :: DisableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disableResponse_httpStatus :: Lens.Lens' DisableResponse Prelude.Int
disableResponse_httpStatus = Lens.lens (\DisableResponse' {httpStatus} -> httpStatus) (\s@DisableResponse' {} a -> s {httpStatus = a} :: DisableResponse)

-- | Information on the accounts that have had Amazon Inspector scans
-- successfully disabled. Details are provided for each account.
disableResponse_accounts :: Lens.Lens' DisableResponse [Account]
disableResponse_accounts = Lens.lens (\DisableResponse' {accounts} -> accounts) (\s@DisableResponse' {} a -> s {accounts = a} :: DisableResponse) Prelude.. Lens.coerced

instance Prelude.NFData DisableResponse where
  rnf DisableResponse' {..} =
    Prelude.rnf failedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accounts
