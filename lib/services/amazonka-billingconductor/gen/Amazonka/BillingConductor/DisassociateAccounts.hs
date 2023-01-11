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
-- Module      : Amazonka.BillingConductor.DisassociateAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified list of account IDs from the given billing group.
module Amazonka.BillingConductor.DisassociateAccounts
  ( -- * Creating a Request
    DisassociateAccounts (..),
    newDisassociateAccounts,

    -- * Request Lenses
    disassociateAccounts_arn,
    disassociateAccounts_accountIds,

    -- * Destructuring the Response
    DisassociateAccountsResponse (..),
    newDisassociateAccountsResponse,

    -- * Response Lenses
    disassociateAccountsResponse_arn,
    disassociateAccountsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateAccounts' smart constructor.
data DisassociateAccounts = DisassociateAccounts'
  { -- | The Amazon Resource Name (ARN) of the billing group that the array of
    -- account IDs will disassociate from.
    arn :: Prelude.Text,
    -- | The array of account IDs to disassociate.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociateAccounts_arn' - The Amazon Resource Name (ARN) of the billing group that the array of
-- account IDs will disassociate from.
--
-- 'accountIds', 'disassociateAccounts_accountIds' - The array of account IDs to disassociate.
newDisassociateAccounts ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateAccounts
newDisassociateAccounts pArn_ pAccountIds_ =
  DisassociateAccounts'
    { arn = pArn_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The Amazon Resource Name (ARN) of the billing group that the array of
-- account IDs will disassociate from.
disassociateAccounts_arn :: Lens.Lens' DisassociateAccounts Prelude.Text
disassociateAccounts_arn = Lens.lens (\DisassociateAccounts' {arn} -> arn) (\s@DisassociateAccounts' {} a -> s {arn = a} :: DisassociateAccounts)

-- | The array of account IDs to disassociate.
disassociateAccounts_accountIds :: Lens.Lens' DisassociateAccounts (Prelude.NonEmpty Prelude.Text)
disassociateAccounts_accountIds = Lens.lens (\DisassociateAccounts' {accountIds} -> accountIds) (\s@DisassociateAccounts' {} a -> s {accountIds = a} :: DisassociateAccounts) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateAccounts where
  type
    AWSResponse DisassociateAccounts =
      DisassociateAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateAccountsResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateAccounts where
  hashWithSalt _salt DisassociateAccounts' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData DisassociateAccounts where
  rnf DisassociateAccounts' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accountIds

instance Data.ToHeaders DisassociateAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateAccounts where
  toJSON DisassociateAccounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just ("AccountIds" Data..= accountIds)
          ]
      )

instance Data.ToPath DisassociateAccounts where
  toPath = Prelude.const "/disassociate-accounts"

instance Data.ToQuery DisassociateAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateAccountsResponse' smart constructor.
data DisassociateAccountsResponse = DisassociateAccountsResponse'
  { -- | The Amazon Resource Name (ARN) of the billing group that the array of
    -- account IDs is disassociated from.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociateAccountsResponse_arn' - The Amazon Resource Name (ARN) of the billing group that the array of
-- account IDs is disassociated from.
--
-- 'httpStatus', 'disassociateAccountsResponse_httpStatus' - The response's http status code.
newDisassociateAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateAccountsResponse
newDisassociateAccountsResponse pHttpStatus_ =
  DisassociateAccountsResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the billing group that the array of
-- account IDs is disassociated from.
disassociateAccountsResponse_arn :: Lens.Lens' DisassociateAccountsResponse (Prelude.Maybe Prelude.Text)
disassociateAccountsResponse_arn = Lens.lens (\DisassociateAccountsResponse' {arn} -> arn) (\s@DisassociateAccountsResponse' {} a -> s {arn = a} :: DisassociateAccountsResponse)

-- | The response's http status code.
disassociateAccountsResponse_httpStatus :: Lens.Lens' DisassociateAccountsResponse Prelude.Int
disassociateAccountsResponse_httpStatus = Lens.lens (\DisassociateAccountsResponse' {httpStatus} -> httpStatus) (\s@DisassociateAccountsResponse' {} a -> s {httpStatus = a} :: DisassociateAccountsResponse)

instance Prelude.NFData DisassociateAccountsResponse where
  rnf DisassociateAccountsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
