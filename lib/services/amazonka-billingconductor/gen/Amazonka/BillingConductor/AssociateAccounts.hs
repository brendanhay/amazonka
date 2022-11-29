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
-- Module      : Amazonka.BillingConductor.AssociateAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects an array of account IDs in a consolidated billing family to a
-- predefined billing group. The account IDs must be a part of the
-- consolidated billing family during the current month, and not already
-- associated with another billing group. The maximum number of accounts
-- that can be associated in one call is 30.
module Amazonka.BillingConductor.AssociateAccounts
  ( -- * Creating a Request
    AssociateAccounts (..),
    newAssociateAccounts,

    -- * Request Lenses
    associateAccounts_arn,
    associateAccounts_accountIds,

    -- * Destructuring the Response
    AssociateAccountsResponse (..),
    newAssociateAccountsResponse,

    -- * Response Lenses
    associateAccountsResponse_arn,
    associateAccountsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAccounts' smart constructor.
data AssociateAccounts = AssociateAccounts'
  { -- | The Amazon Resource Name (ARN) of the billing group that associates the
    -- array of account IDs.
    arn :: Prelude.Text,
    -- | The associating array of account IDs.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associateAccounts_arn' - The Amazon Resource Name (ARN) of the billing group that associates the
-- array of account IDs.
--
-- 'accountIds', 'associateAccounts_accountIds' - The associating array of account IDs.
newAssociateAccounts ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  AssociateAccounts
newAssociateAccounts pArn_ pAccountIds_ =
  AssociateAccounts'
    { arn = pArn_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | The Amazon Resource Name (ARN) of the billing group that associates the
-- array of account IDs.
associateAccounts_arn :: Lens.Lens' AssociateAccounts Prelude.Text
associateAccounts_arn = Lens.lens (\AssociateAccounts' {arn} -> arn) (\s@AssociateAccounts' {} a -> s {arn = a} :: AssociateAccounts)

-- | The associating array of account IDs.
associateAccounts_accountIds :: Lens.Lens' AssociateAccounts (Prelude.NonEmpty Prelude.Text)
associateAccounts_accountIds = Lens.lens (\AssociateAccounts' {accountIds} -> accountIds) (\s@AssociateAccounts' {} a -> s {accountIds = a} :: AssociateAccounts) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateAccounts where
  type
    AWSResponse AssociateAccounts =
      AssociateAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateAccountsResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateAccounts where
  hashWithSalt _salt AssociateAccounts' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData AssociateAccounts where
  rnf AssociateAccounts' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accountIds

instance Core.ToHeaders AssociateAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateAccounts where
  toJSON AssociateAccounts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Core..= arn),
            Prelude.Just ("AccountIds" Core..= accountIds)
          ]
      )

instance Core.ToPath AssociateAccounts where
  toPath = Prelude.const "/associate-accounts"

instance Core.ToQuery AssociateAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateAccountsResponse' smart constructor.
data AssociateAccountsResponse = AssociateAccountsResponse'
  { -- | The Amazon Resource Name (ARN) of the billing group that associates the
    -- array of account IDs.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associateAccountsResponse_arn' - The Amazon Resource Name (ARN) of the billing group that associates the
-- array of account IDs.
--
-- 'httpStatus', 'associateAccountsResponse_httpStatus' - The response's http status code.
newAssociateAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateAccountsResponse
newAssociateAccountsResponse pHttpStatus_ =
  AssociateAccountsResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the billing group that associates the
-- array of account IDs.
associateAccountsResponse_arn :: Lens.Lens' AssociateAccountsResponse (Prelude.Maybe Prelude.Text)
associateAccountsResponse_arn = Lens.lens (\AssociateAccountsResponse' {arn} -> arn) (\s@AssociateAccountsResponse' {} a -> s {arn = a} :: AssociateAccountsResponse)

-- | The response's http status code.
associateAccountsResponse_httpStatus :: Lens.Lens' AssociateAccountsResponse Prelude.Int
associateAccountsResponse_httpStatus = Lens.lens (\AssociateAccountsResponse' {httpStatus} -> httpStatus) (\s@AssociateAccountsResponse' {} a -> s {httpStatus = a} :: AssociateAccountsResponse)

instance Prelude.NFData AssociateAccountsResponse where
  rnf AssociateAccountsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
