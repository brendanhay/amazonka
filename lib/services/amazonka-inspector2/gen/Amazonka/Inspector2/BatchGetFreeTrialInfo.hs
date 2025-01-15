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
-- Module      : Amazonka.Inspector2.BatchGetFreeTrialInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets free trial status for multiple Amazon Web Services accounts.
module Amazonka.Inspector2.BatchGetFreeTrialInfo
  ( -- * Creating a Request
    BatchGetFreeTrialInfo (..),
    newBatchGetFreeTrialInfo,

    -- * Request Lenses
    batchGetFreeTrialInfo_accountIds,

    -- * Destructuring the Response
    BatchGetFreeTrialInfoResponse (..),
    newBatchGetFreeTrialInfoResponse,

    -- * Response Lenses
    batchGetFreeTrialInfoResponse_httpStatus,
    batchGetFreeTrialInfoResponse_accounts,
    batchGetFreeTrialInfoResponse_failedAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetFreeTrialInfo' smart constructor.
data BatchGetFreeTrialInfo = BatchGetFreeTrialInfo'
  { -- | The account IDs to get free trial status for.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetFreeTrialInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'batchGetFreeTrialInfo_accountIds' - The account IDs to get free trial status for.
newBatchGetFreeTrialInfo ::
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetFreeTrialInfo
newBatchGetFreeTrialInfo pAccountIds_ =
  BatchGetFreeTrialInfo'
    { accountIds =
        Lens.coerced Lens.# pAccountIds_
    }

-- | The account IDs to get free trial status for.
batchGetFreeTrialInfo_accountIds :: Lens.Lens' BatchGetFreeTrialInfo (Prelude.NonEmpty Prelude.Text)
batchGetFreeTrialInfo_accountIds = Lens.lens (\BatchGetFreeTrialInfo' {accountIds} -> accountIds) (\s@BatchGetFreeTrialInfo' {} a -> s {accountIds = a} :: BatchGetFreeTrialInfo) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetFreeTrialInfo where
  type
    AWSResponse BatchGetFreeTrialInfo =
      BatchGetFreeTrialInfoResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetFreeTrialInfoResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "accounts" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "failedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchGetFreeTrialInfo where
  hashWithSalt _salt BatchGetFreeTrialInfo' {..} =
    _salt `Prelude.hashWithSalt` accountIds

instance Prelude.NFData BatchGetFreeTrialInfo where
  rnf BatchGetFreeTrialInfo' {..} =
    Prelude.rnf accountIds

instance Data.ToHeaders BatchGetFreeTrialInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetFreeTrialInfo where
  toJSON BatchGetFreeTrialInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Data..= accountIds)]
      )

instance Data.ToPath BatchGetFreeTrialInfo where
  toPath = Prelude.const "/freetrialinfo/batchget"

instance Data.ToQuery BatchGetFreeTrialInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetFreeTrialInfoResponse' smart constructor.
data BatchGetFreeTrialInfoResponse = BatchGetFreeTrialInfoResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of objects that provide Amazon Inspector free trial details for
    -- each of the requested accounts.
    accounts :: [FreeTrialAccountInfo],
    -- | An array of objects detailing any accounts that free trial data could
    -- not be returned for.
    failedAccounts :: [FreeTrialInfoError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetFreeTrialInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchGetFreeTrialInfoResponse_httpStatus' - The response's http status code.
--
-- 'accounts', 'batchGetFreeTrialInfoResponse_accounts' - An array of objects that provide Amazon Inspector free trial details for
-- each of the requested accounts.
--
-- 'failedAccounts', 'batchGetFreeTrialInfoResponse_failedAccounts' - An array of objects detailing any accounts that free trial data could
-- not be returned for.
newBatchGetFreeTrialInfoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetFreeTrialInfoResponse
newBatchGetFreeTrialInfoResponse pHttpStatus_ =
  BatchGetFreeTrialInfoResponse'
    { httpStatus =
        pHttpStatus_,
      accounts = Prelude.mempty,
      failedAccounts = Prelude.mempty
    }

-- | The response's http status code.
batchGetFreeTrialInfoResponse_httpStatus :: Lens.Lens' BatchGetFreeTrialInfoResponse Prelude.Int
batchGetFreeTrialInfoResponse_httpStatus = Lens.lens (\BatchGetFreeTrialInfoResponse' {httpStatus} -> httpStatus) (\s@BatchGetFreeTrialInfoResponse' {} a -> s {httpStatus = a} :: BatchGetFreeTrialInfoResponse)

-- | An array of objects that provide Amazon Inspector free trial details for
-- each of the requested accounts.
batchGetFreeTrialInfoResponse_accounts :: Lens.Lens' BatchGetFreeTrialInfoResponse [FreeTrialAccountInfo]
batchGetFreeTrialInfoResponse_accounts = Lens.lens (\BatchGetFreeTrialInfoResponse' {accounts} -> accounts) (\s@BatchGetFreeTrialInfoResponse' {} a -> s {accounts = a} :: BatchGetFreeTrialInfoResponse) Prelude.. Lens.coerced

-- | An array of objects detailing any accounts that free trial data could
-- not be returned for.
batchGetFreeTrialInfoResponse_failedAccounts :: Lens.Lens' BatchGetFreeTrialInfoResponse [FreeTrialInfoError]
batchGetFreeTrialInfoResponse_failedAccounts = Lens.lens (\BatchGetFreeTrialInfoResponse' {failedAccounts} -> failedAccounts) (\s@BatchGetFreeTrialInfoResponse' {} a -> s {failedAccounts = a} :: BatchGetFreeTrialInfoResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchGetFreeTrialInfoResponse where
  rnf BatchGetFreeTrialInfoResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf accounts `Prelude.seq`
        Prelude.rnf failedAccounts
