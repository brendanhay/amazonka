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
-- Module      : Amazonka.GuardDuty.GetRemainingFreeTrialDays
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the number of days left for each data source used in the free
-- trial period.
module Amazonka.GuardDuty.GetRemainingFreeTrialDays
  ( -- * Creating a Request
    GetRemainingFreeTrialDays (..),
    newGetRemainingFreeTrialDays,

    -- * Request Lenses
    getRemainingFreeTrialDays_accountIds,
    getRemainingFreeTrialDays_detectorId,

    -- * Destructuring the Response
    GetRemainingFreeTrialDaysResponse (..),
    newGetRemainingFreeTrialDaysResponse,

    -- * Response Lenses
    getRemainingFreeTrialDaysResponse_accounts,
    getRemainingFreeTrialDaysResponse_unprocessedAccounts,
    getRemainingFreeTrialDaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRemainingFreeTrialDays' smart constructor.
data GetRemainingFreeTrialDays = GetRemainingFreeTrialDays'
  { -- | A list of account identifiers of the GuardDuty member account.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRemainingFreeTrialDays' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'getRemainingFreeTrialDays_accountIds' - A list of account identifiers of the GuardDuty member account.
--
-- 'detectorId', 'getRemainingFreeTrialDays_detectorId' - The unique ID of the detector of the GuardDuty member account.
newGetRemainingFreeTrialDays ::
  -- | 'detectorId'
  Prelude.Text ->
  GetRemainingFreeTrialDays
newGetRemainingFreeTrialDays pDetectorId_ =
  GetRemainingFreeTrialDays'
    { accountIds =
        Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | A list of account identifiers of the GuardDuty member account.
getRemainingFreeTrialDays_accountIds :: Lens.Lens' GetRemainingFreeTrialDays (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getRemainingFreeTrialDays_accountIds = Lens.lens (\GetRemainingFreeTrialDays' {accountIds} -> accountIds) (\s@GetRemainingFreeTrialDays' {} a -> s {accountIds = a} :: GetRemainingFreeTrialDays) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the detector of the GuardDuty member account.
getRemainingFreeTrialDays_detectorId :: Lens.Lens' GetRemainingFreeTrialDays Prelude.Text
getRemainingFreeTrialDays_detectorId = Lens.lens (\GetRemainingFreeTrialDays' {detectorId} -> detectorId) (\s@GetRemainingFreeTrialDays' {} a -> s {detectorId = a} :: GetRemainingFreeTrialDays)

instance Core.AWSRequest GetRemainingFreeTrialDays where
  type
    AWSResponse GetRemainingFreeTrialDays =
      GetRemainingFreeTrialDaysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRemainingFreeTrialDaysResponse'
            Prelude.<$> (x Data..?> "accounts" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRemainingFreeTrialDays where
  hashWithSalt _salt GetRemainingFreeTrialDays' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData GetRemainingFreeTrialDays where
  rnf GetRemainingFreeTrialDays' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders GetRemainingFreeTrialDays where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRemainingFreeTrialDays where
  toJSON GetRemainingFreeTrialDays' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accountIds" Data..=) Prelude.<$> accountIds]
      )

instance Data.ToPath GetRemainingFreeTrialDays where
  toPath GetRemainingFreeTrialDays' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/freeTrial/daysRemaining"
      ]

instance Data.ToQuery GetRemainingFreeTrialDays where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRemainingFreeTrialDaysResponse' smart constructor.
data GetRemainingFreeTrialDaysResponse = GetRemainingFreeTrialDaysResponse'
  { -- | The member accounts which were included in a request and were processed
    -- successfully.
    accounts :: Prelude.Maybe [AccountFreeTrialInfo],
    -- | The member account that was included in a request but for which the
    -- request could not be processed.
    unprocessedAccounts :: Prelude.Maybe [UnprocessedAccount],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRemainingFreeTrialDaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'getRemainingFreeTrialDaysResponse_accounts' - The member accounts which were included in a request and were processed
-- successfully.
--
-- 'unprocessedAccounts', 'getRemainingFreeTrialDaysResponse_unprocessedAccounts' - The member account that was included in a request but for which the
-- request could not be processed.
--
-- 'httpStatus', 'getRemainingFreeTrialDaysResponse_httpStatus' - The response's http status code.
newGetRemainingFreeTrialDaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRemainingFreeTrialDaysResponse
newGetRemainingFreeTrialDaysResponse pHttpStatus_ =
  GetRemainingFreeTrialDaysResponse'
    { accounts =
        Prelude.Nothing,
      unprocessedAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The member accounts which were included in a request and were processed
-- successfully.
getRemainingFreeTrialDaysResponse_accounts :: Lens.Lens' GetRemainingFreeTrialDaysResponse (Prelude.Maybe [AccountFreeTrialInfo])
getRemainingFreeTrialDaysResponse_accounts = Lens.lens (\GetRemainingFreeTrialDaysResponse' {accounts} -> accounts) (\s@GetRemainingFreeTrialDaysResponse' {} a -> s {accounts = a} :: GetRemainingFreeTrialDaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The member account that was included in a request but for which the
-- request could not be processed.
getRemainingFreeTrialDaysResponse_unprocessedAccounts :: Lens.Lens' GetRemainingFreeTrialDaysResponse (Prelude.Maybe [UnprocessedAccount])
getRemainingFreeTrialDaysResponse_unprocessedAccounts = Lens.lens (\GetRemainingFreeTrialDaysResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@GetRemainingFreeTrialDaysResponse' {} a -> s {unprocessedAccounts = a} :: GetRemainingFreeTrialDaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRemainingFreeTrialDaysResponse_httpStatus :: Lens.Lens' GetRemainingFreeTrialDaysResponse Prelude.Int
getRemainingFreeTrialDaysResponse_httpStatus = Lens.lens (\GetRemainingFreeTrialDaysResponse' {httpStatus} -> httpStatus) (\s@GetRemainingFreeTrialDaysResponse' {} a -> s {httpStatus = a} :: GetRemainingFreeTrialDaysResponse)

instance
  Prelude.NFData
    GetRemainingFreeTrialDaysResponse
  where
  rnf GetRemainingFreeTrialDaysResponse' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf httpStatus
