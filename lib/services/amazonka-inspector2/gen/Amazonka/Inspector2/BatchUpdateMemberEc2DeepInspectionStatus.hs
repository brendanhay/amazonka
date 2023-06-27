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
-- Module      : Amazonka.Inspector2.BatchUpdateMemberEc2DeepInspectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates or deactivates Amazon Inspector deep inspection for the
-- provided member accounts in your organization. You must be the delegated
-- administrator of an organization in Amazon Inspector to use this API.
module Amazonka.Inspector2.BatchUpdateMemberEc2DeepInspectionStatus
  ( -- * Creating a Request
    BatchUpdateMemberEc2DeepInspectionStatus (..),
    newBatchUpdateMemberEc2DeepInspectionStatus,

    -- * Request Lenses
    batchUpdateMemberEc2DeepInspectionStatus_accountIds,

    -- * Destructuring the Response
    BatchUpdateMemberEc2DeepInspectionStatusResponse (..),
    newBatchUpdateMemberEc2DeepInspectionStatusResponse,

    -- * Response Lenses
    batchUpdateMemberEc2DeepInspectionStatusResponse_accountIds,
    batchUpdateMemberEc2DeepInspectionStatusResponse_failedAccountIds,
    batchUpdateMemberEc2DeepInspectionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateMemberEc2DeepInspectionStatus' smart constructor.
data BatchUpdateMemberEc2DeepInspectionStatus = BatchUpdateMemberEc2DeepInspectionStatus'
  { -- | The unique identifiers for the Amazon Web Services accounts to change
    -- Amazon Inspector deep inspection status for.
    accountIds :: [MemberAccountEc2DeepInspectionStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateMemberEc2DeepInspectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'batchUpdateMemberEc2DeepInspectionStatus_accountIds' - The unique identifiers for the Amazon Web Services accounts to change
-- Amazon Inspector deep inspection status for.
newBatchUpdateMemberEc2DeepInspectionStatus ::
  BatchUpdateMemberEc2DeepInspectionStatus
newBatchUpdateMemberEc2DeepInspectionStatus =
  BatchUpdateMemberEc2DeepInspectionStatus'
    { accountIds =
        Prelude.mempty
    }

-- | The unique identifiers for the Amazon Web Services accounts to change
-- Amazon Inspector deep inspection status for.
batchUpdateMemberEc2DeepInspectionStatus_accountIds :: Lens.Lens' BatchUpdateMemberEc2DeepInspectionStatus [MemberAccountEc2DeepInspectionStatus]
batchUpdateMemberEc2DeepInspectionStatus_accountIds = Lens.lens (\BatchUpdateMemberEc2DeepInspectionStatus' {accountIds} -> accountIds) (\s@BatchUpdateMemberEc2DeepInspectionStatus' {} a -> s {accountIds = a} :: BatchUpdateMemberEc2DeepInspectionStatus) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  type
    AWSResponse
      BatchUpdateMemberEc2DeepInspectionStatus =
      BatchUpdateMemberEc2DeepInspectionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateMemberEc2DeepInspectionStatusResponse'
            Prelude.<$> (x Data..?> "accountIds" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "failedAccountIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  hashWithSalt
    _salt
    BatchUpdateMemberEc2DeepInspectionStatus' {..} =
      _salt `Prelude.hashWithSalt` accountIds

instance
  Prelude.NFData
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  rnf BatchUpdateMemberEc2DeepInspectionStatus' {..} =
    Prelude.rnf accountIds

instance
  Data.ToHeaders
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  toJSON BatchUpdateMemberEc2DeepInspectionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Data..= accountIds)]
      )

instance
  Data.ToPath
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  toPath =
    Prelude.const
      "/ec2deepinspectionstatus/member/batch/update"

instance
  Data.ToQuery
    BatchUpdateMemberEc2DeepInspectionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateMemberEc2DeepInspectionStatusResponse' smart constructor.
data BatchUpdateMemberEc2DeepInspectionStatusResponse = BatchUpdateMemberEc2DeepInspectionStatusResponse'
  { -- | An array of objects that provide details for each of the accounts that
    -- Amazon Inspector deep inspection status was successfully changed for.
    accountIds :: Prelude.Maybe [MemberAccountEc2DeepInspectionStatusState],
    -- | An array of objects that provide details for each of the accounts that
    -- Amazon Inspector deep inspection status could not be successfully
    -- changed for.
    failedAccountIds :: Prelude.Maybe [FailedMemberAccountEc2DeepInspectionStatusState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateMemberEc2DeepInspectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'batchUpdateMemberEc2DeepInspectionStatusResponse_accountIds' - An array of objects that provide details for each of the accounts that
-- Amazon Inspector deep inspection status was successfully changed for.
--
-- 'failedAccountIds', 'batchUpdateMemberEc2DeepInspectionStatusResponse_failedAccountIds' - An array of objects that provide details for each of the accounts that
-- Amazon Inspector deep inspection status could not be successfully
-- changed for.
--
-- 'httpStatus', 'batchUpdateMemberEc2DeepInspectionStatusResponse_httpStatus' - The response's http status code.
newBatchUpdateMemberEc2DeepInspectionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateMemberEc2DeepInspectionStatusResponse
newBatchUpdateMemberEc2DeepInspectionStatusResponse
  pHttpStatus_ =
    BatchUpdateMemberEc2DeepInspectionStatusResponse'
      { accountIds =
          Prelude.Nothing,
        failedAccountIds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that provide details for each of the accounts that
-- Amazon Inspector deep inspection status was successfully changed for.
batchUpdateMemberEc2DeepInspectionStatusResponse_accountIds :: Lens.Lens' BatchUpdateMemberEc2DeepInspectionStatusResponse (Prelude.Maybe [MemberAccountEc2DeepInspectionStatusState])
batchUpdateMemberEc2DeepInspectionStatusResponse_accountIds = Lens.lens (\BatchUpdateMemberEc2DeepInspectionStatusResponse' {accountIds} -> accountIds) (\s@BatchUpdateMemberEc2DeepInspectionStatusResponse' {} a -> s {accountIds = a} :: BatchUpdateMemberEc2DeepInspectionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that provide details for each of the accounts that
-- Amazon Inspector deep inspection status could not be successfully
-- changed for.
batchUpdateMemberEc2DeepInspectionStatusResponse_failedAccountIds :: Lens.Lens' BatchUpdateMemberEc2DeepInspectionStatusResponse (Prelude.Maybe [FailedMemberAccountEc2DeepInspectionStatusState])
batchUpdateMemberEc2DeepInspectionStatusResponse_failedAccountIds = Lens.lens (\BatchUpdateMemberEc2DeepInspectionStatusResponse' {failedAccountIds} -> failedAccountIds) (\s@BatchUpdateMemberEc2DeepInspectionStatusResponse' {} a -> s {failedAccountIds = a} :: BatchUpdateMemberEc2DeepInspectionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateMemberEc2DeepInspectionStatusResponse_httpStatus :: Lens.Lens' BatchUpdateMemberEc2DeepInspectionStatusResponse Prelude.Int
batchUpdateMemberEc2DeepInspectionStatusResponse_httpStatus = Lens.lens (\BatchUpdateMemberEc2DeepInspectionStatusResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateMemberEc2DeepInspectionStatusResponse' {} a -> s {httpStatus = a} :: BatchUpdateMemberEc2DeepInspectionStatusResponse)

instance
  Prelude.NFData
    BatchUpdateMemberEc2DeepInspectionStatusResponse
  where
  rnf
    BatchUpdateMemberEc2DeepInspectionStatusResponse' {..} =
      Prelude.rnf accountIds
        `Prelude.seq` Prelude.rnf failedAccountIds
        `Prelude.seq` Prelude.rnf httpStatus
