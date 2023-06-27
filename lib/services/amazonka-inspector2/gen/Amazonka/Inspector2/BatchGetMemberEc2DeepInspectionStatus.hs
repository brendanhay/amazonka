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
-- Module      : Amazonka.Inspector2.BatchGetMemberEc2DeepInspectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves Amazon Inspector deep inspection activation status of multiple
-- member accounts within your organization. You must be the delegated
-- administrator of an organization in Amazon Inspector to use this API.
module Amazonka.Inspector2.BatchGetMemberEc2DeepInspectionStatus
  ( -- * Creating a Request
    BatchGetMemberEc2DeepInspectionStatus (..),
    newBatchGetMemberEc2DeepInspectionStatus,

    -- * Request Lenses
    batchGetMemberEc2DeepInspectionStatus_accountIds,

    -- * Destructuring the Response
    BatchGetMemberEc2DeepInspectionStatusResponse (..),
    newBatchGetMemberEc2DeepInspectionStatusResponse,

    -- * Response Lenses
    batchGetMemberEc2DeepInspectionStatusResponse_accountIds,
    batchGetMemberEc2DeepInspectionStatusResponse_failedAccountIds,
    batchGetMemberEc2DeepInspectionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetMemberEc2DeepInspectionStatus' smart constructor.
data BatchGetMemberEc2DeepInspectionStatus = BatchGetMemberEc2DeepInspectionStatus'
  { -- | The unique identifiers for the Amazon Web Services accounts to retrieve
    -- Amazon Inspector deep inspection activation status for.
    --
    -- >  </p>
    accountIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMemberEc2DeepInspectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'batchGetMemberEc2DeepInspectionStatus_accountIds' - The unique identifiers for the Amazon Web Services accounts to retrieve
-- Amazon Inspector deep inspection activation status for.
--
-- >  </p>
newBatchGetMemberEc2DeepInspectionStatus ::
  BatchGetMemberEc2DeepInspectionStatus
newBatchGetMemberEc2DeepInspectionStatus =
  BatchGetMemberEc2DeepInspectionStatus'
    { accountIds =
        Prelude.Nothing
    }

-- | The unique identifiers for the Amazon Web Services accounts to retrieve
-- Amazon Inspector deep inspection activation status for.
--
-- >  </p>
batchGetMemberEc2DeepInspectionStatus_accountIds :: Lens.Lens' BatchGetMemberEc2DeepInspectionStatus (Prelude.Maybe [Prelude.Text])
batchGetMemberEc2DeepInspectionStatus_accountIds = Lens.lens (\BatchGetMemberEc2DeepInspectionStatus' {accountIds} -> accountIds) (\s@BatchGetMemberEc2DeepInspectionStatus' {} a -> s {accountIds = a} :: BatchGetMemberEc2DeepInspectionStatus) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    BatchGetMemberEc2DeepInspectionStatus
  where
  type
    AWSResponse
      BatchGetMemberEc2DeepInspectionStatus =
      BatchGetMemberEc2DeepInspectionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetMemberEc2DeepInspectionStatusResponse'
            Prelude.<$> (x Data..?> "accountIds" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "failedAccountIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetMemberEc2DeepInspectionStatus
  where
  hashWithSalt
    _salt
    BatchGetMemberEc2DeepInspectionStatus' {..} =
      _salt `Prelude.hashWithSalt` accountIds

instance
  Prelude.NFData
    BatchGetMemberEc2DeepInspectionStatus
  where
  rnf BatchGetMemberEc2DeepInspectionStatus' {..} =
    Prelude.rnf accountIds

instance
  Data.ToHeaders
    BatchGetMemberEc2DeepInspectionStatus
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
    BatchGetMemberEc2DeepInspectionStatus
  where
  toJSON BatchGetMemberEc2DeepInspectionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accountIds" Data..=) Prelude.<$> accountIds]
      )

instance
  Data.ToPath
    BatchGetMemberEc2DeepInspectionStatus
  where
  toPath =
    Prelude.const
      "/ec2deepinspectionstatus/member/batch/get"

instance
  Data.ToQuery
    BatchGetMemberEc2DeepInspectionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetMemberEc2DeepInspectionStatusResponse' smart constructor.
data BatchGetMemberEc2DeepInspectionStatusResponse = BatchGetMemberEc2DeepInspectionStatusResponse'
  { -- | An array of objects that provide details on the activation status of
    -- Amazon Inspector deep inspection for each of the requested accounts.
    --
    -- >  </p>
    accountIds :: Prelude.Maybe [MemberAccountEc2DeepInspectionStatusState],
    -- | An array of objects that provide details on any accounts that failed to
    -- activate Amazon Inspector deep inspection and why.
    --
    -- >  </p>
    failedAccountIds :: Prelude.Maybe [FailedMemberAccountEc2DeepInspectionStatusState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMemberEc2DeepInspectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'batchGetMemberEc2DeepInspectionStatusResponse_accountIds' - An array of objects that provide details on the activation status of
-- Amazon Inspector deep inspection for each of the requested accounts.
--
-- >  </p>
--
-- 'failedAccountIds', 'batchGetMemberEc2DeepInspectionStatusResponse_failedAccountIds' - An array of objects that provide details on any accounts that failed to
-- activate Amazon Inspector deep inspection and why.
--
-- >  </p>
--
-- 'httpStatus', 'batchGetMemberEc2DeepInspectionStatusResponse_httpStatus' - The response's http status code.
newBatchGetMemberEc2DeepInspectionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetMemberEc2DeepInspectionStatusResponse
newBatchGetMemberEc2DeepInspectionStatusResponse
  pHttpStatus_ =
    BatchGetMemberEc2DeepInspectionStatusResponse'
      { accountIds =
          Prelude.Nothing,
        failedAccountIds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that provide details on the activation status of
-- Amazon Inspector deep inspection for each of the requested accounts.
--
-- >  </p>
batchGetMemberEc2DeepInspectionStatusResponse_accountIds :: Lens.Lens' BatchGetMemberEc2DeepInspectionStatusResponse (Prelude.Maybe [MemberAccountEc2DeepInspectionStatusState])
batchGetMemberEc2DeepInspectionStatusResponse_accountIds = Lens.lens (\BatchGetMemberEc2DeepInspectionStatusResponse' {accountIds} -> accountIds) (\s@BatchGetMemberEc2DeepInspectionStatusResponse' {} a -> s {accountIds = a} :: BatchGetMemberEc2DeepInspectionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that provide details on any accounts that failed to
-- activate Amazon Inspector deep inspection and why.
--
-- >  </p>
batchGetMemberEc2DeepInspectionStatusResponse_failedAccountIds :: Lens.Lens' BatchGetMemberEc2DeepInspectionStatusResponse (Prelude.Maybe [FailedMemberAccountEc2DeepInspectionStatusState])
batchGetMemberEc2DeepInspectionStatusResponse_failedAccountIds = Lens.lens (\BatchGetMemberEc2DeepInspectionStatusResponse' {failedAccountIds} -> failedAccountIds) (\s@BatchGetMemberEc2DeepInspectionStatusResponse' {} a -> s {failedAccountIds = a} :: BatchGetMemberEc2DeepInspectionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetMemberEc2DeepInspectionStatusResponse_httpStatus :: Lens.Lens' BatchGetMemberEc2DeepInspectionStatusResponse Prelude.Int
batchGetMemberEc2DeepInspectionStatusResponse_httpStatus = Lens.lens (\BatchGetMemberEc2DeepInspectionStatusResponse' {httpStatus} -> httpStatus) (\s@BatchGetMemberEc2DeepInspectionStatusResponse' {} a -> s {httpStatus = a} :: BatchGetMemberEc2DeepInspectionStatusResponse)

instance
  Prelude.NFData
    BatchGetMemberEc2DeepInspectionStatusResponse
  where
  rnf
    BatchGetMemberEc2DeepInspectionStatusResponse' {..} =
      Prelude.rnf accountIds
        `Prelude.seq` Prelude.rnf failedAccountIds
        `Prelude.seq` Prelude.rnf httpStatus
