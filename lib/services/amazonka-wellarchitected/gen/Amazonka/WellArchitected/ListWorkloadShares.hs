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
-- Module      : Amazonka.WellArchitected.ListWorkloadShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the workload shares associated with the workload.
module Amazonka.WellArchitected.ListWorkloadShares
  ( -- * Creating a Request
    ListWorkloadShares (..),
    newListWorkloadShares,

    -- * Request Lenses
    listWorkloadShares_maxResults,
    listWorkloadShares_nextToken,
    listWorkloadShares_sharedWithPrefix,
    listWorkloadShares_status,
    listWorkloadShares_workloadId,

    -- * Destructuring the Response
    ListWorkloadSharesResponse (..),
    newListWorkloadSharesResponse,

    -- * Response Lenses
    listWorkloadSharesResponse_nextToken,
    listWorkloadSharesResponse_workloadId,
    listWorkloadSharesResponse_workloadShareSummaries,
    listWorkloadSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for List Workload Share
--
-- /See:/ 'newListWorkloadShares' smart constructor.
data ListWorkloadShares = ListWorkloadShares'
  { -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID, IAM role, organization ID, or
    -- organizational unit (OU) ID with which the workload is shared.
    sharedWithPrefix :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ShareStatus,
    workloadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkloadShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkloadShares_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'listWorkloadShares_nextToken' - Undocumented member.
--
-- 'sharedWithPrefix', 'listWorkloadShares_sharedWithPrefix' - The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the workload is shared.
--
-- 'status', 'listWorkloadShares_status' - Undocumented member.
--
-- 'workloadId', 'listWorkloadShares_workloadId' - Undocumented member.
newListWorkloadShares ::
  -- | 'workloadId'
  Prelude.Text ->
  ListWorkloadShares
newListWorkloadShares pWorkloadId_ =
  ListWorkloadShares'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sharedWithPrefix = Prelude.Nothing,
      status = Prelude.Nothing,
      workloadId = pWorkloadId_
    }

-- | The maximum number of results to return for this request.
listWorkloadShares_maxResults :: Lens.Lens' ListWorkloadShares (Prelude.Maybe Prelude.Natural)
listWorkloadShares_maxResults = Lens.lens (\ListWorkloadShares' {maxResults} -> maxResults) (\s@ListWorkloadShares' {} a -> s {maxResults = a} :: ListWorkloadShares)

-- | Undocumented member.
listWorkloadShares_nextToken :: Lens.Lens' ListWorkloadShares (Prelude.Maybe Prelude.Text)
listWorkloadShares_nextToken = Lens.lens (\ListWorkloadShares' {nextToken} -> nextToken) (\s@ListWorkloadShares' {} a -> s {nextToken = a} :: ListWorkloadShares)

-- | The Amazon Web Services account ID, IAM role, organization ID, or
-- organizational unit (OU) ID with which the workload is shared.
listWorkloadShares_sharedWithPrefix :: Lens.Lens' ListWorkloadShares (Prelude.Maybe Prelude.Text)
listWorkloadShares_sharedWithPrefix = Lens.lens (\ListWorkloadShares' {sharedWithPrefix} -> sharedWithPrefix) (\s@ListWorkloadShares' {} a -> s {sharedWithPrefix = a} :: ListWorkloadShares)

-- | Undocumented member.
listWorkloadShares_status :: Lens.Lens' ListWorkloadShares (Prelude.Maybe ShareStatus)
listWorkloadShares_status = Lens.lens (\ListWorkloadShares' {status} -> status) (\s@ListWorkloadShares' {} a -> s {status = a} :: ListWorkloadShares)

-- | Undocumented member.
listWorkloadShares_workloadId :: Lens.Lens' ListWorkloadShares Prelude.Text
listWorkloadShares_workloadId = Lens.lens (\ListWorkloadShares' {workloadId} -> workloadId) (\s@ListWorkloadShares' {} a -> s {workloadId = a} :: ListWorkloadShares)

instance Core.AWSRequest ListWorkloadShares where
  type
    AWSResponse ListWorkloadShares =
      ListWorkloadSharesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkloadSharesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> ( x
                            Data..?> "WorkloadShareSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkloadShares where
  hashWithSalt _salt ListWorkloadShares' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sharedWithPrefix
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData ListWorkloadShares where
  rnf ListWorkloadShares' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sharedWithPrefix
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workloadId

instance Data.ToHeaders ListWorkloadShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkloadShares where
  toPath ListWorkloadShares' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId, "/shares"]

instance Data.ToQuery ListWorkloadShares where
  toQuery ListWorkloadShares' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SharedWithPrefix" Data.=: sharedWithPrefix,
        "Status" Data.=: status
      ]

-- | Input for List Workload Share
--
-- /See:/ 'newListWorkloadSharesResponse' smart constructor.
data ListWorkloadSharesResponse = ListWorkloadSharesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text,
    workloadShareSummaries :: Prelude.Maybe [WorkloadShareSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkloadSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkloadSharesResponse_nextToken' - Undocumented member.
--
-- 'workloadId', 'listWorkloadSharesResponse_workloadId' - Undocumented member.
--
-- 'workloadShareSummaries', 'listWorkloadSharesResponse_workloadShareSummaries' - Undocumented member.
--
-- 'httpStatus', 'listWorkloadSharesResponse_httpStatus' - The response's http status code.
newListWorkloadSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkloadSharesResponse
newListWorkloadSharesResponse pHttpStatus_ =
  ListWorkloadSharesResponse'
    { nextToken =
        Prelude.Nothing,
      workloadId = Prelude.Nothing,
      workloadShareSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkloadSharesResponse_nextToken :: Lens.Lens' ListWorkloadSharesResponse (Prelude.Maybe Prelude.Text)
listWorkloadSharesResponse_nextToken = Lens.lens (\ListWorkloadSharesResponse' {nextToken} -> nextToken) (\s@ListWorkloadSharesResponse' {} a -> s {nextToken = a} :: ListWorkloadSharesResponse)

-- | Undocumented member.
listWorkloadSharesResponse_workloadId :: Lens.Lens' ListWorkloadSharesResponse (Prelude.Maybe Prelude.Text)
listWorkloadSharesResponse_workloadId = Lens.lens (\ListWorkloadSharesResponse' {workloadId} -> workloadId) (\s@ListWorkloadSharesResponse' {} a -> s {workloadId = a} :: ListWorkloadSharesResponse)

-- | Undocumented member.
listWorkloadSharesResponse_workloadShareSummaries :: Lens.Lens' ListWorkloadSharesResponse (Prelude.Maybe [WorkloadShareSummary])
listWorkloadSharesResponse_workloadShareSummaries = Lens.lens (\ListWorkloadSharesResponse' {workloadShareSummaries} -> workloadShareSummaries) (\s@ListWorkloadSharesResponse' {} a -> s {workloadShareSummaries = a} :: ListWorkloadSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkloadSharesResponse_httpStatus :: Lens.Lens' ListWorkloadSharesResponse Prelude.Int
listWorkloadSharesResponse_httpStatus = Lens.lens (\ListWorkloadSharesResponse' {httpStatus} -> httpStatus) (\s@ListWorkloadSharesResponse' {} a -> s {httpStatus = a} :: ListWorkloadSharesResponse)

instance Prelude.NFData ListWorkloadSharesResponse where
  rnf ListWorkloadSharesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf workloadShareSummaries
      `Prelude.seq` Prelude.rnf httpStatus
