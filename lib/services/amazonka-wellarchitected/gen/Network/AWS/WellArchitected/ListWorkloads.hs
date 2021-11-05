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
-- Module      : Network.AWS.WellArchitected.ListWorkloads
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List workloads. Paginated.
module Network.AWS.WellArchitected.ListWorkloads
  ( -- * Creating a Request
    ListWorkloads (..),
    newListWorkloads,

    -- * Request Lenses
    listWorkloads_nextToken,
    listWorkloads_workloadNamePrefix,
    listWorkloads_maxResults,

    -- * Destructuring the Response
    ListWorkloadsResponse (..),
    newListWorkloadsResponse,

    -- * Response Lenses
    listWorkloadsResponse_workloadSummaries,
    listWorkloadsResponse_nextToken,
    listWorkloadsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WellArchitected.Types

-- | Input to list all workloads.
--
-- /See:/ 'newListWorkloads' smart constructor.
data ListWorkloads = ListWorkloads'
  { nextToken :: Prelude.Maybe Prelude.Text,
    workloadNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkloads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkloads_nextToken' - Undocumented member.
--
-- 'workloadNamePrefix', 'listWorkloads_workloadNamePrefix' - Undocumented member.
--
-- 'maxResults', 'listWorkloads_maxResults' - The maximum number of results to return for this request.
newListWorkloads ::
  ListWorkloads
newListWorkloads =
  ListWorkloads'
    { nextToken = Prelude.Nothing,
      workloadNamePrefix = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listWorkloads_nextToken :: Lens.Lens' ListWorkloads (Prelude.Maybe Prelude.Text)
listWorkloads_nextToken = Lens.lens (\ListWorkloads' {nextToken} -> nextToken) (\s@ListWorkloads' {} a -> s {nextToken = a} :: ListWorkloads)

-- | Undocumented member.
listWorkloads_workloadNamePrefix :: Lens.Lens' ListWorkloads (Prelude.Maybe Prelude.Text)
listWorkloads_workloadNamePrefix = Lens.lens (\ListWorkloads' {workloadNamePrefix} -> workloadNamePrefix) (\s@ListWorkloads' {} a -> s {workloadNamePrefix = a} :: ListWorkloads)

-- | The maximum number of results to return for this request.
listWorkloads_maxResults :: Lens.Lens' ListWorkloads (Prelude.Maybe Prelude.Natural)
listWorkloads_maxResults = Lens.lens (\ListWorkloads' {maxResults} -> maxResults) (\s@ListWorkloads' {} a -> s {maxResults = a} :: ListWorkloads)

instance Core.AWSRequest ListWorkloads where
  type
    AWSResponse ListWorkloads =
      ListWorkloadsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkloadsResponse'
            Prelude.<$> ( x Core..?> "WorkloadSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkloads

instance Prelude.NFData ListWorkloads

instance Core.ToHeaders ListWorkloads where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWorkloads where
  toJSON ListWorkloads' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("WorkloadNamePrefix" Core..=)
              Prelude.<$> workloadNamePrefix,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListWorkloads where
  toPath = Prelude.const "/workloadsSummaries"

instance Core.ToQuery ListWorkloads where
  toQuery = Prelude.const Prelude.mempty

-- | Output of a list workloads call.
--
-- /See:/ 'newListWorkloadsResponse' smart constructor.
data ListWorkloadsResponse = ListWorkloadsResponse'
  { workloadSummaries :: Prelude.Maybe [WorkloadSummary],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkloadsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadSummaries', 'listWorkloadsResponse_workloadSummaries' - Undocumented member.
--
-- 'nextToken', 'listWorkloadsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listWorkloadsResponse_httpStatus' - The response's http status code.
newListWorkloadsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkloadsResponse
newListWorkloadsResponse pHttpStatus_ =
  ListWorkloadsResponse'
    { workloadSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkloadsResponse_workloadSummaries :: Lens.Lens' ListWorkloadsResponse (Prelude.Maybe [WorkloadSummary])
listWorkloadsResponse_workloadSummaries = Lens.lens (\ListWorkloadsResponse' {workloadSummaries} -> workloadSummaries) (\s@ListWorkloadsResponse' {} a -> s {workloadSummaries = a} :: ListWorkloadsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listWorkloadsResponse_nextToken :: Lens.Lens' ListWorkloadsResponse (Prelude.Maybe Prelude.Text)
listWorkloadsResponse_nextToken = Lens.lens (\ListWorkloadsResponse' {nextToken} -> nextToken) (\s@ListWorkloadsResponse' {} a -> s {nextToken = a} :: ListWorkloadsResponse)

-- | The response's http status code.
listWorkloadsResponse_httpStatus :: Lens.Lens' ListWorkloadsResponse Prelude.Int
listWorkloadsResponse_httpStatus = Lens.lens (\ListWorkloadsResponse' {httpStatus} -> httpStatus) (\s@ListWorkloadsResponse' {} a -> s {httpStatus = a} :: ListWorkloadsResponse)

instance Prelude.NFData ListWorkloadsResponse
