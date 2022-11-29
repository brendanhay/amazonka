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
-- Module      : Amazonka.WellArchitected.ListWorkloads
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List workloads. Paginated.
module Amazonka.WellArchitected.ListWorkloads
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
    listWorkloadsResponse_nextToken,
    listWorkloadsResponse_workloadSummaries,
    listWorkloadsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkloadsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "WorkloadSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkloads where
  hashWithSalt _salt ListWorkloads' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workloadNamePrefix
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWorkloads where
  rnf ListWorkloads' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadNamePrefix
      `Prelude.seq` Prelude.rnf maxResults

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
  { nextToken :: Prelude.Maybe Prelude.Text,
    workloadSummaries :: Prelude.Maybe [WorkloadSummary],
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
-- 'nextToken', 'listWorkloadsResponse_nextToken' - Undocumented member.
--
-- 'workloadSummaries', 'listWorkloadsResponse_workloadSummaries' - Undocumented member.
--
-- 'httpStatus', 'listWorkloadsResponse_httpStatus' - The response's http status code.
newListWorkloadsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkloadsResponse
newListWorkloadsResponse pHttpStatus_ =
  ListWorkloadsResponse'
    { nextToken = Prelude.Nothing,
      workloadSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkloadsResponse_nextToken :: Lens.Lens' ListWorkloadsResponse (Prelude.Maybe Prelude.Text)
listWorkloadsResponse_nextToken = Lens.lens (\ListWorkloadsResponse' {nextToken} -> nextToken) (\s@ListWorkloadsResponse' {} a -> s {nextToken = a} :: ListWorkloadsResponse)

-- | Undocumented member.
listWorkloadsResponse_workloadSummaries :: Lens.Lens' ListWorkloadsResponse (Prelude.Maybe [WorkloadSummary])
listWorkloadsResponse_workloadSummaries = Lens.lens (\ListWorkloadsResponse' {workloadSummaries} -> workloadSummaries) (\s@ListWorkloadsResponse' {} a -> s {workloadSummaries = a} :: ListWorkloadsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkloadsResponse_httpStatus :: Lens.Lens' ListWorkloadsResponse Prelude.Int
listWorkloadsResponse_httpStatus = Lens.lens (\ListWorkloadsResponse' {httpStatus} -> httpStatus) (\s@ListWorkloadsResponse' {} a -> s {httpStatus = a} :: ListWorkloadsResponse)

instance Prelude.NFData ListWorkloadsResponse where
  rnf ListWorkloadsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadSummaries
      `Prelude.seq` Prelude.rnf httpStatus
