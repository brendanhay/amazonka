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
-- Module      : Amazonka.Synthetics.GetCanaryRuns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of runs for a specified canary.
module Amazonka.Synthetics.GetCanaryRuns
  ( -- * Creating a Request
    GetCanaryRuns (..),
    newGetCanaryRuns,

    -- * Request Lenses
    getCanaryRuns_nextToken,
    getCanaryRuns_maxResults,
    getCanaryRuns_name,

    -- * Destructuring the Response
    GetCanaryRunsResponse (..),
    newGetCanaryRunsResponse,

    -- * Response Lenses
    getCanaryRunsResponse_nextToken,
    getCanaryRunsResponse_canaryRuns,
    getCanaryRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newGetCanaryRuns' smart constructor.
data GetCanaryRuns = GetCanaryRuns'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify this parameter to limit how many runs are returned each time you
    -- use the @GetCanaryRuns@ operation. If you omit this parameter, the
    -- default of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the canary that you want to see runs for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCanaryRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCanaryRuns_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
-- next set of results.
--
-- 'maxResults', 'getCanaryRuns_maxResults' - Specify this parameter to limit how many runs are returned each time you
-- use the @GetCanaryRuns@ operation. If you omit this parameter, the
-- default of 100 is used.
--
-- 'name', 'getCanaryRuns_name' - The name of the canary that you want to see runs for.
newGetCanaryRuns ::
  -- | 'name'
  Prelude.Text ->
  GetCanaryRuns
newGetCanaryRuns pName_ =
  GetCanaryRuns'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
-- next set of results.
getCanaryRuns_nextToken :: Lens.Lens' GetCanaryRuns (Prelude.Maybe Prelude.Text)
getCanaryRuns_nextToken = Lens.lens (\GetCanaryRuns' {nextToken} -> nextToken) (\s@GetCanaryRuns' {} a -> s {nextToken = a} :: GetCanaryRuns)

-- | Specify this parameter to limit how many runs are returned each time you
-- use the @GetCanaryRuns@ operation. If you omit this parameter, the
-- default of 100 is used.
getCanaryRuns_maxResults :: Lens.Lens' GetCanaryRuns (Prelude.Maybe Prelude.Natural)
getCanaryRuns_maxResults = Lens.lens (\GetCanaryRuns' {maxResults} -> maxResults) (\s@GetCanaryRuns' {} a -> s {maxResults = a} :: GetCanaryRuns)

-- | The name of the canary that you want to see runs for.
getCanaryRuns_name :: Lens.Lens' GetCanaryRuns Prelude.Text
getCanaryRuns_name = Lens.lens (\GetCanaryRuns' {name} -> name) (\s@GetCanaryRuns' {} a -> s {name = a} :: GetCanaryRuns)

instance Core.AWSRequest GetCanaryRuns where
  type
    AWSResponse GetCanaryRuns =
      GetCanaryRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCanaryRunsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "CanaryRuns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCanaryRuns where
  hashWithSalt _salt GetCanaryRuns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetCanaryRuns where
  rnf GetCanaryRuns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetCanaryRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCanaryRuns where
  toJSON GetCanaryRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath GetCanaryRuns where
  toPath GetCanaryRuns' {..} =
    Prelude.mconcat
      ["/canary/", Data.toBS name, "/runs"]

instance Data.ToQuery GetCanaryRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCanaryRunsResponse' smart constructor.
data GetCanaryRunsResponse = GetCanaryRunsResponse'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of structures. Each structure contains the details of one of
    -- the retrieved canary runs.
    canaryRuns :: Prelude.Maybe [CanaryRun],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCanaryRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCanaryRunsResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
-- next set of results.
--
-- 'canaryRuns', 'getCanaryRunsResponse_canaryRuns' - An array of structures. Each structure contains the details of one of
-- the retrieved canary runs.
--
-- 'httpStatus', 'getCanaryRunsResponse_httpStatus' - The response's http status code.
newGetCanaryRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCanaryRunsResponse
newGetCanaryRunsResponse pHttpStatus_ =
  GetCanaryRunsResponse'
    { nextToken = Prelude.Nothing,
      canaryRuns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @GetCanaryRuns@ operation to retrieve the
-- next set of results.
getCanaryRunsResponse_nextToken :: Lens.Lens' GetCanaryRunsResponse (Prelude.Maybe Prelude.Text)
getCanaryRunsResponse_nextToken = Lens.lens (\GetCanaryRunsResponse' {nextToken} -> nextToken) (\s@GetCanaryRunsResponse' {} a -> s {nextToken = a} :: GetCanaryRunsResponse)

-- | An array of structures. Each structure contains the details of one of
-- the retrieved canary runs.
getCanaryRunsResponse_canaryRuns :: Lens.Lens' GetCanaryRunsResponse (Prelude.Maybe [CanaryRun])
getCanaryRunsResponse_canaryRuns = Lens.lens (\GetCanaryRunsResponse' {canaryRuns} -> canaryRuns) (\s@GetCanaryRunsResponse' {} a -> s {canaryRuns = a} :: GetCanaryRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCanaryRunsResponse_httpStatus :: Lens.Lens' GetCanaryRunsResponse Prelude.Int
getCanaryRunsResponse_httpStatus = Lens.lens (\GetCanaryRunsResponse' {httpStatus} -> httpStatus) (\s@GetCanaryRunsResponse' {} a -> s {httpStatus = a} :: GetCanaryRunsResponse)

instance Prelude.NFData GetCanaryRunsResponse where
  rnf GetCanaryRunsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf canaryRuns
      `Prelude.seq` Prelude.rnf httpStatus
