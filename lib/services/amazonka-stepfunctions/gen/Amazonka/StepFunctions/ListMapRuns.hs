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
-- Module      : Amazonka.StepFunctions.ListMapRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Map Runs that were started by a given state machine execution.
-- Use this API action to obtain Map Run ARNs, and then call
-- @DescribeMapRun@ to obtain more information, if needed.
--
-- This operation returns paginated results.
module Amazonka.StepFunctions.ListMapRuns
  ( -- * Creating a Request
    ListMapRuns (..),
    newListMapRuns,

    -- * Request Lenses
    listMapRuns_maxResults,
    listMapRuns_nextToken,
    listMapRuns_executionArn,

    -- * Destructuring the Response
    ListMapRunsResponse (..),
    newListMapRunsResponse,

    -- * Response Lenses
    listMapRunsResponse_nextToken,
    listMapRunsResponse_httpStatus,
    listMapRunsResponse_mapRuns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListMapRuns' smart constructor.
data ListMapRuns = ListMapRuns'
  { -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the execution for which the Map Runs
    -- must be listed.
    executionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMapRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMapRuns_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'nextToken', 'listMapRuns_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'executionArn', 'listMapRuns_executionArn' - The Amazon Resource Name (ARN) of the execution for which the Map Runs
-- must be listed.
newListMapRuns ::
  -- | 'executionArn'
  Prelude.Text ->
  ListMapRuns
newListMapRuns pExecutionArn_ =
  ListMapRuns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      executionArn = pExecutionArn_
    }

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listMapRuns_maxResults :: Lens.Lens' ListMapRuns (Prelude.Maybe Prelude.Natural)
listMapRuns_maxResults = Lens.lens (\ListMapRuns' {maxResults} -> maxResults) (\s@ListMapRuns' {} a -> s {maxResults = a} :: ListMapRuns)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listMapRuns_nextToken :: Lens.Lens' ListMapRuns (Prelude.Maybe Prelude.Text)
listMapRuns_nextToken = Lens.lens (\ListMapRuns' {nextToken} -> nextToken) (\s@ListMapRuns' {} a -> s {nextToken = a} :: ListMapRuns)

-- | The Amazon Resource Name (ARN) of the execution for which the Map Runs
-- must be listed.
listMapRuns_executionArn :: Lens.Lens' ListMapRuns Prelude.Text
listMapRuns_executionArn = Lens.lens (\ListMapRuns' {executionArn} -> executionArn) (\s@ListMapRuns' {} a -> s {executionArn = a} :: ListMapRuns)

instance Core.AWSPager ListMapRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMapRunsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listMapRunsResponse_mapRuns) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listMapRuns_nextToken
              Lens..~ rs
              Lens.^? listMapRunsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListMapRuns where
  type AWSResponse ListMapRuns = ListMapRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMapRunsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "mapRuns" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListMapRuns where
  hashWithSalt _salt ListMapRuns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` executionArn

instance Prelude.NFData ListMapRuns where
  rnf ListMapRuns' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf executionArn

instance Data.ToHeaders ListMapRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.ListMapRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMapRuns where
  toJSON ListMapRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("executionArn" Data..= executionArn)
          ]
      )

instance Data.ToPath ListMapRuns where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMapRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMapRunsResponse' smart constructor.
data ListMapRunsResponse = ListMapRunsResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array that lists information related to a Map Run, such as the Amazon
    -- Resource Name (ARN) of the Map Run and the ARN of the state machine that
    -- started the Map Run.
    mapRuns :: [MapRunListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMapRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMapRunsResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listMapRunsResponse_httpStatus' - The response's http status code.
--
-- 'mapRuns', 'listMapRunsResponse_mapRuns' - An array that lists information related to a Map Run, such as the Amazon
-- Resource Name (ARN) of the Map Run and the ARN of the state machine that
-- started the Map Run.
newListMapRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMapRunsResponse
newListMapRunsResponse pHttpStatus_ =
  ListMapRunsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      mapRuns = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listMapRunsResponse_nextToken :: Lens.Lens' ListMapRunsResponse (Prelude.Maybe Prelude.Text)
listMapRunsResponse_nextToken = Lens.lens (\ListMapRunsResponse' {nextToken} -> nextToken) (\s@ListMapRunsResponse' {} a -> s {nextToken = a} :: ListMapRunsResponse)

-- | The response's http status code.
listMapRunsResponse_httpStatus :: Lens.Lens' ListMapRunsResponse Prelude.Int
listMapRunsResponse_httpStatus = Lens.lens (\ListMapRunsResponse' {httpStatus} -> httpStatus) (\s@ListMapRunsResponse' {} a -> s {httpStatus = a} :: ListMapRunsResponse)

-- | An array that lists information related to a Map Run, such as the Amazon
-- Resource Name (ARN) of the Map Run and the ARN of the state machine that
-- started the Map Run.
listMapRunsResponse_mapRuns :: Lens.Lens' ListMapRunsResponse [MapRunListItem]
listMapRunsResponse_mapRuns = Lens.lens (\ListMapRunsResponse' {mapRuns} -> mapRuns) (\s@ListMapRunsResponse' {} a -> s {mapRuns = a} :: ListMapRunsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMapRunsResponse where
  rnf ListMapRunsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf mapRuns
