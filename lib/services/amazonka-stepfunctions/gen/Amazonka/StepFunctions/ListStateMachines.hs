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
-- Module      : Amazonka.StepFunctions.ListStateMachines
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing state machines.
--
-- If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- This operation returns paginated results.
module Amazonka.StepFunctions.ListStateMachines
  ( -- * Creating a Request
    ListStateMachines (..),
    newListStateMachines,

    -- * Request Lenses
    listStateMachines_nextToken,
    listStateMachines_maxResults,

    -- * Destructuring the Response
    ListStateMachinesResponse (..),
    newListStateMachinesResponse,

    -- * Response Lenses
    listStateMachinesResponse_nextToken,
    listStateMachinesResponse_httpStatus,
    listStateMachinesResponse_stateMachines,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListStateMachines' smart constructor.
data ListStateMachines = ListStateMachines'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @nextToken@ to obtain further pages of results. The default is 100 and
    -- the maximum allowed page size is 1000. A value of 0 uses the default.
    --
    -- This is only an upper limit. The actual number of results returned per
    -- call might be fewer than the specified maximum.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachines_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'maxResults', 'listStateMachines_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
newListStateMachines ::
  ListStateMachines
newListStateMachines =
  ListStateMachines'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachines_nextToken :: Lens.Lens' ListStateMachines (Prelude.Maybe Prelude.Text)
listStateMachines_nextToken = Lens.lens (\ListStateMachines' {nextToken} -> nextToken) (\s@ListStateMachines' {} a -> s {nextToken = a} :: ListStateMachines)

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listStateMachines_maxResults :: Lens.Lens' ListStateMachines (Prelude.Maybe Prelude.Natural)
listStateMachines_maxResults = Lens.lens (\ListStateMachines' {maxResults} -> maxResults) (\s@ListStateMachines' {} a -> s {maxResults = a} :: ListStateMachines)

instance Core.AWSPager ListStateMachines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStateMachinesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listStateMachinesResponse_stateMachines) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStateMachines_nextToken
          Lens..~ rs
          Lens.^? listStateMachinesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStateMachines where
  type
    AWSResponse ListStateMachines =
      ListStateMachinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStateMachinesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "stateMachines" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListStateMachines where
  hashWithSalt _salt ListStateMachines' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListStateMachines where
  rnf ListStateMachines' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListStateMachines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.ListStateMachines" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStateMachines where
  toJSON ListStateMachines' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListStateMachines where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStateMachines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStateMachinesResponse' smart constructor.
data ListStateMachinesResponse = ListStateMachinesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    stateMachines :: [StateMachineListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachinesResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listStateMachinesResponse_httpStatus' - The response's http status code.
--
-- 'stateMachines', 'listStateMachinesResponse_stateMachines' - Undocumented member.
newListStateMachinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStateMachinesResponse
newListStateMachinesResponse pHttpStatus_ =
  ListStateMachinesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stateMachines = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachinesResponse_nextToken :: Lens.Lens' ListStateMachinesResponse (Prelude.Maybe Prelude.Text)
listStateMachinesResponse_nextToken = Lens.lens (\ListStateMachinesResponse' {nextToken} -> nextToken) (\s@ListStateMachinesResponse' {} a -> s {nextToken = a} :: ListStateMachinesResponse)

-- | The response's http status code.
listStateMachinesResponse_httpStatus :: Lens.Lens' ListStateMachinesResponse Prelude.Int
listStateMachinesResponse_httpStatus = Lens.lens (\ListStateMachinesResponse' {httpStatus} -> httpStatus) (\s@ListStateMachinesResponse' {} a -> s {httpStatus = a} :: ListStateMachinesResponse)

-- | Undocumented member.
listStateMachinesResponse_stateMachines :: Lens.Lens' ListStateMachinesResponse [StateMachineListItem]
listStateMachinesResponse_stateMachines = Lens.lens (\ListStateMachinesResponse' {stateMachines} -> stateMachines) (\s@ListStateMachinesResponse' {} a -> s {stateMachines = a} :: ListStateMachinesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStateMachinesResponse where
  rnf ListStateMachinesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachines
