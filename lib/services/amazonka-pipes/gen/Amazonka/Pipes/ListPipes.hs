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
-- Module      : Amazonka.Pipes.ListPipes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the pipes associated with this account. For more information about
-- pipes, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-pipes.html Amazon EventBridge Pipes>
-- in the Amazon EventBridge User Guide.
--
-- This operation returns paginated results.
module Amazonka.Pipes.ListPipes
  ( -- * Creating a Request
    ListPipes (..),
    newListPipes,

    -- * Request Lenses
    listPipes_currentState,
    listPipes_desiredState,
    listPipes_limit,
    listPipes_namePrefix,
    listPipes_nextToken,
    listPipes_sourcePrefix,
    listPipes_targetPrefix,

    -- * Destructuring the Response
    ListPipesResponse (..),
    newListPipesResponse,

    -- * Response Lenses
    listPipesResponse_nextToken,
    listPipesResponse_pipes,
    listPipesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPipes' smart constructor.
data ListPipes = ListPipes'
  { -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
    -- | The maximum number of pipes to include in the response.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A value that will return a subset of the pipes associated with this
    -- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
    -- endpoints with \"ABC\" in the name.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The prefix matching the pipe source.
    sourcePrefix :: Prelude.Maybe Prelude.Text,
    -- | The prefix matching the pipe target.
    targetPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentState', 'listPipes_currentState' - The state the pipe is in.
--
-- 'desiredState', 'listPipes_desiredState' - The state the pipe should be in.
--
-- 'limit', 'listPipes_limit' - The maximum number of pipes to include in the response.
--
-- 'namePrefix', 'listPipes_namePrefix' - A value that will return a subset of the pipes associated with this
-- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
-- endpoints with \"ABC\" in the name.
--
-- 'nextToken', 'listPipes_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
--
-- 'sourcePrefix', 'listPipes_sourcePrefix' - The prefix matching the pipe source.
--
-- 'targetPrefix', 'listPipes_targetPrefix' - The prefix matching the pipe target.
newListPipes ::
  ListPipes
newListPipes =
  ListPipes'
    { currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      limit = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourcePrefix = Prelude.Nothing,
      targetPrefix = Prelude.Nothing
    }

-- | The state the pipe is in.
listPipes_currentState :: Lens.Lens' ListPipes (Prelude.Maybe PipeState)
listPipes_currentState = Lens.lens (\ListPipes' {currentState} -> currentState) (\s@ListPipes' {} a -> s {currentState = a} :: ListPipes)

-- | The state the pipe should be in.
listPipes_desiredState :: Lens.Lens' ListPipes (Prelude.Maybe RequestedPipeState)
listPipes_desiredState = Lens.lens (\ListPipes' {desiredState} -> desiredState) (\s@ListPipes' {} a -> s {desiredState = a} :: ListPipes)

-- | The maximum number of pipes to include in the response.
listPipes_limit :: Lens.Lens' ListPipes (Prelude.Maybe Prelude.Natural)
listPipes_limit = Lens.lens (\ListPipes' {limit} -> limit) (\s@ListPipes' {} a -> s {limit = a} :: ListPipes)

-- | A value that will return a subset of the pipes associated with this
-- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
-- endpoints with \"ABC\" in the name.
listPipes_namePrefix :: Lens.Lens' ListPipes (Prelude.Maybe Prelude.Text)
listPipes_namePrefix = Lens.lens (\ListPipes' {namePrefix} -> namePrefix) (\s@ListPipes' {} a -> s {namePrefix = a} :: ListPipes)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
listPipes_nextToken :: Lens.Lens' ListPipes (Prelude.Maybe Prelude.Text)
listPipes_nextToken = Lens.lens (\ListPipes' {nextToken} -> nextToken) (\s@ListPipes' {} a -> s {nextToken = a} :: ListPipes) Prelude.. Lens.mapping Data._Sensitive

-- | The prefix matching the pipe source.
listPipes_sourcePrefix :: Lens.Lens' ListPipes (Prelude.Maybe Prelude.Text)
listPipes_sourcePrefix = Lens.lens (\ListPipes' {sourcePrefix} -> sourcePrefix) (\s@ListPipes' {} a -> s {sourcePrefix = a} :: ListPipes)

-- | The prefix matching the pipe target.
listPipes_targetPrefix :: Lens.Lens' ListPipes (Prelude.Maybe Prelude.Text)
listPipes_targetPrefix = Lens.lens (\ListPipes' {targetPrefix} -> targetPrefix) (\s@ListPipes' {} a -> s {targetPrefix = a} :: ListPipes)

instance Core.AWSPager ListPipes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipesResponse_pipes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPipes_nextToken
          Lens..~ rs
          Lens.^? listPipesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPipes where
  type AWSResponse ListPipes = ListPipesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Pipes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipes where
  hashWithSalt _salt ListPipes' {..} =
    _salt
      `Prelude.hashWithSalt` currentState
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourcePrefix
      `Prelude.hashWithSalt` targetPrefix

instance Prelude.NFData ListPipes where
  rnf ListPipes' {..} =
    Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourcePrefix
      `Prelude.seq` Prelude.rnf targetPrefix

instance Data.ToHeaders ListPipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPipes where
  toPath = Prelude.const "/v1/pipes"

instance Data.ToQuery ListPipes where
  toQuery ListPipes' {..} =
    Prelude.mconcat
      [ "CurrentState" Data.=: currentState,
        "DesiredState" Data.=: desiredState,
        "Limit" Data.=: limit,
        "NamePrefix" Data.=: namePrefix,
        "NextToken" Data.=: nextToken,
        "SourcePrefix" Data.=: sourcePrefix,
        "TargetPrefix" Data.=: targetPrefix
      ]

-- | /See:/ 'newListPipesResponse' smart constructor.
data ListPipesResponse = ListPipesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The pipes returned by the call.
    pipes :: Prelude.Maybe [Pipe],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipesResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
--
-- 'pipes', 'listPipesResponse_pipes' - The pipes returned by the call.
--
-- 'httpStatus', 'listPipesResponse_httpStatus' - The response's http status code.
newListPipesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipesResponse
newListPipesResponse pHttpStatus_ =
  ListPipesResponse'
    { nextToken = Prelude.Nothing,
      pipes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
listPipesResponse_nextToken :: Lens.Lens' ListPipesResponse (Prelude.Maybe Prelude.Text)
listPipesResponse_nextToken = Lens.lens (\ListPipesResponse' {nextToken} -> nextToken) (\s@ListPipesResponse' {} a -> s {nextToken = a} :: ListPipesResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The pipes returned by the call.
listPipesResponse_pipes :: Lens.Lens' ListPipesResponse (Prelude.Maybe [Pipe])
listPipesResponse_pipes = Lens.lens (\ListPipesResponse' {pipes} -> pipes) (\s@ListPipesResponse' {} a -> s {pipes = a} :: ListPipesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPipesResponse_httpStatus :: Lens.Lens' ListPipesResponse Prelude.Int
listPipesResponse_httpStatus = Lens.lens (\ListPipesResponse' {httpStatus} -> httpStatus) (\s@ListPipesResponse' {} a -> s {httpStatus = a} :: ListPipesResponse)

instance Prelude.NFData ListPipesResponse where
  rnf ListPipesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pipes
      `Prelude.seq` Prelude.rnf httpStatus
