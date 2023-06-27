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
-- Module      : Amazonka.StepFunctions.ListStateMachineVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html versions>
-- for the specified state machine Amazon Resource Name (ARN).
--
-- The results are sorted in descending order of the version creation time.
--
-- If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- __Related operations:__
--
-- -   PublishStateMachineVersion
--
-- -   DeleteStateMachineVersion
module Amazonka.StepFunctions.ListStateMachineVersions
  ( -- * Creating a Request
    ListStateMachineVersions (..),
    newListStateMachineVersions,

    -- * Request Lenses
    listStateMachineVersions_maxResults,
    listStateMachineVersions_nextToken,
    listStateMachineVersions_stateMachineArn,

    -- * Destructuring the Response
    ListStateMachineVersionsResponse (..),
    newListStateMachineVersionsResponse,

    -- * Response Lenses
    listStateMachineVersionsResponse_nextToken,
    listStateMachineVersionsResponse_httpStatus,
    listStateMachineVersionsResponse_stateMachineVersions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListStateMachineVersions' smart constructor.
data ListStateMachineVersions = ListStateMachineVersions'
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
    -- | The Amazon Resource Name (ARN) of the state machine.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStateMachineVersions_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'nextToken', 'listStateMachineVersions_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'stateMachineArn', 'listStateMachineVersions_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine.
newListStateMachineVersions ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  ListStateMachineVersions
newListStateMachineVersions pStateMachineArn_ =
  ListStateMachineVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateMachineArn = pStateMachineArn_
    }

-- | The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
listStateMachineVersions_maxResults :: Lens.Lens' ListStateMachineVersions (Prelude.Maybe Prelude.Natural)
listStateMachineVersions_maxResults = Lens.lens (\ListStateMachineVersions' {maxResults} -> maxResults) (\s@ListStateMachineVersions' {} a -> s {maxResults = a} :: ListStateMachineVersions)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachineVersions_nextToken :: Lens.Lens' ListStateMachineVersions (Prelude.Maybe Prelude.Text)
listStateMachineVersions_nextToken = Lens.lens (\ListStateMachineVersions' {nextToken} -> nextToken) (\s@ListStateMachineVersions' {} a -> s {nextToken = a} :: ListStateMachineVersions)

-- | The Amazon Resource Name (ARN) of the state machine.
listStateMachineVersions_stateMachineArn :: Lens.Lens' ListStateMachineVersions Prelude.Text
listStateMachineVersions_stateMachineArn = Lens.lens (\ListStateMachineVersions' {stateMachineArn} -> stateMachineArn) (\s@ListStateMachineVersions' {} a -> s {stateMachineArn = a} :: ListStateMachineVersions)

instance Core.AWSRequest ListStateMachineVersions where
  type
    AWSResponse ListStateMachineVersions =
      ListStateMachineVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStateMachineVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "stateMachineVersions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListStateMachineVersions where
  hashWithSalt _salt ListStateMachineVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData ListStateMachineVersions where
  rnf ListStateMachineVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders ListStateMachineVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.ListStateMachineVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStateMachineVersions where
  toJSON ListStateMachineVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath ListStateMachineVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStateMachineVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStateMachineVersionsResponse' smart constructor.
data ListStateMachineVersionsResponse = ListStateMachineVersionsResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Versions for the state machine.
    stateMachineVersions :: [StateMachineVersionListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachineVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachineVersionsResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listStateMachineVersionsResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineVersions', 'listStateMachineVersionsResponse_stateMachineVersions' - Versions for the state machine.
newListStateMachineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStateMachineVersionsResponse
newListStateMachineVersionsResponse pHttpStatus_ =
  ListStateMachineVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stateMachineVersions = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachineVersionsResponse_nextToken :: Lens.Lens' ListStateMachineVersionsResponse (Prelude.Maybe Prelude.Text)
listStateMachineVersionsResponse_nextToken = Lens.lens (\ListStateMachineVersionsResponse' {nextToken} -> nextToken) (\s@ListStateMachineVersionsResponse' {} a -> s {nextToken = a} :: ListStateMachineVersionsResponse)

-- | The response's http status code.
listStateMachineVersionsResponse_httpStatus :: Lens.Lens' ListStateMachineVersionsResponse Prelude.Int
listStateMachineVersionsResponse_httpStatus = Lens.lens (\ListStateMachineVersionsResponse' {httpStatus} -> httpStatus) (\s@ListStateMachineVersionsResponse' {} a -> s {httpStatus = a} :: ListStateMachineVersionsResponse)

-- | Versions for the state machine.
listStateMachineVersionsResponse_stateMachineVersions :: Lens.Lens' ListStateMachineVersionsResponse [StateMachineVersionListItem]
listStateMachineVersionsResponse_stateMachineVersions = Lens.lens (\ListStateMachineVersionsResponse' {stateMachineVersions} -> stateMachineVersions) (\s@ListStateMachineVersionsResponse' {} a -> s {stateMachineVersions = a} :: ListStateMachineVersionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListStateMachineVersionsResponse
  where
  rnf ListStateMachineVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachineVersions
