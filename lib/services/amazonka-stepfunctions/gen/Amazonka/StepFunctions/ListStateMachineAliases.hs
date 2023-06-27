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
-- Module      : Amazonka.StepFunctions.ListStateMachineAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html aliases>
-- for a specified state machine ARN. Results are sorted by time, with the
-- most recently created aliases listed first.
--
-- To list aliases that reference a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html version>,
-- you can specify the version ARN in the @stateMachineArn@ parameter.
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
-- -   CreateStateMachineAlias
--
-- -   DescribeStateMachineAlias
--
-- -   UpdateStateMachineAlias
--
-- -   DeleteStateMachineAlias
module Amazonka.StepFunctions.ListStateMachineAliases
  ( -- * Creating a Request
    ListStateMachineAliases (..),
    newListStateMachineAliases,

    -- * Request Lenses
    listStateMachineAliases_maxResults,
    listStateMachineAliases_nextToken,
    listStateMachineAliases_stateMachineArn,

    -- * Destructuring the Response
    ListStateMachineAliasesResponse (..),
    newListStateMachineAliasesResponse,

    -- * Response Lenses
    listStateMachineAliasesResponse_nextToken,
    listStateMachineAliasesResponse_httpStatus,
    listStateMachineAliasesResponse_stateMachineAliases,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newListStateMachineAliases' smart constructor.
data ListStateMachineAliases = ListStateMachineAliases'
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
    -- | The Amazon Resource Name (ARN) of the state machine for which you want
    -- to list aliases.
    --
    -- If you specify a state machine version ARN, this API returns a list of
    -- aliases for that version.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachineAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStateMachineAliases_maxResults' - The maximum number of results that are returned per call. You can use
-- @nextToken@ to obtain further pages of results. The default is 100 and
-- the maximum allowed page size is 1000. A value of 0 uses the default.
--
-- This is only an upper limit. The actual number of results returned per
-- call might be fewer than the specified maximum.
--
-- 'nextToken', 'listStateMachineAliases_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'stateMachineArn', 'listStateMachineAliases_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine for which you want
-- to list aliases.
--
-- If you specify a state machine version ARN, this API returns a list of
-- aliases for that version.
newListStateMachineAliases ::
  -- | 'stateMachineArn'
  Prelude.Text ->
  ListStateMachineAliases
newListStateMachineAliases pStateMachineArn_ =
  ListStateMachineAliases'
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
listStateMachineAliases_maxResults :: Lens.Lens' ListStateMachineAliases (Prelude.Maybe Prelude.Natural)
listStateMachineAliases_maxResults = Lens.lens (\ListStateMachineAliases' {maxResults} -> maxResults) (\s@ListStateMachineAliases' {} a -> s {maxResults = a} :: ListStateMachineAliases)

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachineAliases_nextToken :: Lens.Lens' ListStateMachineAliases (Prelude.Maybe Prelude.Text)
listStateMachineAliases_nextToken = Lens.lens (\ListStateMachineAliases' {nextToken} -> nextToken) (\s@ListStateMachineAliases' {} a -> s {nextToken = a} :: ListStateMachineAliases)

-- | The Amazon Resource Name (ARN) of the state machine for which you want
-- to list aliases.
--
-- If you specify a state machine version ARN, this API returns a list of
-- aliases for that version.
listStateMachineAliases_stateMachineArn :: Lens.Lens' ListStateMachineAliases Prelude.Text
listStateMachineAliases_stateMachineArn = Lens.lens (\ListStateMachineAliases' {stateMachineArn} -> stateMachineArn) (\s@ListStateMachineAliases' {} a -> s {stateMachineArn = a} :: ListStateMachineAliases)

instance Core.AWSRequest ListStateMachineAliases where
  type
    AWSResponse ListStateMachineAliases =
      ListStateMachineAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStateMachineAliasesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "stateMachineAliases"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListStateMachineAliases where
  hashWithSalt _salt ListStateMachineAliases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData ListStateMachineAliases where
  rnf ListStateMachineAliases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateMachineArn

instance Data.ToHeaders ListStateMachineAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.ListStateMachineAliases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStateMachineAliases where
  toJSON ListStateMachineAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath ListStateMachineAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStateMachineAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStateMachineAliasesResponse' smart constructor.
data ListStateMachineAliasesResponse = ListStateMachineAliasesResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an /HTTP 400 InvalidToken/
    -- error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Aliases for the state machine.
    stateMachineAliases :: [StateMachineAliasListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStateMachineAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStateMachineAliasesResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
--
-- 'httpStatus', 'listStateMachineAliasesResponse_httpStatus' - The response's http status code.
--
-- 'stateMachineAliases', 'listStateMachineAliasesResponse_stateMachineAliases' - Aliases for the state machine.
newListStateMachineAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStateMachineAliasesResponse
newListStateMachineAliasesResponse pHttpStatus_ =
  ListStateMachineAliasesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      stateMachineAliases = Prelude.mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an /HTTP 400 InvalidToken/
-- error.
listStateMachineAliasesResponse_nextToken :: Lens.Lens' ListStateMachineAliasesResponse (Prelude.Maybe Prelude.Text)
listStateMachineAliasesResponse_nextToken = Lens.lens (\ListStateMachineAliasesResponse' {nextToken} -> nextToken) (\s@ListStateMachineAliasesResponse' {} a -> s {nextToken = a} :: ListStateMachineAliasesResponse)

-- | The response's http status code.
listStateMachineAliasesResponse_httpStatus :: Lens.Lens' ListStateMachineAliasesResponse Prelude.Int
listStateMachineAliasesResponse_httpStatus = Lens.lens (\ListStateMachineAliasesResponse' {httpStatus} -> httpStatus) (\s@ListStateMachineAliasesResponse' {} a -> s {httpStatus = a} :: ListStateMachineAliasesResponse)

-- | Aliases for the state machine.
listStateMachineAliasesResponse_stateMachineAliases :: Lens.Lens' ListStateMachineAliasesResponse [StateMachineAliasListItem]
listStateMachineAliasesResponse_stateMachineAliases = Lens.lens (\ListStateMachineAliasesResponse' {stateMachineAliases} -> stateMachineAliases) (\s@ListStateMachineAliasesResponse' {} a -> s {stateMachineAliases = a} :: ListStateMachineAliasesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListStateMachineAliasesResponse
  where
  rnf ListStateMachineAliasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stateMachineAliases
