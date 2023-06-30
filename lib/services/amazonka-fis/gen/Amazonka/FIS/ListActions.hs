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
-- Module      : Amazonka.FIS.ListActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available FIS actions.
module Amazonka.FIS.ListActions
  ( -- * Creating a Request
    ListActions (..),
    newListActions,

    -- * Request Lenses
    listActions_maxResults,
    listActions_nextToken,

    -- * Destructuring the Response
    ListActionsResponse (..),
    newListActionsResponse,

    -- * Response Lenses
    listActionsResponse_actions,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListActions' smart constructor.
data ListActions = ListActions'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listActions_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listActions_nextToken' - The token for the next page of results.
newListActions ::
  ListActions
newListActions =
  ListActions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listActions_maxResults :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Natural)
listActions_maxResults = Lens.lens (\ListActions' {maxResults} -> maxResults) (\s@ListActions' {} a -> s {maxResults = a} :: ListActions)

-- | The token for the next page of results.
listActions_nextToken :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_nextToken = Lens.lens (\ListActions' {nextToken} -> nextToken) (\s@ListActions' {} a -> s {nextToken = a} :: ListActions)

instance Core.AWSRequest ListActions where
  type AWSResponse ListActions = ListActionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionsResponse'
            Prelude.<$> (x Data..?> "actions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListActions where
  hashWithSalt _salt ListActions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListActions where
  rnf ListActions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListActions where
  toPath = Prelude.const "/actions"

instance Data.ToQuery ListActions where
  toQuery ListActions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListActionsResponse' smart constructor.
data ListActionsResponse = ListActionsResponse'
  { -- | The actions.
    actions :: Prelude.Maybe [ActionSummary],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'listActionsResponse_actions' - The actions.
--
-- 'nextToken', 'listActionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listActionsResponse_httpStatus' - The response's http status code.
newListActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActionsResponse
newListActionsResponse pHttpStatus_ =
  ListActionsResponse'
    { actions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The actions.
listActionsResponse_actions :: Lens.Lens' ListActionsResponse (Prelude.Maybe [ActionSummary])
listActionsResponse_actions = Lens.lens (\ListActionsResponse' {actions} -> actions) (\s@ListActionsResponse' {} a -> s {actions = a} :: ListActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listActionsResponse_nextToken :: Lens.Lens' ListActionsResponse (Prelude.Maybe Prelude.Text)
listActionsResponse_nextToken = Lens.lens (\ListActionsResponse' {nextToken} -> nextToken) (\s@ListActionsResponse' {} a -> s {nextToken = a} :: ListActionsResponse)

-- | The response's http status code.
listActionsResponse_httpStatus :: Lens.Lens' ListActionsResponse Prelude.Int
listActionsResponse_httpStatus = Lens.lens (\ListActionsResponse' {httpStatus} -> httpStatus) (\s@ListActionsResponse' {} a -> s {httpStatus = a} :: ListActionsResponse)

instance Prelude.NFData ListActionsResponse where
  rnf ListActionsResponse' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
