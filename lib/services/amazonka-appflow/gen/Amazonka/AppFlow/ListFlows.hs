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
-- Module      : Amazonka.AppFlow.ListFlows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the flows associated with your account.
module Amazonka.AppFlow.ListFlows
  ( -- * Creating a Request
    ListFlows (..),
    newListFlows,

    -- * Request Lenses
    listFlows_nextToken,
    listFlows_maxResults,

    -- * Destructuring the Response
    ListFlowsResponse (..),
    newListFlowsResponse,

    -- * Response Lenses
    listFlowsResponse_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFlows' smart constructor.
data ListFlows = ListFlows'
  { -- | The pagination token for next page of data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of items that should be returned in the
    -- result set.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlows_nextToken' - The pagination token for next page of data.
--
-- 'maxResults', 'listFlows_maxResults' - Specifies the maximum number of items that should be returned in the
-- result set.
newListFlows ::
  ListFlows
newListFlows =
  ListFlows'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token for next page of data.
listFlows_nextToken :: Lens.Lens' ListFlows (Prelude.Maybe Prelude.Text)
listFlows_nextToken = Lens.lens (\ListFlows' {nextToken} -> nextToken) (\s@ListFlows' {} a -> s {nextToken = a} :: ListFlows)

-- | Specifies the maximum number of items that should be returned in the
-- result set.
listFlows_maxResults :: Lens.Lens' ListFlows (Prelude.Maybe Prelude.Natural)
listFlows_maxResults = Lens.lens (\ListFlows' {maxResults} -> maxResults) (\s@ListFlows' {} a -> s {maxResults = a} :: ListFlows)

instance Core.AWSRequest ListFlows where
  type AWSResponse ListFlows = ListFlowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlowsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "flows" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFlows where
  hashWithSalt _salt ListFlows' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFlows where
  rnf ListFlows' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListFlows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFlows where
  toJSON ListFlows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListFlows where
  toPath = Prelude.const "/list-flows"

instance Data.ToQuery ListFlows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFlowsResponse' smart constructor.
data ListFlowsResponse = ListFlowsResponse'
  { -- | The pagination token for next page of data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of flows associated with your account.
    flows :: Prelude.Maybe [FlowDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowsResponse_nextToken' - The pagination token for next page of data.
--
-- 'flows', 'listFlowsResponse_flows' - The list of flows associated with your account.
--
-- 'httpStatus', 'listFlowsResponse_httpStatus' - The response's http status code.
newListFlowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlowsResponse
newListFlowsResponse pHttpStatus_ =
  ListFlowsResponse'
    { nextToken = Prelude.Nothing,
      flows = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token for next page of data.
listFlowsResponse_nextToken :: Lens.Lens' ListFlowsResponse (Prelude.Maybe Prelude.Text)
listFlowsResponse_nextToken = Lens.lens (\ListFlowsResponse' {nextToken} -> nextToken) (\s@ListFlowsResponse' {} a -> s {nextToken = a} :: ListFlowsResponse)

-- | The list of flows associated with your account.
listFlowsResponse_flows :: Lens.Lens' ListFlowsResponse (Prelude.Maybe [FlowDefinition])
listFlowsResponse_flows = Lens.lens (\ListFlowsResponse' {flows} -> flows) (\s@ListFlowsResponse' {} a -> s {flows = a} :: ListFlowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFlowsResponse_httpStatus :: Lens.Lens' ListFlowsResponse Prelude.Int
listFlowsResponse_httpStatus = Lens.lens (\ListFlowsResponse' {httpStatus} -> httpStatus) (\s@ListFlowsResponse' {} a -> s {httpStatus = a} :: ListFlowsResponse)

instance Prelude.NFData ListFlowsResponse where
  rnf ListFlowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf flows
      `Prelude.seq` Prelude.rnf httpStatus
