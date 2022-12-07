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
-- Module      : Amazonka.FIS.ListExperiments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your experiments.
module Amazonka.FIS.ListExperiments
  ( -- * Creating a Request
    ListExperiments (..),
    newListExperiments,

    -- * Request Lenses
    listExperiments_nextToken,
    listExperiments_maxResults,

    -- * Destructuring the Response
    ListExperimentsResponse (..),
    newListExperimentsResponse,

    -- * Response Lenses
    listExperimentsResponse_experiments,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperiments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperiments_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listExperiments_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListExperiments ::
  ListExperiments
newListExperiments =
  ListExperiments'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listExperiments_nextToken :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Text)
listExperiments_nextToken = Lens.lens (\ListExperiments' {nextToken} -> nextToken) (\s@ListExperiments' {} a -> s {nextToken = a} :: ListExperiments)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listExperiments_maxResults :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Natural)
listExperiments_maxResults = Lens.lens (\ListExperiments' {maxResults} -> maxResults) (\s@ListExperiments' {} a -> s {maxResults = a} :: ListExperiments)

instance Core.AWSRequest ListExperiments where
  type
    AWSResponse ListExperiments =
      ListExperimentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Prelude.<$> (x Data..?> "experiments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperiments where
  hashWithSalt _salt ListExperiments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListExperiments where
  rnf ListExperiments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListExperiments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExperiments where
  toPath = Prelude.const "/experiments"

instance Data.ToQuery ListExperiments where
  toQuery ListExperiments' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { -- | The experiments.
    experiments :: Prelude.Maybe [ExperimentSummary],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperimentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiments', 'listExperimentsResponse_experiments' - The experiments.
--
-- 'nextToken', 'listExperimentsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listExperimentsResponse_httpStatus' - The response's http status code.
newListExperimentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExperimentsResponse
newListExperimentsResponse pHttpStatus_ =
  ListExperimentsResponse'
    { experiments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The experiments.
listExperimentsResponse_experiments :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe [ExperimentSummary])
listExperimentsResponse_experiments = Lens.lens (\ListExperimentsResponse' {experiments} -> experiments) (\s@ListExperimentsResponse' {} a -> s {experiments = a} :: ListExperimentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listExperimentsResponse_nextToken :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe Prelude.Text)
listExperimentsResponse_nextToken = Lens.lens (\ListExperimentsResponse' {nextToken} -> nextToken) (\s@ListExperimentsResponse' {} a -> s {nextToken = a} :: ListExperimentsResponse)

-- | The response's http status code.
listExperimentsResponse_httpStatus :: Lens.Lens' ListExperimentsResponse Prelude.Int
listExperimentsResponse_httpStatus = Lens.lens (\ListExperimentsResponse' {httpStatus} -> httpStatus) (\s@ListExperimentsResponse' {} a -> s {httpStatus = a} :: ListExperimentsResponse)

instance Prelude.NFData ListExperimentsResponse where
  rnf ListExperimentsResponse' {..} =
    Prelude.rnf experiments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
