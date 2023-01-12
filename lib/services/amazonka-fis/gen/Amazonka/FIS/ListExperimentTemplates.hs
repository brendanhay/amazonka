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
-- Module      : Amazonka.FIS.ListExperimentTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your experiment templates.
module Amazonka.FIS.ListExperimentTemplates
  ( -- * Creating a Request
    ListExperimentTemplates (..),
    newListExperimentTemplates,

    -- * Request Lenses
    listExperimentTemplates_maxResults,
    listExperimentTemplates_nextToken,

    -- * Destructuring the Response
    ListExperimentTemplatesResponse (..),
    newListExperimentTemplatesResponse,

    -- * Response Lenses
    listExperimentTemplatesResponse_experimentTemplates,
    listExperimentTemplatesResponse_nextToken,
    listExperimentTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExperimentTemplates' smart constructor.
data ListExperimentTemplates = ListExperimentTemplates'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperimentTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExperimentTemplates_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listExperimentTemplates_nextToken' - The token for the next page of results.
newListExperimentTemplates ::
  ListExperimentTemplates
newListExperimentTemplates =
  ListExperimentTemplates'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listExperimentTemplates_maxResults :: Lens.Lens' ListExperimentTemplates (Prelude.Maybe Prelude.Natural)
listExperimentTemplates_maxResults = Lens.lens (\ListExperimentTemplates' {maxResults} -> maxResults) (\s@ListExperimentTemplates' {} a -> s {maxResults = a} :: ListExperimentTemplates)

-- | The token for the next page of results.
listExperimentTemplates_nextToken :: Lens.Lens' ListExperimentTemplates (Prelude.Maybe Prelude.Text)
listExperimentTemplates_nextToken = Lens.lens (\ListExperimentTemplates' {nextToken} -> nextToken) (\s@ListExperimentTemplates' {} a -> s {nextToken = a} :: ListExperimentTemplates)

instance Core.AWSRequest ListExperimentTemplates where
  type
    AWSResponse ListExperimentTemplates =
      ListExperimentTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentTemplatesResponse'
            Prelude.<$> ( x Data..?> "experimentTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperimentTemplates where
  hashWithSalt _salt ListExperimentTemplates' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListExperimentTemplates where
  rnf ListExperimentTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListExperimentTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExperimentTemplates where
  toPath = Prelude.const "/experimentTemplates"

instance Data.ToQuery ListExperimentTemplates where
  toQuery ListExperimentTemplates' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListExperimentTemplatesResponse' smart constructor.
data ListExperimentTemplatesResponse = ListExperimentTemplatesResponse'
  { -- | The experiment templates.
    experimentTemplates :: Prelude.Maybe [ExperimentTemplateSummary],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperimentTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentTemplates', 'listExperimentTemplatesResponse_experimentTemplates' - The experiment templates.
--
-- 'nextToken', 'listExperimentTemplatesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listExperimentTemplatesResponse_httpStatus' - The response's http status code.
newListExperimentTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExperimentTemplatesResponse
newListExperimentTemplatesResponse pHttpStatus_ =
  ListExperimentTemplatesResponse'
    { experimentTemplates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The experiment templates.
listExperimentTemplatesResponse_experimentTemplates :: Lens.Lens' ListExperimentTemplatesResponse (Prelude.Maybe [ExperimentTemplateSummary])
listExperimentTemplatesResponse_experimentTemplates = Lens.lens (\ListExperimentTemplatesResponse' {experimentTemplates} -> experimentTemplates) (\s@ListExperimentTemplatesResponse' {} a -> s {experimentTemplates = a} :: ListExperimentTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listExperimentTemplatesResponse_nextToken :: Lens.Lens' ListExperimentTemplatesResponse (Prelude.Maybe Prelude.Text)
listExperimentTemplatesResponse_nextToken = Lens.lens (\ListExperimentTemplatesResponse' {nextToken} -> nextToken) (\s@ListExperimentTemplatesResponse' {} a -> s {nextToken = a} :: ListExperimentTemplatesResponse)

-- | The response's http status code.
listExperimentTemplatesResponse_httpStatus :: Lens.Lens' ListExperimentTemplatesResponse Prelude.Int
listExperimentTemplatesResponse_httpStatus = Lens.lens (\ListExperimentTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListExperimentTemplatesResponse' {} a -> s {httpStatus = a} :: ListExperimentTemplatesResponse)

instance
  Prelude.NFData
    ListExperimentTemplatesResponse
  where
  rnf ListExperimentTemplatesResponse' {..} =
    Prelude.rnf experimentTemplates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
