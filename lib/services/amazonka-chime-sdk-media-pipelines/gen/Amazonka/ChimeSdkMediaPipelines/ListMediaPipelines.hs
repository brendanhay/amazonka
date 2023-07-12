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
-- Module      : Amazonka.ChimeSdkMediaPipelines.ListMediaPipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of media pipelines.
module Amazonka.ChimeSdkMediaPipelines.ListMediaPipelines
  ( -- * Creating a Request
    ListMediaPipelines (..),
    newListMediaPipelines,

    -- * Request Lenses
    listMediaPipelines_maxResults,
    listMediaPipelines_nextToken,

    -- * Destructuring the Response
    ListMediaPipelinesResponse (..),
    newListMediaPipelinesResponse,

    -- * Response Lenses
    listMediaPipelinesResponse_mediaPipelines,
    listMediaPipelinesResponse_nextToken,
    listMediaPipelinesResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMediaPipelines' smart constructor.
data ListMediaPipelines = ListMediaPipelines'
  { -- | The maximum number of results to return in a single call. Valid Range: 1
    -- - 99.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaPipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMediaPipelines_maxResults' - The maximum number of results to return in a single call. Valid Range: 1
-- - 99.
--
-- 'nextToken', 'listMediaPipelines_nextToken' - The token used to retrieve the next page of results.
newListMediaPipelines ::
  ListMediaPipelines
newListMediaPipelines =
  ListMediaPipelines'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call. Valid Range: 1
-- - 99.
listMediaPipelines_maxResults :: Lens.Lens' ListMediaPipelines (Prelude.Maybe Prelude.Natural)
listMediaPipelines_maxResults = Lens.lens (\ListMediaPipelines' {maxResults} -> maxResults) (\s@ListMediaPipelines' {} a -> s {maxResults = a} :: ListMediaPipelines)

-- | The token used to retrieve the next page of results.
listMediaPipelines_nextToken :: Lens.Lens' ListMediaPipelines (Prelude.Maybe Prelude.Text)
listMediaPipelines_nextToken = Lens.lens (\ListMediaPipelines' {nextToken} -> nextToken) (\s@ListMediaPipelines' {} a -> s {nextToken = a} :: ListMediaPipelines)

instance Core.AWSRequest ListMediaPipelines where
  type
    AWSResponse ListMediaPipelines =
      ListMediaPipelinesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMediaPipelinesResponse'
            Prelude.<$> (x Data..?> "MediaPipelines" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMediaPipelines where
  hashWithSalt _salt ListMediaPipelines' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMediaPipelines where
  rnf ListMediaPipelines' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListMediaPipelines where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMediaPipelines where
  toPath = Prelude.const "/sdk-media-pipelines"

instance Data.ToQuery ListMediaPipelines where
  toQuery ListMediaPipelines' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListMediaPipelinesResponse' smart constructor.
data ListMediaPipelinesResponse = ListMediaPipelinesResponse'
  { -- | The media pipeline objects in the list.
    mediaPipelines :: Prelude.Maybe [MediaPipelineSummary],
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaPipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelines', 'listMediaPipelinesResponse_mediaPipelines' - The media pipeline objects in the list.
--
-- 'nextToken', 'listMediaPipelinesResponse_nextToken' - The token used to retrieve the next page of results.
--
-- 'httpStatus', 'listMediaPipelinesResponse_httpStatus' - The response's http status code.
newListMediaPipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMediaPipelinesResponse
newListMediaPipelinesResponse pHttpStatus_ =
  ListMediaPipelinesResponse'
    { mediaPipelines =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The media pipeline objects in the list.
listMediaPipelinesResponse_mediaPipelines :: Lens.Lens' ListMediaPipelinesResponse (Prelude.Maybe [MediaPipelineSummary])
listMediaPipelinesResponse_mediaPipelines = Lens.lens (\ListMediaPipelinesResponse' {mediaPipelines} -> mediaPipelines) (\s@ListMediaPipelinesResponse' {} a -> s {mediaPipelines = a} :: ListMediaPipelinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of results.
listMediaPipelinesResponse_nextToken :: Lens.Lens' ListMediaPipelinesResponse (Prelude.Maybe Prelude.Text)
listMediaPipelinesResponse_nextToken = Lens.lens (\ListMediaPipelinesResponse' {nextToken} -> nextToken) (\s@ListMediaPipelinesResponse' {} a -> s {nextToken = a} :: ListMediaPipelinesResponse)

-- | The response's http status code.
listMediaPipelinesResponse_httpStatus :: Lens.Lens' ListMediaPipelinesResponse Prelude.Int
listMediaPipelinesResponse_httpStatus = Lens.lens (\ListMediaPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListMediaPipelinesResponse' {} a -> s {httpStatus = a} :: ListMediaPipelinesResponse)

instance Prelude.NFData ListMediaPipelinesResponse where
  rnf ListMediaPipelinesResponse' {..} =
    Prelude.rnf mediaPipelines
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
