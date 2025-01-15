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
-- Module      : Amazonka.Chime.ListMediaCapturePipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of media capture pipelines.
module Amazonka.Chime.ListMediaCapturePipelines
  ( -- * Creating a Request
    ListMediaCapturePipelines (..),
    newListMediaCapturePipelines,

    -- * Request Lenses
    listMediaCapturePipelines_maxResults,
    listMediaCapturePipelines_nextToken,

    -- * Destructuring the Response
    ListMediaCapturePipelinesResponse (..),
    newListMediaCapturePipelinesResponse,

    -- * Response Lenses
    listMediaCapturePipelinesResponse_mediaCapturePipelines,
    listMediaCapturePipelinesResponse_nextToken,
    listMediaCapturePipelinesResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMediaCapturePipelines' smart constructor.
data ListMediaCapturePipelines = ListMediaCapturePipelines'
  { -- | The maximum number of results to return in a single call. Valid Range: 1
    -- - 99.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaCapturePipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMediaCapturePipelines_maxResults' - The maximum number of results to return in a single call. Valid Range: 1
-- - 99.
--
-- 'nextToken', 'listMediaCapturePipelines_nextToken' - The token used to retrieve the next page of results.
newListMediaCapturePipelines ::
  ListMediaCapturePipelines
newListMediaCapturePipelines =
  ListMediaCapturePipelines'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call. Valid Range: 1
-- - 99.
listMediaCapturePipelines_maxResults :: Lens.Lens' ListMediaCapturePipelines (Prelude.Maybe Prelude.Natural)
listMediaCapturePipelines_maxResults = Lens.lens (\ListMediaCapturePipelines' {maxResults} -> maxResults) (\s@ListMediaCapturePipelines' {} a -> s {maxResults = a} :: ListMediaCapturePipelines)

-- | The token used to retrieve the next page of results.
listMediaCapturePipelines_nextToken :: Lens.Lens' ListMediaCapturePipelines (Prelude.Maybe Prelude.Text)
listMediaCapturePipelines_nextToken = Lens.lens (\ListMediaCapturePipelines' {nextToken} -> nextToken) (\s@ListMediaCapturePipelines' {} a -> s {nextToken = a} :: ListMediaCapturePipelines)

instance Core.AWSRequest ListMediaCapturePipelines where
  type
    AWSResponse ListMediaCapturePipelines =
      ListMediaCapturePipelinesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMediaCapturePipelinesResponse'
            Prelude.<$> ( x
                            Data..?> "MediaCapturePipelines"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMediaCapturePipelines where
  hashWithSalt _salt ListMediaCapturePipelines' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMediaCapturePipelines where
  rnf ListMediaCapturePipelines' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListMediaCapturePipelines where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMediaCapturePipelines where
  toPath = Prelude.const "/media-capture-pipelines"

instance Data.ToQuery ListMediaCapturePipelines where
  toQuery ListMediaCapturePipelines' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListMediaCapturePipelinesResponse' smart constructor.
data ListMediaCapturePipelinesResponse = ListMediaCapturePipelinesResponse'
  { -- | The media capture pipeline objects in the list.
    mediaCapturePipelines :: Prelude.Maybe [MediaCapturePipeline],
    -- | The token used to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaCapturePipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaCapturePipelines', 'listMediaCapturePipelinesResponse_mediaCapturePipelines' - The media capture pipeline objects in the list.
--
-- 'nextToken', 'listMediaCapturePipelinesResponse_nextToken' - The token used to retrieve the next page of results.
--
-- 'httpStatus', 'listMediaCapturePipelinesResponse_httpStatus' - The response's http status code.
newListMediaCapturePipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMediaCapturePipelinesResponse
newListMediaCapturePipelinesResponse pHttpStatus_ =
  ListMediaCapturePipelinesResponse'
    { mediaCapturePipelines =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The media capture pipeline objects in the list.
listMediaCapturePipelinesResponse_mediaCapturePipelines :: Lens.Lens' ListMediaCapturePipelinesResponse (Prelude.Maybe [MediaCapturePipeline])
listMediaCapturePipelinesResponse_mediaCapturePipelines = Lens.lens (\ListMediaCapturePipelinesResponse' {mediaCapturePipelines} -> mediaCapturePipelines) (\s@ListMediaCapturePipelinesResponse' {} a -> s {mediaCapturePipelines = a} :: ListMediaCapturePipelinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of results.
listMediaCapturePipelinesResponse_nextToken :: Lens.Lens' ListMediaCapturePipelinesResponse (Prelude.Maybe Prelude.Text)
listMediaCapturePipelinesResponse_nextToken = Lens.lens (\ListMediaCapturePipelinesResponse' {nextToken} -> nextToken) (\s@ListMediaCapturePipelinesResponse' {} a -> s {nextToken = a} :: ListMediaCapturePipelinesResponse)

-- | The response's http status code.
listMediaCapturePipelinesResponse_httpStatus :: Lens.Lens' ListMediaCapturePipelinesResponse Prelude.Int
listMediaCapturePipelinesResponse_httpStatus = Lens.lens (\ListMediaCapturePipelinesResponse' {httpStatus} -> httpStatus) (\s@ListMediaCapturePipelinesResponse' {} a -> s {httpStatus = a} :: ListMediaCapturePipelinesResponse)

instance
  Prelude.NFData
    ListMediaCapturePipelinesResponse
  where
  rnf ListMediaCapturePipelinesResponse' {..} =
    Prelude.rnf mediaCapturePipelines `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
