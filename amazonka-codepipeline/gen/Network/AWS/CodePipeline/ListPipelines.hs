{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.ListPipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all of the pipelines associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelines
  ( -- * Creating a Request
    ListPipelines (..),
    newListPipelines,

    -- * Request Lenses
    listPipelines_nextToken,
    listPipelines_maxResults,

    -- * Destructuring the Response
    ListPipelinesResponse (..),
    newListPipelinesResponse,

    -- * Response Lenses
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListPipelines@ action.
--
-- /See:/ 'newListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { -- | An identifier that was returned from the previous list pipelines call.
    -- It can be used to return the next set of pipelines in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of pipelines to return in a single call. To retrieve
    -- the remaining pipelines, make another call with the returned nextToken
    -- value. The minimum value you can specify is 1. The maximum accepted
    -- value is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelines_nextToken' - An identifier that was returned from the previous list pipelines call.
-- It can be used to return the next set of pipelines in the list.
--
-- 'maxResults', 'listPipelines_maxResults' - The maximum number of pipelines to return in a single call. To retrieve
-- the remaining pipelines, make another call with the returned nextToken
-- value. The minimum value you can specify is 1. The maximum accepted
-- value is 1000.
newListPipelines ::
  ListPipelines
newListPipelines =
  ListPipelines'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An identifier that was returned from the previous list pipelines call.
-- It can be used to return the next set of pipelines in the list.
listPipelines_nextToken :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Text)
listPipelines_nextToken = Lens.lens (\ListPipelines' {nextToken} -> nextToken) (\s@ListPipelines' {} a -> s {nextToken = a} :: ListPipelines)

-- | The maximum number of pipelines to return in a single call. To retrieve
-- the remaining pipelines, make another call with the returned nextToken
-- value. The minimum value you can specify is 1. The maximum accepted
-- value is 1000.
listPipelines_maxResults :: Lens.Lens' ListPipelines (Prelude.Maybe Prelude.Natural)
listPipelines_maxResults = Lens.lens (\ListPipelines' {maxResults} -> maxResults) (\s@ListPipelines' {} a -> s {maxResults = a} :: ListPipelines)

instance Pager.AWSPager ListPipelines where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPipelinesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listPipelinesResponse_pipelines Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPipelines_nextToken
          Lens..~ rs
          Lens.^? listPipelinesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListPipelines where
  type Rs ListPipelines = ListPipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelinesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "pipelines"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelines

instance Prelude.NFData ListPipelines

instance Prelude.ToHeaders ListPipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.ListPipelines" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListPipelines where
  toJSON ListPipelines' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListPipelines where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPipelines where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListPipelines@ action.
--
-- /See:/ 'newListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { -- | If the amount of returned information is significantly large, an
    -- identifier is also returned. It can be used in a subsequent list
    -- pipelines call to return the next set of pipelines in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of pipelines.
    pipelines :: Prelude.Maybe [PipelineSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelinesResponse_nextToken' - If the amount of returned information is significantly large, an
-- identifier is also returned. It can be used in a subsequent list
-- pipelines call to return the next set of pipelines in the list.
--
-- 'pipelines', 'listPipelinesResponse_pipelines' - The list of pipelines.
--
-- 'httpStatus', 'listPipelinesResponse_httpStatus' - The response's http status code.
newListPipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelinesResponse
newListPipelinesResponse pHttpStatus_ =
  ListPipelinesResponse'
    { nextToken = Prelude.Nothing,
      pipelines = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the amount of returned information is significantly large, an
-- identifier is also returned. It can be used in a subsequent list
-- pipelines call to return the next set of pipelines in the list.
listPipelinesResponse_nextToken :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe Prelude.Text)
listPipelinesResponse_nextToken = Lens.lens (\ListPipelinesResponse' {nextToken} -> nextToken) (\s@ListPipelinesResponse' {} a -> s {nextToken = a} :: ListPipelinesResponse)

-- | The list of pipelines.
listPipelinesResponse_pipelines :: Lens.Lens' ListPipelinesResponse (Prelude.Maybe [PipelineSummary])
listPipelinesResponse_pipelines = Lens.lens (\ListPipelinesResponse' {pipelines} -> pipelines) (\s@ListPipelinesResponse' {} a -> s {pipelines = a} :: ListPipelinesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listPipelinesResponse_httpStatus :: Lens.Lens' ListPipelinesResponse Prelude.Int
listPipelinesResponse_httpStatus = Lens.lens (\ListPipelinesResponse' {httpStatus} -> httpStatus) (\s@ListPipelinesResponse' {} a -> s {httpStatus = a} :: ListPipelinesResponse)

instance Prelude.NFData ListPipelinesResponse
