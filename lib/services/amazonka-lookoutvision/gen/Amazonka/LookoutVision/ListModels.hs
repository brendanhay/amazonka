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
-- Module      : Amazonka.LookoutVision.ListModels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a model in an Amazon Lookout for Vision project.
--
-- The @ListModels@ operation is eventually consistent. Recent calls to
-- @CreateModel@ might take a while to appear in the response from
-- @ListProjects@.
--
-- This operation requires permissions to perform the
-- @lookoutvision:ListModels@ operation.
--
-- This operation returns paginated results.
module Amazonka.LookoutVision.ListModels
  ( -- * Creating a Request
    ListModels (..),
    newListModels,

    -- * Request Lenses
    listModels_maxResults,
    listModels_nextToken,
    listModels_projectName,

    -- * Destructuring the Response
    ListModelsResponse (..),
    newListModelsResponse,

    -- * Response Lenses
    listModelsResponse_models,
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListModels' smart constructor.
data ListModels = ListModels'
  { -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Lookout for Vision returns a pagination token in the
    -- response. You can use this pagination token to retrieve the next set of
    -- models.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that contains the model versions that you want
    -- to list.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listModels_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'nextToken', 'listModels_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Lookout for Vision returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- models.
--
-- 'projectName', 'listModels_projectName' - The name of the project that contains the model versions that you want
-- to list.
newListModels ::
  -- | 'projectName'
  Prelude.Text ->
  ListModels
newListModels pProjectName_ =
  ListModels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
listModels_maxResults :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Natural)
listModels_maxResults = Lens.lens (\ListModels' {maxResults} -> maxResults) (\s@ListModels' {} a -> s {maxResults = a} :: ListModels)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Lookout for Vision returns a pagination token in the
-- response. You can use this pagination token to retrieve the next set of
-- models.
listModels_nextToken :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nextToken = Lens.lens (\ListModels' {nextToken} -> nextToken) (\s@ListModels' {} a -> s {nextToken = a} :: ListModels)

-- | The name of the project that contains the model versions that you want
-- to list.
listModels_projectName :: Lens.Lens' ListModels Prelude.Text
listModels_projectName = Lens.lens (\ListModels' {projectName} -> projectName) (\s@ListModels' {} a -> s {projectName = a} :: ListModels)

instance Core.AWSPager ListModels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listModelsResponse_models Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModels_nextToken
          Lens..~ rs
          Lens.^? listModelsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListModels where
  type AWSResponse ListModels = ListModelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Prelude.<$> (x Data..?> "Models" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListModels where
  hashWithSalt _salt ListModels' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData ListModels where
  rnf ListModels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders ListModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListModels where
  toPath ListModels' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/models"
      ]

instance Data.ToQuery ListModels where
  toQuery ListModels' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | A list of model versions in the specified project.
    models :: Prelude.Maybe [ModelMetadata],
    -- | If the response is truncated, Amazon Lookout for Vision returns this
    -- token that you can use in the subsequent request to retrieve the next
    -- set of models.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'models', 'listModelsResponse_models' - A list of model versions in the specified project.
--
-- 'nextToken', 'listModelsResponse_nextToken' - If the response is truncated, Amazon Lookout for Vision returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of models.
--
-- 'httpStatus', 'listModelsResponse_httpStatus' - The response's http status code.
newListModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelsResponse
newListModelsResponse pHttpStatus_ =
  ListModelsResponse'
    { models = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of model versions in the specified project.
listModelsResponse_models :: Lens.Lens' ListModelsResponse (Prelude.Maybe [ModelMetadata])
listModelsResponse_models = Lens.lens (\ListModelsResponse' {models} -> models) (\s@ListModelsResponse' {} a -> s {models = a} :: ListModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Lookout for Vision returns this
-- token that you can use in the subsequent request to retrieve the next
-- set of models.
listModelsResponse_nextToken :: Lens.Lens' ListModelsResponse (Prelude.Maybe Prelude.Text)
listModelsResponse_nextToken = Lens.lens (\ListModelsResponse' {nextToken} -> nextToken) (\s@ListModelsResponse' {} a -> s {nextToken = a} :: ListModelsResponse)

-- | The response's http status code.
listModelsResponse_httpStatus :: Lens.Lens' ListModelsResponse Prelude.Int
listModelsResponse_httpStatus = Lens.lens (\ListModelsResponse' {httpStatus} -> httpStatus) (\s@ListModelsResponse' {} a -> s {httpStatus = a} :: ListModelsResponse)

instance Prelude.NFData ListModelsResponse where
  rnf ListModelsResponse' {..} =
    Prelude.rnf models
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
