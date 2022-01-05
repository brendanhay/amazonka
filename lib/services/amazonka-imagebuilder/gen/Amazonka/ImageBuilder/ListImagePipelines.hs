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
-- Module      : Amazonka.ImageBuilder.ListImagePipelines
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of image pipelines.
module Amazonka.ImageBuilder.ListImagePipelines
  ( -- * Creating a Request
    ListImagePipelines (..),
    newListImagePipelines,

    -- * Request Lenses
    listImagePipelines_filters,
    listImagePipelines_nextToken,
    listImagePipelines_maxResults,

    -- * Destructuring the Response
    ListImagePipelinesResponse (..),
    newListImagePipelinesResponse,

    -- * Response Lenses
    listImagePipelinesResponse_requestId,
    listImagePipelinesResponse_nextToken,
    listImagePipelinesResponse_imagePipelineList,
    listImagePipelinesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImagePipelines' smart constructor.
data ListImagePipelines = ListImagePipelines'
  { -- | Use the following filters to streamline results:
    --
    -- -   @description@
    --
    -- -   @distributionConfigurationArn@
    --
    -- -   @imageRecipeArn@
    --
    -- -   @infrastructureConfigurationArn@
    --
    -- -   @name@
    --
    -- -   @status@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePipelines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listImagePipelines_filters' - Use the following filters to streamline results:
--
-- -   @description@
--
-- -   @distributionConfigurationArn@
--
-- -   @imageRecipeArn@
--
-- -   @infrastructureConfigurationArn@
--
-- -   @name@
--
-- -   @status@
--
-- 'nextToken', 'listImagePipelines_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'maxResults', 'listImagePipelines_maxResults' - The maximum items to return in a request.
newListImagePipelines ::
  ListImagePipelines
newListImagePipelines =
  ListImagePipelines'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Use the following filters to streamline results:
--
-- -   @description@
--
-- -   @distributionConfigurationArn@
--
-- -   @imageRecipeArn@
--
-- -   @infrastructureConfigurationArn@
--
-- -   @name@
--
-- -   @status@
listImagePipelines_filters :: Lens.Lens' ListImagePipelines (Prelude.Maybe (Prelude.NonEmpty Filter))
listImagePipelines_filters = Lens.lens (\ListImagePipelines' {filters} -> filters) (\s@ListImagePipelines' {} a -> s {filters = a} :: ListImagePipelines) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImagePipelines_nextToken :: Lens.Lens' ListImagePipelines (Prelude.Maybe Prelude.Text)
listImagePipelines_nextToken = Lens.lens (\ListImagePipelines' {nextToken} -> nextToken) (\s@ListImagePipelines' {} a -> s {nextToken = a} :: ListImagePipelines)

-- | The maximum items to return in a request.
listImagePipelines_maxResults :: Lens.Lens' ListImagePipelines (Prelude.Maybe Prelude.Natural)
listImagePipelines_maxResults = Lens.lens (\ListImagePipelines' {maxResults} -> maxResults) (\s@ListImagePipelines' {} a -> s {maxResults = a} :: ListImagePipelines)

instance Core.AWSRequest ListImagePipelines where
  type
    AWSResponse ListImagePipelines =
      ListImagePipelinesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagePipelinesResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "imagePipelineList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImagePipelines where
  hashWithSalt _salt ListImagePipelines' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListImagePipelines where
  rnf ListImagePipelines' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListImagePipelines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImagePipelines where
  toJSON ListImagePipelines' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListImagePipelines where
  toPath = Prelude.const "/ListImagePipelines"

instance Core.ToQuery ListImagePipelines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagePipelinesResponse' smart constructor.
data ListImagePipelinesResponse = ListImagePipelinesResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of image pipelines.
    imagePipelineList :: Prelude.Maybe [ImagePipeline],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagePipelinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'listImagePipelinesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'nextToken', 'listImagePipelinesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'imagePipelineList', 'listImagePipelinesResponse_imagePipelineList' - The list of image pipelines.
--
-- 'httpStatus', 'listImagePipelinesResponse_httpStatus' - The response's http status code.
newListImagePipelinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagePipelinesResponse
newListImagePipelinesResponse pHttpStatus_ =
  ListImagePipelinesResponse'
    { requestId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imagePipelineList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
listImagePipelinesResponse_requestId :: Lens.Lens' ListImagePipelinesResponse (Prelude.Maybe Prelude.Text)
listImagePipelinesResponse_requestId = Lens.lens (\ListImagePipelinesResponse' {requestId} -> requestId) (\s@ListImagePipelinesResponse' {} a -> s {requestId = a} :: ListImagePipelinesResponse)

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImagePipelinesResponse_nextToken :: Lens.Lens' ListImagePipelinesResponse (Prelude.Maybe Prelude.Text)
listImagePipelinesResponse_nextToken = Lens.lens (\ListImagePipelinesResponse' {nextToken} -> nextToken) (\s@ListImagePipelinesResponse' {} a -> s {nextToken = a} :: ListImagePipelinesResponse)

-- | The list of image pipelines.
listImagePipelinesResponse_imagePipelineList :: Lens.Lens' ListImagePipelinesResponse (Prelude.Maybe [ImagePipeline])
listImagePipelinesResponse_imagePipelineList = Lens.lens (\ListImagePipelinesResponse' {imagePipelineList} -> imagePipelineList) (\s@ListImagePipelinesResponse' {} a -> s {imagePipelineList = a} :: ListImagePipelinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImagePipelinesResponse_httpStatus :: Lens.Lens' ListImagePipelinesResponse Prelude.Int
listImagePipelinesResponse_httpStatus = Lens.lens (\ListImagePipelinesResponse' {httpStatus} -> httpStatus) (\s@ListImagePipelinesResponse' {} a -> s {httpStatus = a} :: ListImagePipelinesResponse)

instance Prelude.NFData ListImagePipelinesResponse where
  rnf ListImagePipelinesResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imagePipelineList
      `Prelude.seq` Prelude.rnf httpStatus
