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
-- Module      : Network.AWS.Transcribe.ListLanguageModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information about the custom language models you\'ve
-- created. You can use the information in this list to find a specific
-- custom language model. You can then use the operation to get more
-- information about it.
module Network.AWS.Transcribe.ListLanguageModels
  ( -- * Creating a Request
    ListLanguageModels (..),
    newListLanguageModels,

    -- * Request Lenses
    listLanguageModels_nextToken,
    listLanguageModels_nameContains,
    listLanguageModels_maxResults,
    listLanguageModels_statusEquals,

    -- * Destructuring the Response
    ListLanguageModelsResponse (..),
    newListLanguageModelsResponse,

    -- * Response Lenses
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListLanguageModels' smart constructor.
data ListLanguageModels = ListLanguageModels'
  { -- | When included, fetches the next set of jobs if the result of the
    -- previous request was truncated.
    nextToken :: Core.Maybe Core.Text,
    -- | When specified, the custom language model names returned contain the
    -- substring you\'ve specified.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of language models to return in the response. If
    -- there are fewer results in the list, the response contains only the
    -- actual results.
    maxResults :: Core.Maybe Core.Natural,
    -- | When specified, returns only custom language models with the specified
    -- status. Language models are ordered by creation date, with the newest
    -- models first. If you don\'t specify a status, Amazon Transcribe returns
    -- all custom language models ordered by date.
    statusEquals :: Core.Maybe ModelStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLanguageModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLanguageModels_nextToken' - When included, fetches the next set of jobs if the result of the
-- previous request was truncated.
--
-- 'nameContains', 'listLanguageModels_nameContains' - When specified, the custom language model names returned contain the
-- substring you\'ve specified.
--
-- 'maxResults', 'listLanguageModels_maxResults' - The maximum number of language models to return in the response. If
-- there are fewer results in the list, the response contains only the
-- actual results.
--
-- 'statusEquals', 'listLanguageModels_statusEquals' - When specified, returns only custom language models with the specified
-- status. Language models are ordered by creation date, with the newest
-- models first. If you don\'t specify a status, Amazon Transcribe returns
-- all custom language models ordered by date.
newListLanguageModels ::
  ListLanguageModels
newListLanguageModels =
  ListLanguageModels'
    { nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      statusEquals = Core.Nothing
    }

-- | When included, fetches the next set of jobs if the result of the
-- previous request was truncated.
listLanguageModels_nextToken :: Lens.Lens' ListLanguageModels (Core.Maybe Core.Text)
listLanguageModels_nextToken = Lens.lens (\ListLanguageModels' {nextToken} -> nextToken) (\s@ListLanguageModels' {} a -> s {nextToken = a} :: ListLanguageModels)

-- | When specified, the custom language model names returned contain the
-- substring you\'ve specified.
listLanguageModels_nameContains :: Lens.Lens' ListLanguageModels (Core.Maybe Core.Text)
listLanguageModels_nameContains = Lens.lens (\ListLanguageModels' {nameContains} -> nameContains) (\s@ListLanguageModels' {} a -> s {nameContains = a} :: ListLanguageModels)

-- | The maximum number of language models to return in the response. If
-- there are fewer results in the list, the response contains only the
-- actual results.
listLanguageModels_maxResults :: Lens.Lens' ListLanguageModels (Core.Maybe Core.Natural)
listLanguageModels_maxResults = Lens.lens (\ListLanguageModels' {maxResults} -> maxResults) (\s@ListLanguageModels' {} a -> s {maxResults = a} :: ListLanguageModels)

-- | When specified, returns only custom language models with the specified
-- status. Language models are ordered by creation date, with the newest
-- models first. If you don\'t specify a status, Amazon Transcribe returns
-- all custom language models ordered by date.
listLanguageModels_statusEquals :: Lens.Lens' ListLanguageModels (Core.Maybe ModelStatus)
listLanguageModels_statusEquals = Lens.lens (\ListLanguageModels' {statusEquals} -> statusEquals) (\s@ListLanguageModels' {} a -> s {statusEquals = a} :: ListLanguageModels)

instance Core.AWSRequest ListLanguageModels where
  type
    AWSResponse ListLanguageModels =
      ListLanguageModelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLanguageModelsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Models" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLanguageModels

instance Core.NFData ListLanguageModels

instance Core.ToHeaders ListLanguageModels where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Transcribe.ListLanguageModels" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListLanguageModels where
  toJSON ListLanguageModels' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StatusEquals" Core..=) Core.<$> statusEquals
          ]
      )

instance Core.ToPath ListLanguageModels where
  toPath = Core.const "/"

instance Core.ToQuery ListLanguageModels where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListLanguageModelsResponse' smart constructor.
data ListLanguageModelsResponse = ListLanguageModelsResponse'
  { -- | The operation returns a page of jobs at a time. The maximum size of the
    -- list is set by the MaxResults parameter. If there are more language
    -- models in the list than the page size, Amazon Transcribe returns the
    -- @NextPage@ token. Include the token in the next request to the operation
    -- to return the next page of language models.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of objects containing information about custom language models.
    models :: Core.Maybe [LanguageModel],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLanguageModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLanguageModelsResponse_nextToken' - The operation returns a page of jobs at a time. The maximum size of the
-- list is set by the MaxResults parameter. If there are more language
-- models in the list than the page size, Amazon Transcribe returns the
-- @NextPage@ token. Include the token in the next request to the operation
-- to return the next page of language models.
--
-- 'models', 'listLanguageModelsResponse_models' - A list of objects containing information about custom language models.
--
-- 'httpStatus', 'listLanguageModelsResponse_httpStatus' - The response's http status code.
newListLanguageModelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListLanguageModelsResponse
newListLanguageModelsResponse pHttpStatus_ =
  ListLanguageModelsResponse'
    { nextToken =
        Core.Nothing,
      models = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The operation returns a page of jobs at a time. The maximum size of the
-- list is set by the MaxResults parameter. If there are more language
-- models in the list than the page size, Amazon Transcribe returns the
-- @NextPage@ token. Include the token in the next request to the operation
-- to return the next page of language models.
listLanguageModelsResponse_nextToken :: Lens.Lens' ListLanguageModelsResponse (Core.Maybe Core.Text)
listLanguageModelsResponse_nextToken = Lens.lens (\ListLanguageModelsResponse' {nextToken} -> nextToken) (\s@ListLanguageModelsResponse' {} a -> s {nextToken = a} :: ListLanguageModelsResponse)

-- | A list of objects containing information about custom language models.
listLanguageModelsResponse_models :: Lens.Lens' ListLanguageModelsResponse (Core.Maybe [LanguageModel])
listLanguageModelsResponse_models = Lens.lens (\ListLanguageModelsResponse' {models} -> models) (\s@ListLanguageModelsResponse' {} a -> s {models = a} :: ListLanguageModelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLanguageModelsResponse_httpStatus :: Lens.Lens' ListLanguageModelsResponse Core.Int
listLanguageModelsResponse_httpStatus = Lens.lens (\ListLanguageModelsResponse' {httpStatus} -> httpStatus) (\s@ListLanguageModelsResponse' {} a -> s {httpStatus = a} :: ListLanguageModelsResponse)

instance Core.NFData ListLanguageModelsResponse
