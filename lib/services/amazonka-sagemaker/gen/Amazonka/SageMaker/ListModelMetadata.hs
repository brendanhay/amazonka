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
-- Module      : Amazonka.SageMaker.ListModelMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domain, framework, task, and model name of standard machine
-- learning models found in common model zoos.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelMetadata
  ( -- * Creating a Request
    ListModelMetadata (..),
    newListModelMetadata,

    -- * Request Lenses
    listModelMetadata_maxResults,
    listModelMetadata_nextToken,
    listModelMetadata_searchExpression,

    -- * Destructuring the Response
    ListModelMetadataResponse (..),
    newListModelMetadataResponse,

    -- * Response Lenses
    listModelMetadataResponse_nextToken,
    listModelMetadataResponse_httpStatus,
    listModelMetadataResponse_modelMetadataSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelMetadata' smart constructor.
data ListModelMetadata = ListModelMetadata'
  { -- | The maximum number of models to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response to a previous @ListModelMetadataResponse@ request was
    -- truncated, the response includes a NextToken. To retrieve the next set
    -- of model metadata, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters that searches for the specified resource or
    -- resources in a search. All resource objects that satisfy the
    -- expression\'s condition are included in the search results. Specify the
    -- Framework, FrameworkVersion, Domain or Task to filter supported. Filter
    -- names and values are case-sensitive.
    searchExpression :: Prelude.Maybe ModelMetadataSearchExpression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listModelMetadata_maxResults' - The maximum number of models to return in the response.
--
-- 'nextToken', 'listModelMetadata_nextToken' - If the response to a previous @ListModelMetadataResponse@ request was
-- truncated, the response includes a NextToken. To retrieve the next set
-- of model metadata, use the token in the next request.
--
-- 'searchExpression', 'listModelMetadata_searchExpression' - One or more filters that searches for the specified resource or
-- resources in a search. All resource objects that satisfy the
-- expression\'s condition are included in the search results. Specify the
-- Framework, FrameworkVersion, Domain or Task to filter supported. Filter
-- names and values are case-sensitive.
newListModelMetadata ::
  ListModelMetadata
newListModelMetadata =
  ListModelMetadata'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchExpression = Prelude.Nothing
    }

-- | The maximum number of models to return in the response.
listModelMetadata_maxResults :: Lens.Lens' ListModelMetadata (Prelude.Maybe Prelude.Natural)
listModelMetadata_maxResults = Lens.lens (\ListModelMetadata' {maxResults} -> maxResults) (\s@ListModelMetadata' {} a -> s {maxResults = a} :: ListModelMetadata)

-- | If the response to a previous @ListModelMetadataResponse@ request was
-- truncated, the response includes a NextToken. To retrieve the next set
-- of model metadata, use the token in the next request.
listModelMetadata_nextToken :: Lens.Lens' ListModelMetadata (Prelude.Maybe Prelude.Text)
listModelMetadata_nextToken = Lens.lens (\ListModelMetadata' {nextToken} -> nextToken) (\s@ListModelMetadata' {} a -> s {nextToken = a} :: ListModelMetadata)

-- | One or more filters that searches for the specified resource or
-- resources in a search. All resource objects that satisfy the
-- expression\'s condition are included in the search results. Specify the
-- Framework, FrameworkVersion, Domain or Task to filter supported. Filter
-- names and values are case-sensitive.
listModelMetadata_searchExpression :: Lens.Lens' ListModelMetadata (Prelude.Maybe ModelMetadataSearchExpression)
listModelMetadata_searchExpression = Lens.lens (\ListModelMetadata' {searchExpression} -> searchExpression) (\s@ListModelMetadata' {} a -> s {searchExpression = a} :: ListModelMetadata)

instance Core.AWSPager ListModelMetadata where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelMetadataResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelMetadataResponse_modelMetadataSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listModelMetadata_nextToken
              Lens..~ rs
              Lens.^? listModelMetadataResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListModelMetadata where
  type
    AWSResponse ListModelMetadata =
      ListModelMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelMetadataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ModelMetadataSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelMetadata where
  hashWithSalt _salt ListModelMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchExpression

instance Prelude.NFData ListModelMetadata where
  rnf ListModelMetadata' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf searchExpression

instance Data.ToHeaders ListModelMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelMetadata where
  toJSON ListModelMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SearchExpression" Data..=)
              Prelude.<$> searchExpression
          ]
      )

instance Data.ToPath ListModelMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelMetadataResponse' smart constructor.
data ListModelMetadataResponse = ListModelMetadataResponse'
  { -- | A token for getting the next set of recommendations, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that holds model metadata.
    modelMetadataSummaries :: [ModelMetadataSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelMetadataResponse_nextToken' - A token for getting the next set of recommendations, if there are any.
--
-- 'httpStatus', 'listModelMetadataResponse_httpStatus' - The response's http status code.
--
-- 'modelMetadataSummaries', 'listModelMetadataResponse_modelMetadataSummaries' - A structure that holds model metadata.
newListModelMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelMetadataResponse
newListModelMetadataResponse pHttpStatus_ =
  ListModelMetadataResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelMetadataSummaries = Prelude.mempty
    }

-- | A token for getting the next set of recommendations, if there are any.
listModelMetadataResponse_nextToken :: Lens.Lens' ListModelMetadataResponse (Prelude.Maybe Prelude.Text)
listModelMetadataResponse_nextToken = Lens.lens (\ListModelMetadataResponse' {nextToken} -> nextToken) (\s@ListModelMetadataResponse' {} a -> s {nextToken = a} :: ListModelMetadataResponse)

-- | The response's http status code.
listModelMetadataResponse_httpStatus :: Lens.Lens' ListModelMetadataResponse Prelude.Int
listModelMetadataResponse_httpStatus = Lens.lens (\ListModelMetadataResponse' {httpStatus} -> httpStatus) (\s@ListModelMetadataResponse' {} a -> s {httpStatus = a} :: ListModelMetadataResponse)

-- | A structure that holds model metadata.
listModelMetadataResponse_modelMetadataSummaries :: Lens.Lens' ListModelMetadataResponse [ModelMetadataSummary]
listModelMetadataResponse_modelMetadataSummaries = Lens.lens (\ListModelMetadataResponse' {modelMetadataSummaries} -> modelMetadataSummaries) (\s@ListModelMetadataResponse' {} a -> s {modelMetadataSummaries = a} :: ListModelMetadataResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListModelMetadataResponse where
  rnf ListModelMetadataResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf modelMetadataSummaries
