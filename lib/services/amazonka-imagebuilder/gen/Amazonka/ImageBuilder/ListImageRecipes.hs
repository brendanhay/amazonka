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
-- Module      : Amazonka.ImageBuilder.ListImageRecipes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of image recipes.
module Amazonka.ImageBuilder.ListImageRecipes
  ( -- * Creating a Request
    ListImageRecipes (..),
    newListImageRecipes,

    -- * Request Lenses
    listImageRecipes_nextToken,
    listImageRecipes_filters,
    listImageRecipes_owner,
    listImageRecipes_maxResults,

    -- * Destructuring the Response
    ListImageRecipesResponse (..),
    newListImageRecipesResponse,

    -- * Response Lenses
    listImageRecipesResponse_nextToken,
    listImageRecipesResponse_requestId,
    listImageRecipesResponse_imageRecipeSummaryList,
    listImageRecipesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImageRecipes' smart constructor.
data ListImageRecipes = ListImageRecipes'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use the following filters to streamline results:
    --
    -- -   @name@
    --
    -- -   @parentImage@
    --
    -- -   @platform@
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The owner defines which image recipes you want to list. By default, this
    -- request will only show image recipes owned by your account. You can use
    -- this field to specify if you want to view image recipes owned by
    -- yourself, by Amazon, or those image recipes that have been shared with
    -- you by other customers.
    owner :: Prelude.Maybe Ownership,
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageRecipes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImageRecipes_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'filters', 'listImageRecipes_filters' - Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @parentImage@
--
-- -   @platform@
--
-- 'owner', 'listImageRecipes_owner' - The owner defines which image recipes you want to list. By default, this
-- request will only show image recipes owned by your account. You can use
-- this field to specify if you want to view image recipes owned by
-- yourself, by Amazon, or those image recipes that have been shared with
-- you by other customers.
--
-- 'maxResults', 'listImageRecipes_maxResults' - The maximum items to return in a request.
newListImageRecipes ::
  ListImageRecipes
newListImageRecipes =
  ListImageRecipes'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      owner = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImageRecipes_nextToken :: Lens.Lens' ListImageRecipes (Prelude.Maybe Prelude.Text)
listImageRecipes_nextToken = Lens.lens (\ListImageRecipes' {nextToken} -> nextToken) (\s@ListImageRecipes' {} a -> s {nextToken = a} :: ListImageRecipes)

-- | Use the following filters to streamline results:
--
-- -   @name@
--
-- -   @parentImage@
--
-- -   @platform@
listImageRecipes_filters :: Lens.Lens' ListImageRecipes (Prelude.Maybe (Prelude.NonEmpty Filter))
listImageRecipes_filters = Lens.lens (\ListImageRecipes' {filters} -> filters) (\s@ListImageRecipes' {} a -> s {filters = a} :: ListImageRecipes) Prelude.. Lens.mapping Lens.coerced

-- | The owner defines which image recipes you want to list. By default, this
-- request will only show image recipes owned by your account. You can use
-- this field to specify if you want to view image recipes owned by
-- yourself, by Amazon, or those image recipes that have been shared with
-- you by other customers.
listImageRecipes_owner :: Lens.Lens' ListImageRecipes (Prelude.Maybe Ownership)
listImageRecipes_owner = Lens.lens (\ListImageRecipes' {owner} -> owner) (\s@ListImageRecipes' {} a -> s {owner = a} :: ListImageRecipes)

-- | The maximum items to return in a request.
listImageRecipes_maxResults :: Lens.Lens' ListImageRecipes (Prelude.Maybe Prelude.Natural)
listImageRecipes_maxResults = Lens.lens (\ListImageRecipes' {maxResults} -> maxResults) (\s@ListImageRecipes' {} a -> s {maxResults = a} :: ListImageRecipes)

instance Core.AWSRequest ListImageRecipes where
  type
    AWSResponse ListImageRecipes =
      ListImageRecipesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageRecipesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> ( x Data..?> "imageRecipeSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImageRecipes where
  hashWithSalt _salt ListImageRecipes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListImageRecipes where
  rnf ListImageRecipes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListImageRecipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImageRecipes where
  toJSON ListImageRecipes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("owner" Data..=) Prelude.<$> owner,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListImageRecipes where
  toPath = Prelude.const "/ListImageRecipes"

instance Data.ToQuery ListImageRecipes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImageRecipesResponse' smart constructor.
data ListImageRecipesResponse = ListImageRecipesResponse'
  { -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The list of image pipelines.
    imageRecipeSummaryList :: Prelude.Maybe [ImageRecipeSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageRecipesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImageRecipesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'requestId', 'listImageRecipesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageRecipeSummaryList', 'listImageRecipesResponse_imageRecipeSummaryList' - The list of image pipelines.
--
-- 'httpStatus', 'listImageRecipesResponse_httpStatus' - The response's http status code.
newListImageRecipesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImageRecipesResponse
newListImageRecipesResponse pHttpStatus_ =
  ListImageRecipesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      imageRecipeSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImageRecipesResponse_nextToken :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe Prelude.Text)
listImageRecipesResponse_nextToken = Lens.lens (\ListImageRecipesResponse' {nextToken} -> nextToken) (\s@ListImageRecipesResponse' {} a -> s {nextToken = a} :: ListImageRecipesResponse)

-- | The request ID that uniquely identifies this request.
listImageRecipesResponse_requestId :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe Prelude.Text)
listImageRecipesResponse_requestId = Lens.lens (\ListImageRecipesResponse' {requestId} -> requestId) (\s@ListImageRecipesResponse' {} a -> s {requestId = a} :: ListImageRecipesResponse)

-- | The list of image pipelines.
listImageRecipesResponse_imageRecipeSummaryList :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe [ImageRecipeSummary])
listImageRecipesResponse_imageRecipeSummaryList = Lens.lens (\ListImageRecipesResponse' {imageRecipeSummaryList} -> imageRecipeSummaryList) (\s@ListImageRecipesResponse' {} a -> s {imageRecipeSummaryList = a} :: ListImageRecipesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImageRecipesResponse_httpStatus :: Lens.Lens' ListImageRecipesResponse Prelude.Int
listImageRecipesResponse_httpStatus = Lens.lens (\ListImageRecipesResponse' {httpStatus} -> httpStatus) (\s@ListImageRecipesResponse' {} a -> s {httpStatus = a} :: ListImageRecipesResponse)

instance Prelude.NFData ListImageRecipesResponse where
  rnf ListImageRecipesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageRecipeSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
