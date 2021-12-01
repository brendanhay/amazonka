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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    listImageRecipes_filters,
    listImageRecipes_owner,
    listImageRecipes_nextToken,
    listImageRecipes_maxResults,

    -- * Destructuring the Response
    ListImageRecipesResponse (..),
    newListImageRecipesResponse,

    -- * Response Lenses
    listImageRecipesResponse_requestId,
    listImageRecipesResponse_nextToken,
    listImageRecipesResponse_imageRecipeSummaryList,
    listImageRecipesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImageRecipes' smart constructor.
data ListImageRecipes = ListImageRecipes'
  { -- | Use the following filters to streamline results:
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
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listImageRecipes_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'maxResults', 'listImageRecipes_maxResults' - The maximum items to return in a request.
newListImageRecipes ::
  ListImageRecipes
newListImageRecipes =
  ListImageRecipes'
    { filters = Prelude.Nothing,
      owner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImageRecipes_nextToken :: Lens.Lens' ListImageRecipes (Prelude.Maybe Prelude.Text)
listImageRecipes_nextToken = Lens.lens (\ListImageRecipes' {nextToken} -> nextToken) (\s@ListImageRecipes' {} a -> s {nextToken = a} :: ListImageRecipes)

-- | The maximum items to return in a request.
listImageRecipes_maxResults :: Lens.Lens' ListImageRecipes (Prelude.Maybe Prelude.Natural)
listImageRecipes_maxResults = Lens.lens (\ListImageRecipes' {maxResults} -> maxResults) (\s@ListImageRecipes' {} a -> s {maxResults = a} :: ListImageRecipes)

instance Core.AWSRequest ListImageRecipes where
  type
    AWSResponse ListImageRecipes =
      ListImageRecipesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageRecipesResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "imageRecipeSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImageRecipes where
  hashWithSalt salt' ListImageRecipes' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListImageRecipes where
  rnf ListImageRecipes' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owner

instance Core.ToHeaders ListImageRecipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImageRecipes where
  toJSON ListImageRecipes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("owner" Core..=) Prelude.<$> owner,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListImageRecipes where
  toPath = Prelude.const "/ListImageRecipes"

instance Core.ToQuery ListImageRecipes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImageRecipesResponse' smart constructor.
data ListImageRecipesResponse = ListImageRecipesResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'requestId', 'listImageRecipesResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'nextToken', 'listImageRecipesResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
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
    { requestId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imageRecipeSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
listImageRecipesResponse_requestId :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe Prelude.Text)
listImageRecipesResponse_requestId = Lens.lens (\ListImageRecipesResponse' {requestId} -> requestId) (\s@ListImageRecipesResponse' {} a -> s {requestId = a} :: ListImageRecipesResponse)

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listImageRecipesResponse_nextToken :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe Prelude.Text)
listImageRecipesResponse_nextToken = Lens.lens (\ListImageRecipesResponse' {nextToken} -> nextToken) (\s@ListImageRecipesResponse' {} a -> s {nextToken = a} :: ListImageRecipesResponse)

-- | The list of image pipelines.
listImageRecipesResponse_imageRecipeSummaryList :: Lens.Lens' ListImageRecipesResponse (Prelude.Maybe [ImageRecipeSummary])
listImageRecipesResponse_imageRecipeSummaryList = Lens.lens (\ListImageRecipesResponse' {imageRecipeSummaryList} -> imageRecipeSummaryList) (\s@ListImageRecipesResponse' {} a -> s {imageRecipeSummaryList = a} :: ListImageRecipesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImageRecipesResponse_httpStatus :: Lens.Lens' ListImageRecipesResponse Prelude.Int
listImageRecipesResponse_httpStatus = Lens.lens (\ListImageRecipesResponse' {httpStatus} -> httpStatus) (\s@ListImageRecipesResponse' {} a -> s {httpStatus = a} :: ListImageRecipesResponse)

instance Prelude.NFData ListImageRecipesResponse where
  rnf ListImageRecipesResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf imageRecipeSummaryList
      `Prelude.seq` Prelude.rnf nextToken
