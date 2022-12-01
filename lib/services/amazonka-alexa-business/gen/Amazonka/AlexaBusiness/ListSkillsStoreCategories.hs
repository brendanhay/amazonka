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
-- Module      : Amazonka.AlexaBusiness.ListSkillsStoreCategories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all categories in the Alexa skill store.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListSkillsStoreCategories
  ( -- * Creating a Request
    ListSkillsStoreCategories (..),
    newListSkillsStoreCategories,

    -- * Request Lenses
    listSkillsStoreCategories_nextToken,
    listSkillsStoreCategories_maxResults,

    -- * Destructuring the Response
    ListSkillsStoreCategoriesResponse (..),
    newListSkillsStoreCategoriesResponse,

    -- * Response Lenses
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSkillsStoreCategories' smart constructor.
data ListSkillsStoreCategories = ListSkillsStoreCategories'
  { -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of categories returned, per paginated calls.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSkillsStoreCategories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkillsStoreCategories_nextToken' - The tokens used for pagination.
--
-- 'maxResults', 'listSkillsStoreCategories_maxResults' - The maximum number of categories returned, per paginated calls.
newListSkillsStoreCategories ::
  ListSkillsStoreCategories
newListSkillsStoreCategories =
  ListSkillsStoreCategories'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The tokens used for pagination.
listSkillsStoreCategories_nextToken :: Lens.Lens' ListSkillsStoreCategories (Prelude.Maybe Prelude.Text)
listSkillsStoreCategories_nextToken = Lens.lens (\ListSkillsStoreCategories' {nextToken} -> nextToken) (\s@ListSkillsStoreCategories' {} a -> s {nextToken = a} :: ListSkillsStoreCategories)

-- | The maximum number of categories returned, per paginated calls.
listSkillsStoreCategories_maxResults :: Lens.Lens' ListSkillsStoreCategories (Prelude.Maybe Prelude.Natural)
listSkillsStoreCategories_maxResults = Lens.lens (\ListSkillsStoreCategories' {maxResults} -> maxResults) (\s@ListSkillsStoreCategories' {} a -> s {maxResults = a} :: ListSkillsStoreCategories)

instance Core.AWSPager ListSkillsStoreCategories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreCategoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreCategoriesResponse_categoryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSkillsStoreCategories_nextToken
          Lens..~ rs
          Lens.^? listSkillsStoreCategoriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSkillsStoreCategories where
  type
    AWSResponse ListSkillsStoreCategories =
      ListSkillsStoreCategoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsStoreCategoriesResponse'
            Prelude.<$> (x Core..?> "CategoryList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSkillsStoreCategories where
  hashWithSalt _salt ListSkillsStoreCategories' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSkillsStoreCategories where
  rnf ListSkillsStoreCategories' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSkillsStoreCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListSkillsStoreCategories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSkillsStoreCategories where
  toJSON ListSkillsStoreCategories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListSkillsStoreCategories where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSkillsStoreCategories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSkillsStoreCategoriesResponse' smart constructor.
data ListSkillsStoreCategoriesResponse = ListSkillsStoreCategoriesResponse'
  { -- | The list of categories.
    categoryList :: Prelude.Maybe [Category],
    -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSkillsStoreCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryList', 'listSkillsStoreCategoriesResponse_categoryList' - The list of categories.
--
-- 'nextToken', 'listSkillsStoreCategoriesResponse_nextToken' - The tokens used for pagination.
--
-- 'httpStatus', 'listSkillsStoreCategoriesResponse_httpStatus' - The response's http status code.
newListSkillsStoreCategoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSkillsStoreCategoriesResponse
newListSkillsStoreCategoriesResponse pHttpStatus_ =
  ListSkillsStoreCategoriesResponse'
    { categoryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of categories.
listSkillsStoreCategoriesResponse_categoryList :: Lens.Lens' ListSkillsStoreCategoriesResponse (Prelude.Maybe [Category])
listSkillsStoreCategoriesResponse_categoryList = Lens.lens (\ListSkillsStoreCategoriesResponse' {categoryList} -> categoryList) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {categoryList = a} :: ListSkillsStoreCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tokens used for pagination.
listSkillsStoreCategoriesResponse_nextToken :: Lens.Lens' ListSkillsStoreCategoriesResponse (Prelude.Maybe Prelude.Text)
listSkillsStoreCategoriesResponse_nextToken = Lens.lens (\ListSkillsStoreCategoriesResponse' {nextToken} -> nextToken) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {nextToken = a} :: ListSkillsStoreCategoriesResponse)

-- | The response's http status code.
listSkillsStoreCategoriesResponse_httpStatus :: Lens.Lens' ListSkillsStoreCategoriesResponse Prelude.Int
listSkillsStoreCategoriesResponse_httpStatus = Lens.lens (\ListSkillsStoreCategoriesResponse' {httpStatus} -> httpStatus) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {httpStatus = a} :: ListSkillsStoreCategoriesResponse)

instance
  Prelude.NFData
    ListSkillsStoreCategoriesResponse
  where
  rnf ListSkillsStoreCategoriesResponse' {..} =
    Prelude.rnf categoryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
