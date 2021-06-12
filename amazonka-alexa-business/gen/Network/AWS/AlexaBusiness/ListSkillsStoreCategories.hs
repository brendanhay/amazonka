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
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreCategories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all categories in the Alexa skill store.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreCategories
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
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSkillsStoreCategories' smart constructor.
data ListSkillsStoreCategories = ListSkillsStoreCategories'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of categories returned, per paginated calls.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The tokens used for pagination.
listSkillsStoreCategories_nextToken :: Lens.Lens' ListSkillsStoreCategories (Core.Maybe Core.Text)
listSkillsStoreCategories_nextToken = Lens.lens (\ListSkillsStoreCategories' {nextToken} -> nextToken) (\s@ListSkillsStoreCategories' {} a -> s {nextToken = a} :: ListSkillsStoreCategories)

-- | The maximum number of categories returned, per paginated calls.
listSkillsStoreCategories_maxResults :: Lens.Lens' ListSkillsStoreCategories (Core.Maybe Core.Natural)
listSkillsStoreCategories_maxResults = Lens.lens (\ListSkillsStoreCategories' {maxResults} -> maxResults) (\s@ListSkillsStoreCategories' {} a -> s {maxResults = a} :: ListSkillsStoreCategories)

instance Core.AWSPager ListSkillsStoreCategories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreCategoriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreCategoriesResponse_categoryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSkillsStoreCategories_nextToken
          Lens..~ rs
          Lens.^? listSkillsStoreCategoriesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSkillsStoreCategories where
  type
    AWSResponse ListSkillsStoreCategories =
      ListSkillsStoreCategoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsStoreCategoriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "CategoryList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSkillsStoreCategories

instance Core.NFData ListSkillsStoreCategories

instance Core.ToHeaders ListSkillsStoreCategories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListSkillsStoreCategories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSkillsStoreCategories where
  toJSON ListSkillsStoreCategories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListSkillsStoreCategories where
  toPath = Core.const "/"

instance Core.ToQuery ListSkillsStoreCategories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSkillsStoreCategoriesResponse' smart constructor.
data ListSkillsStoreCategoriesResponse = ListSkillsStoreCategoriesResponse'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of categories.
    categoryList :: Core.Maybe [Category],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSkillsStoreCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkillsStoreCategoriesResponse_nextToken' - The tokens used for pagination.
--
-- 'categoryList', 'listSkillsStoreCategoriesResponse_categoryList' - The list of categories.
--
-- 'httpStatus', 'listSkillsStoreCategoriesResponse_httpStatus' - The response's http status code.
newListSkillsStoreCategoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSkillsStoreCategoriesResponse
newListSkillsStoreCategoriesResponse pHttpStatus_ =
  ListSkillsStoreCategoriesResponse'
    { nextToken =
        Core.Nothing,
      categoryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tokens used for pagination.
listSkillsStoreCategoriesResponse_nextToken :: Lens.Lens' ListSkillsStoreCategoriesResponse (Core.Maybe Core.Text)
listSkillsStoreCategoriesResponse_nextToken = Lens.lens (\ListSkillsStoreCategoriesResponse' {nextToken} -> nextToken) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {nextToken = a} :: ListSkillsStoreCategoriesResponse)

-- | The list of categories.
listSkillsStoreCategoriesResponse_categoryList :: Lens.Lens' ListSkillsStoreCategoriesResponse (Core.Maybe [Category])
listSkillsStoreCategoriesResponse_categoryList = Lens.lens (\ListSkillsStoreCategoriesResponse' {categoryList} -> categoryList) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {categoryList = a} :: ListSkillsStoreCategoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSkillsStoreCategoriesResponse_httpStatus :: Lens.Lens' ListSkillsStoreCategoriesResponse Core.Int
listSkillsStoreCategoriesResponse_httpStatus = Lens.lens (\ListSkillsStoreCategoriesResponse' {httpStatus} -> httpStatus) (\s@ListSkillsStoreCategoriesResponse' {} a -> s {httpStatus = a} :: ListSkillsStoreCategoriesResponse)

instance
  Core.NFData
    ListSkillsStoreCategoriesResponse
