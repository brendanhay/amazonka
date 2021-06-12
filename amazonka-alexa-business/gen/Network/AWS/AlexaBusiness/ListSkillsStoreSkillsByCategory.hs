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
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all skills in the Alexa skill store by category.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
  ( -- * Creating a Request
    ListSkillsStoreSkillsByCategory (..),
    newListSkillsStoreSkillsByCategory,

    -- * Request Lenses
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_categoryId,

    -- * Destructuring the Response
    ListSkillsStoreSkillsByCategoryResponse (..),
    newListSkillsStoreSkillsByCategoryResponse,

    -- * Response Lenses
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSkillsStoreSkillsByCategory' smart constructor.
data ListSkillsStoreSkillsByCategory = ListSkillsStoreSkillsByCategory'
  { -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of skills returned per paginated calls.
    maxResults :: Core.Maybe Core.Natural,
    -- | The category ID for which the skills are being retrieved from the skill
    -- store.
    categoryId :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSkillsStoreSkillsByCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkillsStoreSkillsByCategory_nextToken' - The tokens used for pagination.
--
-- 'maxResults', 'listSkillsStoreSkillsByCategory_maxResults' - The maximum number of skills returned per paginated calls.
--
-- 'categoryId', 'listSkillsStoreSkillsByCategory_categoryId' - The category ID for which the skills are being retrieved from the skill
-- store.
newListSkillsStoreSkillsByCategory ::
  -- | 'categoryId'
  Core.Natural ->
  ListSkillsStoreSkillsByCategory
newListSkillsStoreSkillsByCategory pCategoryId_ =
  ListSkillsStoreSkillsByCategory'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      categoryId = pCategoryId_
    }

-- | The tokens used for pagination.
listSkillsStoreSkillsByCategory_nextToken :: Lens.Lens' ListSkillsStoreSkillsByCategory (Core.Maybe Core.Text)
listSkillsStoreSkillsByCategory_nextToken = Lens.lens (\ListSkillsStoreSkillsByCategory' {nextToken} -> nextToken) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategory)

-- | The maximum number of skills returned per paginated calls.
listSkillsStoreSkillsByCategory_maxResults :: Lens.Lens' ListSkillsStoreSkillsByCategory (Core.Maybe Core.Natural)
listSkillsStoreSkillsByCategory_maxResults = Lens.lens (\ListSkillsStoreSkillsByCategory' {maxResults} -> maxResults) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {maxResults = a} :: ListSkillsStoreSkillsByCategory)

-- | The category ID for which the skills are being retrieved from the skill
-- store.
listSkillsStoreSkillsByCategory_categoryId :: Lens.Lens' ListSkillsStoreSkillsByCategory Core.Natural
listSkillsStoreSkillsByCategory_categoryId = Lens.lens (\ListSkillsStoreSkillsByCategory' {categoryId} -> categoryId) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {categoryId = a} :: ListSkillsStoreSkillsByCategory)

instance
  Core.AWSPager
    ListSkillsStoreSkillsByCategory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreSkillsByCategoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSkillsStoreSkillsByCategory_nextToken
          Lens..~ rs
          Lens.^? listSkillsStoreSkillsByCategoryResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListSkillsStoreSkillsByCategory
  where
  type
    AWSResponse ListSkillsStoreSkillsByCategory =
      ListSkillsStoreSkillsByCategoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsStoreSkillsByCategoryResponse'
            Core.<$> (x Core..?> "SkillsStoreSkills" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListSkillsStoreSkillsByCategory

instance Core.NFData ListSkillsStoreSkillsByCategory

instance
  Core.ToHeaders
    ListSkillsStoreSkillsByCategory
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ListSkillsStoreSkillsByCategory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSkillsStoreSkillsByCategory where
  toJSON ListSkillsStoreSkillsByCategory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("CategoryId" Core..= categoryId)
          ]
      )

instance Core.ToPath ListSkillsStoreSkillsByCategory where
  toPath = Core.const "/"

instance Core.ToQuery ListSkillsStoreSkillsByCategory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSkillsStoreSkillsByCategoryResponse' smart constructor.
data ListSkillsStoreSkillsByCategoryResponse = ListSkillsStoreSkillsByCategoryResponse'
  { -- | The skill store skills.
    skillsStoreSkills :: Core.Maybe [SkillsStoreSkill],
    -- | The tokens used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSkillsStoreSkillsByCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillsStoreSkills', 'listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills' - The skill store skills.
--
-- 'nextToken', 'listSkillsStoreSkillsByCategoryResponse_nextToken' - The tokens used for pagination.
--
-- 'httpStatus', 'listSkillsStoreSkillsByCategoryResponse_httpStatus' - The response's http status code.
newListSkillsStoreSkillsByCategoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSkillsStoreSkillsByCategoryResponse
newListSkillsStoreSkillsByCategoryResponse
  pHttpStatus_ =
    ListSkillsStoreSkillsByCategoryResponse'
      { skillsStoreSkills =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The skill store skills.
listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Core.Maybe [SkillsStoreSkill])
listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {skillsStoreSkills} -> skillsStoreSkills) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {skillsStoreSkills = a} :: ListSkillsStoreSkillsByCategoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The tokens used for pagination.
listSkillsStoreSkillsByCategoryResponse_nextToken :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Core.Maybe Core.Text)
listSkillsStoreSkillsByCategoryResponse_nextToken = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {nextToken} -> nextToken) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategoryResponse)

-- | The response's http status code.
listSkillsStoreSkillsByCategoryResponse_httpStatus :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse Core.Int
listSkillsStoreSkillsByCategoryResponse_httpStatus = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {httpStatus} -> httpStatus) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {httpStatus = a} :: ListSkillsStoreSkillsByCategoryResponse)

instance
  Core.NFData
    ListSkillsStoreSkillsByCategoryResponse
