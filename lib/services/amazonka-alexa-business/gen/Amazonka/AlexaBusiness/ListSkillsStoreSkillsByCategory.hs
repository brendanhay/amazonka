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
-- Module      : Amazonka.AlexaBusiness.ListSkillsStoreSkillsByCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all skills in the Alexa skill store by category.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.ListSkillsStoreSkillsByCategory
  ( -- * Creating a Request
    ListSkillsStoreSkillsByCategory (..),
    newListSkillsStoreSkillsByCategory,

    -- * Request Lenses
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_categoryId,

    -- * Destructuring the Response
    ListSkillsStoreSkillsByCategoryResponse (..),
    newListSkillsStoreSkillsByCategoryResponse,

    -- * Response Lenses
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSkillsStoreSkillsByCategory' smart constructor.
data ListSkillsStoreSkillsByCategory = ListSkillsStoreSkillsByCategory'
  { -- | The maximum number of skills returned per paginated calls.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The category ID for which the skills are being retrieved from the skill
    -- store.
    categoryId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSkillsStoreSkillsByCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSkillsStoreSkillsByCategory_maxResults' - The maximum number of skills returned per paginated calls.
--
-- 'nextToken', 'listSkillsStoreSkillsByCategory_nextToken' - The tokens used for pagination.
--
-- 'categoryId', 'listSkillsStoreSkillsByCategory_categoryId' - The category ID for which the skills are being retrieved from the skill
-- store.
newListSkillsStoreSkillsByCategory ::
  -- | 'categoryId'
  Prelude.Natural ->
  ListSkillsStoreSkillsByCategory
newListSkillsStoreSkillsByCategory pCategoryId_ =
  ListSkillsStoreSkillsByCategory'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      categoryId = pCategoryId_
    }

-- | The maximum number of skills returned per paginated calls.
listSkillsStoreSkillsByCategory_maxResults :: Lens.Lens' ListSkillsStoreSkillsByCategory (Prelude.Maybe Prelude.Natural)
listSkillsStoreSkillsByCategory_maxResults = Lens.lens (\ListSkillsStoreSkillsByCategory' {maxResults} -> maxResults) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {maxResults = a} :: ListSkillsStoreSkillsByCategory)

-- | The tokens used for pagination.
listSkillsStoreSkillsByCategory_nextToken :: Lens.Lens' ListSkillsStoreSkillsByCategory (Prelude.Maybe Prelude.Text)
listSkillsStoreSkillsByCategory_nextToken = Lens.lens (\ListSkillsStoreSkillsByCategory' {nextToken} -> nextToken) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategory)

-- | The category ID for which the skills are being retrieved from the skill
-- store.
listSkillsStoreSkillsByCategory_categoryId :: Lens.Lens' ListSkillsStoreSkillsByCategory Prelude.Natural
listSkillsStoreSkillsByCategory_categoryId = Lens.lens (\ListSkillsStoreSkillsByCategory' {categoryId} -> categoryId) (\s@ListSkillsStoreSkillsByCategory' {} a -> s {categoryId = a} :: ListSkillsStoreSkillsByCategory)

instance
  Core.AWSPager
    ListSkillsStoreSkillsByCategory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreSkillsByCategoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSkillsStoreSkillsByCategory_nextToken
          Lens..~ rs
          Lens.^? listSkillsStoreSkillsByCategoryResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListSkillsStoreSkillsByCategory
  where
  type
    AWSResponse ListSkillsStoreSkillsByCategory =
      ListSkillsStoreSkillsByCategoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSkillsStoreSkillsByCategoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SkillsStoreSkills"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSkillsStoreSkillsByCategory
  where
  hashWithSalt
    _salt
    ListSkillsStoreSkillsByCategory' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` categoryId

instance
  Prelude.NFData
    ListSkillsStoreSkillsByCategory
  where
  rnf ListSkillsStoreSkillsByCategory' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf categoryId

instance
  Data.ToHeaders
    ListSkillsStoreSkillsByCategory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ListSkillsStoreSkillsByCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSkillsStoreSkillsByCategory where
  toJSON ListSkillsStoreSkillsByCategory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("CategoryId" Data..= categoryId)
          ]
      )

instance Data.ToPath ListSkillsStoreSkillsByCategory where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSkillsStoreSkillsByCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSkillsStoreSkillsByCategoryResponse' smart constructor.
data ListSkillsStoreSkillsByCategoryResponse = ListSkillsStoreSkillsByCategoryResponse'
  { -- | The tokens used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The skill store skills.
    skillsStoreSkills :: Prelude.Maybe [SkillsStoreSkill],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSkillsStoreSkillsByCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSkillsStoreSkillsByCategoryResponse_nextToken' - The tokens used for pagination.
--
-- 'skillsStoreSkills', 'listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills' - The skill store skills.
--
-- 'httpStatus', 'listSkillsStoreSkillsByCategoryResponse_httpStatus' - The response's http status code.
newListSkillsStoreSkillsByCategoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSkillsStoreSkillsByCategoryResponse
newListSkillsStoreSkillsByCategoryResponse
  pHttpStatus_ =
    ListSkillsStoreSkillsByCategoryResponse'
      { nextToken =
          Prelude.Nothing,
        skillsStoreSkills =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The tokens used for pagination.
listSkillsStoreSkillsByCategoryResponse_nextToken :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Prelude.Maybe Prelude.Text)
listSkillsStoreSkillsByCategoryResponse_nextToken = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {nextToken} -> nextToken) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategoryResponse)

-- | The skill store skills.
listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Prelude.Maybe [SkillsStoreSkill])
listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {skillsStoreSkills} -> skillsStoreSkills) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {skillsStoreSkills = a} :: ListSkillsStoreSkillsByCategoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSkillsStoreSkillsByCategoryResponse_httpStatus :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse Prelude.Int
listSkillsStoreSkillsByCategoryResponse_httpStatus = Lens.lens (\ListSkillsStoreSkillsByCategoryResponse' {httpStatus} -> httpStatus) (\s@ListSkillsStoreSkillsByCategoryResponse' {} a -> s {httpStatus = a} :: ListSkillsStoreSkillsByCategoryResponse)

instance
  Prelude.NFData
    ListSkillsStoreSkillsByCategoryResponse
  where
  rnf ListSkillsStoreSkillsByCategoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf skillsStoreSkills
      `Prelude.seq` Prelude.rnf httpStatus
