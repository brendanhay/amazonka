{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all skills in the Alexa skill store by category.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
  ( -- * Creating a request
    ListSkillsStoreSkillsByCategory (..),
    mkListSkillsStoreSkillsByCategory,

    -- ** Request lenses
    lsssbcNextToken,
    lsssbcCategoryId,
    lsssbcMaxResults,

    -- * Destructuring the response
    ListSkillsStoreSkillsByCategoryResponse (..),
    mkListSkillsStoreSkillsByCategoryResponse,

    -- ** Response lenses
    lsssbcrsNextToken,
    lsssbcrsSkillsStoreSkills,
    lsssbcrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSkillsStoreSkillsByCategory' smart constructor.
data ListSkillsStoreSkillsByCategory = ListSkillsStoreSkillsByCategory'
  { -- | The tokens used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The category ID for which the skills are being retrieved from the skill store.
    categoryId :: Lude.Natural,
    -- | The maximum number of skills returned per paginated calls.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkillsStoreSkillsByCategory' with the minimum fields required to make a request.
--
-- * 'nextToken' - The tokens used for pagination.
-- * 'categoryId' - The category ID for which the skills are being retrieved from the skill store.
-- * 'maxResults' - The maximum number of skills returned per paginated calls.
mkListSkillsStoreSkillsByCategory ::
  -- | 'categoryId'
  Lude.Natural ->
  ListSkillsStoreSkillsByCategory
mkListSkillsStoreSkillsByCategory pCategoryId_ =
  ListSkillsStoreSkillsByCategory'
    { nextToken = Lude.Nothing,
      categoryId = pCategoryId_,
      maxResults = Lude.Nothing
    }

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcNextToken :: Lens.Lens' ListSkillsStoreSkillsByCategory (Lude.Maybe Lude.Text)
lsssbcNextToken = Lens.lens (nextToken :: ListSkillsStoreSkillsByCategory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategory)
{-# DEPRECATED lsssbcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The category ID for which the skills are being retrieved from the skill store.
--
-- /Note:/ Consider using 'categoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcCategoryId :: Lens.Lens' ListSkillsStoreSkillsByCategory Lude.Natural
lsssbcCategoryId = Lens.lens (categoryId :: ListSkillsStoreSkillsByCategory -> Lude.Natural) (\s a -> s {categoryId = a} :: ListSkillsStoreSkillsByCategory)
{-# DEPRECATED lsssbcCategoryId "Use generic-lens or generic-optics with 'categoryId' instead." #-}

-- | The maximum number of skills returned per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcMaxResults :: Lens.Lens' ListSkillsStoreSkillsByCategory (Lude.Maybe Lude.Natural)
lsssbcMaxResults = Lens.lens (maxResults :: ListSkillsStoreSkillsByCategory -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSkillsStoreSkillsByCategory)
{-# DEPRECATED lsssbcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSkillsStoreSkillsByCategory where
  page rq rs
    | Page.stop (rs Lens.^. lsssbcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsssbcrsSkillsStoreSkills) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsssbcNextToken Lens..~ rs Lens.^. lsssbcrsNextToken

instance Lude.AWSRequest ListSkillsStoreSkillsByCategory where
  type
    Rs ListSkillsStoreSkillsByCategory =
      ListSkillsStoreSkillsByCategoryResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSkillsStoreSkillsByCategoryResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SkillsStoreSkills" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSkillsStoreSkillsByCategory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.ListSkillsStoreSkillsByCategory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSkillsStoreSkillsByCategory where
  toJSON ListSkillsStoreSkillsByCategory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("CategoryId" Lude..= categoryId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSkillsStoreSkillsByCategory where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSkillsStoreSkillsByCategory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSkillsStoreSkillsByCategoryResponse' smart constructor.
data ListSkillsStoreSkillsByCategoryResponse = ListSkillsStoreSkillsByCategoryResponse'
  { -- | The tokens used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The skill store skills.
    skillsStoreSkills :: Lude.Maybe [SkillsStoreSkill],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkillsStoreSkillsByCategoryResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The tokens used for pagination.
-- * 'skillsStoreSkills' - The skill store skills.
-- * 'responseStatus' - The response status code.
mkListSkillsStoreSkillsByCategoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSkillsStoreSkillsByCategoryResponse
mkListSkillsStoreSkillsByCategoryResponse pResponseStatus_ =
  ListSkillsStoreSkillsByCategoryResponse'
    { nextToken =
        Lude.Nothing,
      skillsStoreSkills = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrsNextToken :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Lude.Maybe Lude.Text)
lsssbcrsNextToken = Lens.lens (nextToken :: ListSkillsStoreSkillsByCategoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkillsStoreSkillsByCategoryResponse)
{-# DEPRECATED lsssbcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The skill store skills.
--
-- /Note:/ Consider using 'skillsStoreSkills' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrsSkillsStoreSkills :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse (Lude.Maybe [SkillsStoreSkill])
lsssbcrsSkillsStoreSkills = Lens.lens (skillsStoreSkills :: ListSkillsStoreSkillsByCategoryResponse -> Lude.Maybe [SkillsStoreSkill]) (\s a -> s {skillsStoreSkills = a} :: ListSkillsStoreSkillsByCategoryResponse)
{-# DEPRECATED lsssbcrsSkillsStoreSkills "Use generic-lens or generic-optics with 'skillsStoreSkills' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsssbcrsResponseStatus :: Lens.Lens' ListSkillsStoreSkillsByCategoryResponse Lude.Int
lsssbcrsResponseStatus = Lens.lens (responseStatus :: ListSkillsStoreSkillsByCategoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSkillsStoreSkillsByCategoryResponse)
{-# DEPRECATED lsssbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
