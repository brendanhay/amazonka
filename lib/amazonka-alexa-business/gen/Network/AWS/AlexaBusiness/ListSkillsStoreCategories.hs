{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreCategories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all categories in the Alexa skill store.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreCategories
  ( -- * Creating a request
    ListSkillsStoreCategories (..),
    mkListSkillsStoreCategories,

    -- ** Request lenses
    lsscNextToken,
    lsscMaxResults,

    -- * Destructuring the response
    ListSkillsStoreCategoriesResponse (..),
    mkListSkillsStoreCategoriesResponse,

    -- ** Response lenses
    lsscrsCategoryList,
    lsscrsNextToken,
    lsscrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSkillsStoreCategories' smart constructor.
data ListSkillsStoreCategories = ListSkillsStoreCategories'
  { -- | The tokens used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of categories returned, per paginated calls.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkillsStoreCategories' with the minimum fields required to make a request.
--
-- * 'nextToken' - The tokens used for pagination.
-- * 'maxResults' - The maximum number of categories returned, per paginated calls.
mkListSkillsStoreCategories ::
  ListSkillsStoreCategories
mkListSkillsStoreCategories =
  ListSkillsStoreCategories'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscNextToken :: Lens.Lens' ListSkillsStoreCategories (Lude.Maybe Lude.Text)
lsscNextToken = Lens.lens (nextToken :: ListSkillsStoreCategories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkillsStoreCategories)
{-# DEPRECATED lsscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of categories returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscMaxResults :: Lens.Lens' ListSkillsStoreCategories (Lude.Maybe Lude.Natural)
lsscMaxResults = Lens.lens (maxResults :: ListSkillsStoreCategories -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSkillsStoreCategories)
{-# DEPRECATED lsscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSkillsStoreCategories where
  page rq rs
    | Page.stop (rs Lens.^. lsscrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsscrsCategoryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsscNextToken Lens..~ rs Lens.^. lsscrsNextToken

instance Lude.AWSRequest ListSkillsStoreCategories where
  type
    Rs ListSkillsStoreCategories =
      ListSkillsStoreCategoriesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSkillsStoreCategoriesResponse'
            Lude.<$> (x Lude..?> "CategoryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSkillsStoreCategories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListSkillsStoreCategories" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSkillsStoreCategories where
  toJSON ListSkillsStoreCategories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSkillsStoreCategories where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSkillsStoreCategories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSkillsStoreCategoriesResponse' smart constructor.
data ListSkillsStoreCategoriesResponse = ListSkillsStoreCategoriesResponse'
  { -- | The list of categories.
    categoryList :: Lude.Maybe [Category],
    -- | The tokens used for pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSkillsStoreCategoriesResponse' with the minimum fields required to make a request.
--
-- * 'categoryList' - The list of categories.
-- * 'nextToken' - The tokens used for pagination.
-- * 'responseStatus' - The response status code.
mkListSkillsStoreCategoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSkillsStoreCategoriesResponse
mkListSkillsStoreCategoriesResponse pResponseStatus_ =
  ListSkillsStoreCategoriesResponse'
    { categoryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of categories.
--
-- /Note:/ Consider using 'categoryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrsCategoryList :: Lens.Lens' ListSkillsStoreCategoriesResponse (Lude.Maybe [Category])
lsscrsCategoryList = Lens.lens (categoryList :: ListSkillsStoreCategoriesResponse -> Lude.Maybe [Category]) (\s a -> s {categoryList = a} :: ListSkillsStoreCategoriesResponse)
{-# DEPRECATED lsscrsCategoryList "Use generic-lens or generic-optics with 'categoryList' instead." #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrsNextToken :: Lens.Lens' ListSkillsStoreCategoriesResponse (Lude.Maybe Lude.Text)
lsscrsNextToken = Lens.lens (nextToken :: ListSkillsStoreCategoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSkillsStoreCategoriesResponse)
{-# DEPRECATED lsscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsscrsResponseStatus :: Lens.Lens' ListSkillsStoreCategoriesResponse Lude.Int
lsscrsResponseStatus = Lens.lens (responseStatus :: ListSkillsStoreCategoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSkillsStoreCategoriesResponse)
{-# DEPRECATED lsscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
