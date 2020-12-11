{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEntityRecognizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the properties of all entity recognizers that you created, including recognizers currently in training. Allows you to filter the list of recognizers based on criteria such as status and submission time. This call returns up to 500 entity recognizers in the list, with a default number of 100 recognizers in the list.
--
-- The results of this list are not in any particular order. Please get the list and sort locally if needed.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntityRecognizers
  ( -- * Creating a request
    ListEntityRecognizers (..),
    mkListEntityRecognizers,

    -- ** Request lenses
    lerNextToken,
    lerFilter,
    lerMaxResults,

    -- * Destructuring the response
    ListEntityRecognizersResponse (..),
    mkListEntityRecognizersResponse,

    -- ** Response lenses
    lerrsNextToken,
    lerrsEntityRecognizerPropertiesList,
    lerrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEntityRecognizers' smart constructor.
data ListEntityRecognizers = ListEntityRecognizers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter :: Lude.Maybe EntityRecognizerFilter,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEntityRecognizers' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return on each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListEntityRecognizers ::
  ListEntityRecognizers
mkListEntityRecognizers =
  ListEntityRecognizers'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerNextToken :: Lens.Lens' ListEntityRecognizers (Lude.Maybe Lude.Text)
lerNextToken = Lens.lens (nextToken :: ListEntityRecognizers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEntityRecognizers)
{-# DEPRECATED lerNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerFilter :: Lens.Lens' ListEntityRecognizers (Lude.Maybe EntityRecognizerFilter)
lerFilter = Lens.lens (filter :: ListEntityRecognizers -> Lude.Maybe EntityRecognizerFilter) (\s a -> s {filter = a} :: ListEntityRecognizers)
{-# DEPRECATED lerFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return on each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerMaxResults :: Lens.Lens' ListEntityRecognizers (Lude.Maybe Lude.Natural)
lerMaxResults = Lens.lens (maxResults :: ListEntityRecognizers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListEntityRecognizers)
{-# DEPRECATED lerMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListEntityRecognizers where
  page rq rs
    | Page.stop (rs Lens.^. lerrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lerrsEntityRecognizerPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lerNextToken Lens..~ rs Lens.^. lerrsNextToken

instance Lude.AWSRequest ListEntityRecognizers where
  type Rs ListEntityRecognizers = ListEntityRecognizersResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListEntityRecognizersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "EntityRecognizerPropertiesList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEntityRecognizers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.ListEntityRecognizers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListEntityRecognizers where
  toJSON ListEntityRecognizers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListEntityRecognizers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEntityRecognizers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListEntityRecognizersResponse' smart constructor.
data ListEntityRecognizersResponse = ListEntityRecognizersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    entityRecognizerPropertiesList ::
      Lude.Maybe
        [EntityRecognizerProperties],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEntityRecognizersResponse' with the minimum fields required to make a request.
--
-- * 'entityRecognizerPropertiesList' - The list of properties of an entity recognizer.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListEntityRecognizersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEntityRecognizersResponse
mkListEntityRecognizersResponse pResponseStatus_ =
  ListEntityRecognizersResponse'
    { nextToken = Lude.Nothing,
      entityRecognizerPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListEntityRecognizersResponse (Lude.Maybe Lude.Text)
lerrsNextToken = Lens.lens (nextToken :: ListEntityRecognizersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListEntityRecognizersResponse)
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of properties of an entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsEntityRecognizerPropertiesList :: Lens.Lens' ListEntityRecognizersResponse (Lude.Maybe [EntityRecognizerProperties])
lerrsEntityRecognizerPropertiesList = Lens.lens (entityRecognizerPropertiesList :: ListEntityRecognizersResponse -> Lude.Maybe [EntityRecognizerProperties]) (\s a -> s {entityRecognizerPropertiesList = a} :: ListEntityRecognizersResponse)
{-# DEPRECATED lerrsEntityRecognizerPropertiesList "Use generic-lens or generic-optics with 'entityRecognizerPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListEntityRecognizersResponse Lude.Int
lerrsResponseStatus = Lens.lens (responseStatus :: ListEntityRecognizersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEntityRecognizersResponse)
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
