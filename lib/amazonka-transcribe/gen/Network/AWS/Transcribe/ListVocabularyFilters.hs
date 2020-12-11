{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListVocabularyFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about vocabulary filters.
module Network.AWS.Transcribe.ListVocabularyFilters
  ( -- * Creating a request
    ListVocabularyFilters (..),
    mkListVocabularyFilters,

    -- ** Request lenses
    lvfNameContains,
    lvfNextToken,
    lvfMaxResults,

    -- * Destructuring the response
    ListVocabularyFiltersResponse (..),
    mkListVocabularyFiltersResponse,

    -- ** Response lenses
    lvfrsNextToken,
    lvfrsVocabularyFilters,
    lvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListVocabularyFilters' smart constructor.
data ListVocabularyFilters = ListVocabularyFilters'
  { nameContains ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListVocabularyFilters' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of filters to return in the response. If there are fewer results in the list, this response contains only the actual results.
-- * 'nameContains' - Filters the response so that it only contains vocabulary filters whose name contains the specified string.
-- * 'nextToken' - If the result of the previous request to @ListVocabularyFilters@ was truncated, include the @NextToken@ to fetch the next set of collections.
mkListVocabularyFilters ::
  ListVocabularyFilters
mkListVocabularyFilters =
  ListVocabularyFilters'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters the response so that it only contains vocabulary filters whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfNameContains :: Lens.Lens' ListVocabularyFilters (Lude.Maybe Lude.Text)
lvfNameContains = Lens.lens (nameContains :: ListVocabularyFilters -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListVocabularyFilters)
{-# DEPRECATED lvfNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous request to @ListVocabularyFilters@ was truncated, include the @NextToken@ to fetch the next set of collections.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfNextToken :: Lens.Lens' ListVocabularyFilters (Lude.Maybe Lude.Text)
lvfNextToken = Lens.lens (nextToken :: ListVocabularyFilters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVocabularyFilters)
{-# DEPRECATED lvfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of filters to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfMaxResults :: Lens.Lens' ListVocabularyFilters (Lude.Maybe Lude.Natural)
lvfMaxResults = Lens.lens (maxResults :: ListVocabularyFilters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListVocabularyFilters)
{-# DEPRECATED lvfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListVocabularyFilters where
  type Rs ListVocabularyFilters = ListVocabularyFiltersResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVocabularyFiltersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "VocabularyFilters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVocabularyFilters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListVocabularyFilters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVocabularyFilters where
  toJSON ListVocabularyFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListVocabularyFilters where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVocabularyFilters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListVocabularyFiltersResponse' smart constructor.
data ListVocabularyFiltersResponse = ListVocabularyFiltersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    vocabularyFilters ::
      Lude.Maybe
        [VocabularyFilterInfo],
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

-- | Creates a value of 'ListVocabularyFiltersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @ListVocabularyFilters@ operation returns a page of collections at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularyFilters@ operation to return in the next page of jobs.
-- * 'responseStatus' - The response status code.
-- * 'vocabularyFilters' - The list of vocabulary filters. It contains at most @MaxResults@ number of filters. If there are more filters, call the @ListVocabularyFilters@ operation again with the @NextToken@ parameter in the request set to the value of the @NextToken@ field in the response.
mkListVocabularyFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVocabularyFiltersResponse
mkListVocabularyFiltersResponse pResponseStatus_ =
  ListVocabularyFiltersResponse'
    { nextToken = Lude.Nothing,
      vocabularyFilters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ListVocabularyFilters@ operation returns a page of collections at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularyFilters@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrsNextToken :: Lens.Lens' ListVocabularyFiltersResponse (Lude.Maybe Lude.Text)
lvfrsNextToken = Lens.lens (nextToken :: ListVocabularyFiltersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVocabularyFiltersResponse)
{-# DEPRECATED lvfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of vocabulary filters. It contains at most @MaxResults@ number of filters. If there are more filters, call the @ListVocabularyFilters@ operation again with the @NextToken@ parameter in the request set to the value of the @NextToken@ field in the response.
--
-- /Note:/ Consider using 'vocabularyFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrsVocabularyFilters :: Lens.Lens' ListVocabularyFiltersResponse (Lude.Maybe [VocabularyFilterInfo])
lvfrsVocabularyFilters = Lens.lens (vocabularyFilters :: ListVocabularyFiltersResponse -> Lude.Maybe [VocabularyFilterInfo]) (\s a -> s {vocabularyFilters = a} :: ListVocabularyFiltersResponse)
{-# DEPRECATED lvfrsVocabularyFilters "Use generic-lens or generic-optics with 'vocabularyFilters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrsResponseStatus :: Lens.Lens' ListVocabularyFiltersResponse Lude.Int
lvfrsResponseStatus = Lens.lens (responseStatus :: ListVocabularyFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVocabularyFiltersResponse)
{-# DEPRECATED lvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
