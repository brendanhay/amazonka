{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListVocabularies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If no criteria are specified, returns the entire list of vocabularies.
module Network.AWS.Transcribe.ListVocabularies
  ( -- * Creating a request
    ListVocabularies (..),
    mkListVocabularies,

    -- ** Request lenses
    lvNameContains,
    lvNextToken,
    lvStateEquals,
    lvMaxResults,

    -- * Destructuring the response
    ListVocabulariesResponse (..),
    mkListVocabulariesResponse,

    -- ** Response lenses
    lvrsVocabularies,
    lvrsStatus,
    lvrsNextToken,
    lvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListVocabularies' smart constructor.
data ListVocabularies = ListVocabularies'
  { -- | When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is not case sensitive, @ListVocabularies@ returns both "vocabularyname" and "VocabularyName" in the response list.
    nameContains :: Lude.Maybe Lude.Text,
    -- | If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
    nextToken :: Lude.Maybe Lude.Text,
    -- | When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
    stateEquals :: Lude.Maybe VocabularyState,
    -- | The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVocabularies' with the minimum fields required to make a request.
--
-- * 'nameContains' - When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is not case sensitive, @ListVocabularies@ returns both "vocabularyname" and "VocabularyName" in the response list.
-- * 'nextToken' - If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
-- * 'stateEquals' - When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
-- * 'maxResults' - The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
mkListVocabularies ::
  ListVocabularies
mkListVocabularies =
  ListVocabularies'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      stateEquals = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is not case sensitive, @ListVocabularies@ returns both "vocabularyname" and "VocabularyName" in the response list.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvNameContains :: Lens.Lens' ListVocabularies (Lude.Maybe Lude.Text)
lvNameContains = Lens.lens (nameContains :: ListVocabularies -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListVocabularies)
{-# DEPRECATED lvNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvNextToken :: Lens.Lens' ListVocabularies (Lude.Maybe Lude.Text)
lvNextToken = Lens.lens (nextToken :: ListVocabularies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVocabularies)
{-# DEPRECATED lvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
--
-- /Note:/ Consider using 'stateEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvStateEquals :: Lens.Lens' ListVocabularies (Lude.Maybe VocabularyState)
lvStateEquals = Lens.lens (stateEquals :: ListVocabularies -> Lude.Maybe VocabularyState) (\s a -> s {stateEquals = a} :: ListVocabularies)
{-# DEPRECATED lvStateEquals "Use generic-lens or generic-optics with 'stateEquals' instead." #-}

-- | The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMaxResults :: Lens.Lens' ListVocabularies (Lude.Maybe Lude.Natural)
lvMaxResults = Lens.lens (maxResults :: ListVocabularies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListVocabularies)
{-# DEPRECATED lvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListVocabularies where
  type Rs ListVocabularies = ListVocabulariesResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVocabulariesResponse'
            Lude.<$> (x Lude..?> "Vocabularies" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVocabularies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListVocabularies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVocabularies where
  toJSON ListVocabularies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StateEquals" Lude..=) Lude.<$> stateEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListVocabularies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVocabularies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListVocabulariesResponse' smart constructor.
data ListVocabulariesResponse = ListVocabulariesResponse'
  { -- | A list of objects that describe the vocabularies that match the search criteria in the request.
    vocabularies :: Lude.Maybe [VocabularyInfo],
    -- | The requested vocabulary state.
    status :: Lude.Maybe VocabularyState,
    -- | The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set in the @MaxResults@ parameter. If there are more jobs in the list than will fit on the page, Amazon Transcribe returns the @NextPage@ token. To return in the next page of jobs, include the token in the next request to the @ListVocabularies@ operation.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVocabulariesResponse' with the minimum fields required to make a request.
--
-- * 'vocabularies' - A list of objects that describe the vocabularies that match the search criteria in the request.
-- * 'status' - The requested vocabulary state.
-- * 'nextToken' - The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set in the @MaxResults@ parameter. If there are more jobs in the list than will fit on the page, Amazon Transcribe returns the @NextPage@ token. To return in the next page of jobs, include the token in the next request to the @ListVocabularies@ operation.
-- * 'responseStatus' - The response status code.
mkListVocabulariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVocabulariesResponse
mkListVocabulariesResponse pResponseStatus_ =
  ListVocabulariesResponse'
    { vocabularies = Lude.Nothing,
      status = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that describe the vocabularies that match the search criteria in the request.
--
-- /Note:/ Consider using 'vocabularies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsVocabularies :: Lens.Lens' ListVocabulariesResponse (Lude.Maybe [VocabularyInfo])
lvrsVocabularies = Lens.lens (vocabularies :: ListVocabulariesResponse -> Lude.Maybe [VocabularyInfo]) (\s a -> s {vocabularies = a} :: ListVocabulariesResponse)
{-# DEPRECATED lvrsVocabularies "Use generic-lens or generic-optics with 'vocabularies' instead." #-}

-- | The requested vocabulary state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsStatus :: Lens.Lens' ListVocabulariesResponse (Lude.Maybe VocabularyState)
lvrsStatus = Lens.lens (status :: ListVocabulariesResponse -> Lude.Maybe VocabularyState) (\s a -> s {status = a} :: ListVocabulariesResponse)
{-# DEPRECATED lvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set in the @MaxResults@ parameter. If there are more jobs in the list than will fit on the page, Amazon Transcribe returns the @NextPage@ token. To return in the next page of jobs, include the token in the next request to the @ListVocabularies@ operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsNextToken :: Lens.Lens' ListVocabulariesResponse (Lude.Maybe Lude.Text)
lvrsNextToken = Lens.lens (nextToken :: ListVocabulariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVocabulariesResponse)
{-# DEPRECATED lvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsResponseStatus :: Lens.Lens' ListVocabulariesResponse Lude.Int
lvrsResponseStatus = Lens.lens (responseStatus :: ListVocabulariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVocabulariesResponse)
{-# DEPRECATED lvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
