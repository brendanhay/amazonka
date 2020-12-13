{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListMedicalVocabularies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If you don't enter a value in any of the request parameters, returns the entire list of vocabularies.
module Network.AWS.Transcribe.ListMedicalVocabularies
  ( -- * Creating a request
    ListMedicalVocabularies (..),
    mkListMedicalVocabularies,

    -- ** Request lenses
    lmvNameContains,
    lmvNextToken,
    lmvStateEquals,
    lmvMaxResults,

    -- * Destructuring the response
    ListMedicalVocabulariesResponse (..),
    mkListMedicalVocabulariesResponse,

    -- ** Response lenses
    lmvrsVocabularies,
    lmvrsStatus,
    lmvrsNextToken,
    lmvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkListMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { -- | Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
    nameContains :: Lude.Maybe Lude.Text,
    -- | If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
    nextToken :: Lude.Maybe Lude.Text,
    -- | When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
    stateEquals :: Lude.Maybe VocabularyState,
    -- | The maximum number of vocabularies to return in the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMedicalVocabularies' with the minimum fields required to make a request.
--
-- * 'nameContains' - Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
-- * 'nextToken' - If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
-- * 'stateEquals' - When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
-- * 'maxResults' - The maximum number of vocabularies to return in the response.
mkListMedicalVocabularies ::
  ListMedicalVocabularies
mkListMedicalVocabularies =
  ListMedicalVocabularies'
    { nameContains = Lude.Nothing,
      nextToken = Lude.Nothing,
      stateEquals = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvNameContains :: Lens.Lens' ListMedicalVocabularies (Lude.Maybe Lude.Text)
lmvNameContains = Lens.lens (nameContains :: ListMedicalVocabularies -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListMedicalVocabularies)
{-# DEPRECATED lmvNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvNextToken :: Lens.Lens' ListMedicalVocabularies (Lude.Maybe Lude.Text)
lmvNextToken = Lens.lens (nextToken :: ListMedicalVocabularies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMedicalVocabularies)
{-# DEPRECATED lmvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
--
-- /Note:/ Consider using 'stateEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvStateEquals :: Lens.Lens' ListMedicalVocabularies (Lude.Maybe VocabularyState)
lmvStateEquals = Lens.lens (stateEquals :: ListMedicalVocabularies -> Lude.Maybe VocabularyState) (\s a -> s {stateEquals = a} :: ListMedicalVocabularies)
{-# DEPRECATED lmvStateEquals "Use generic-lens or generic-optics with 'stateEquals' instead." #-}

-- | The maximum number of vocabularies to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvMaxResults :: Lens.Lens' ListMedicalVocabularies (Lude.Maybe Lude.Natural)
lmvMaxResults = Lens.lens (maxResults :: ListMedicalVocabularies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMedicalVocabularies)
{-# DEPRECATED lmvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListMedicalVocabularies where
  type Rs ListMedicalVocabularies = ListMedicalVocabulariesResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            Lude.<$> (x Lude..?> "Vocabularies" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMedicalVocabularies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.ListMedicalVocabularies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StateEquals" Lude..=) Lude.<$> stateEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListMedicalVocabularies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMedicalVocabularies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { -- | A list of objects that describe the vocabularies that match your search criteria.
    vocabularies :: Lude.Maybe [VocabularyInfo],
    -- | The requested vocabulary state.
    status :: Lude.Maybe VocabularyState,
    -- | The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMedicalVocabulariesResponse' with the minimum fields required to make a request.
--
-- * 'vocabularies' - A list of objects that describe the vocabularies that match your search criteria.
-- * 'status' - The requested vocabulary state.
-- * 'nextToken' - The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
-- * 'responseStatus' - The response status code.
mkListMedicalVocabulariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMedicalVocabulariesResponse
mkListMedicalVocabulariesResponse pResponseStatus_ =
  ListMedicalVocabulariesResponse'
    { vocabularies = Lude.Nothing,
      status = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that describe the vocabularies that match your search criteria.
--
-- /Note:/ Consider using 'vocabularies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrsVocabularies :: Lens.Lens' ListMedicalVocabulariesResponse (Lude.Maybe [VocabularyInfo])
lmvrsVocabularies = Lens.lens (vocabularies :: ListMedicalVocabulariesResponse -> Lude.Maybe [VocabularyInfo]) (\s a -> s {vocabularies = a} :: ListMedicalVocabulariesResponse)
{-# DEPRECATED lmvrsVocabularies "Use generic-lens or generic-optics with 'vocabularies' instead." #-}

-- | The requested vocabulary state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrsStatus :: Lens.Lens' ListMedicalVocabulariesResponse (Lude.Maybe VocabularyState)
lmvrsStatus = Lens.lens (status :: ListMedicalVocabulariesResponse -> Lude.Maybe VocabularyState) (\s a -> s {status = a} :: ListMedicalVocabulariesResponse)
{-# DEPRECATED lmvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrsNextToken :: Lens.Lens' ListMedicalVocabulariesResponse (Lude.Maybe Lude.Text)
lmvrsNextToken = Lens.lens (nextToken :: ListMedicalVocabulariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMedicalVocabulariesResponse)
{-# DEPRECATED lmvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrsResponseStatus :: Lens.Lens' ListMedicalVocabulariesResponse Lude.Int
lmvrsResponseStatus = Lens.lens (responseStatus :: ListMedicalVocabulariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMedicalVocabulariesResponse)
{-# DEPRECATED lmvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
