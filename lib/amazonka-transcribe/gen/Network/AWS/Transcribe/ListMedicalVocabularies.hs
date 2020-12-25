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
    lmvMaxResults,
    lmvNameContains,
    lmvNextToken,
    lmvStateEquals,

    -- * Destructuring the response
    ListMedicalVocabulariesResponse (..),
    mkListMedicalVocabulariesResponse,

    -- ** Response lenses
    lmvrrsNextToken,
    lmvrrsStatus,
    lmvrrsVocabularies,
    lmvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkListMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { -- | The maximum number of vocabularies to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
    nameContains :: Core.Maybe Types.VocabularyName,
    -- | If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
    nextToken :: Core.Maybe Types.NextToken,
    -- | When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
    stateEquals :: Core.Maybe Types.VocabularyState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMedicalVocabularies' value with any optional fields omitted.
mkListMedicalVocabularies ::
  ListMedicalVocabularies
mkListMedicalVocabularies =
  ListMedicalVocabularies'
    { maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      stateEquals = Core.Nothing
    }

-- | The maximum number of vocabularies to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvMaxResults :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Core.Natural)
lmvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Returns vocabularies whose names contain the specified string. The search is not case sensitive. @ListMedicalVocabularies@ returns both "@vocabularyname@ " and "@VocabularyName@ ".
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvNameContains :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Types.VocabularyName)
lmvNameContains = Lens.field @"nameContains"
{-# DEPRECATED lmvNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of your previous request to @ListMedicalVocabularies@ was truncated, include the @NextToken@ to fetch the next set of vocabularies.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvNextToken :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Types.NextToken)
lmvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When specified, returns only vocabularies with the @VocabularyState@ equal to the specified vocabulary state. Use this field to see which vocabularies are ready for your medical transcription jobs.
--
-- /Note:/ Consider using 'stateEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvStateEquals :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Types.VocabularyState)
lmvStateEquals = Lens.field @"stateEquals"
{-# DEPRECATED lmvStateEquals "Use generic-lens or generic-optics with 'stateEquals' instead." #-}

instance Core.FromJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StateEquals" Core..=) Core.<$> stateEquals
          ]
      )

instance Core.AWSRequest ListMedicalVocabularies where
  type Rs ListMedicalVocabularies = ListMedicalVocabulariesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.ListMedicalVocabularies")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "Vocabularies")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { -- | The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The requested vocabulary state.
    status :: Core.Maybe Types.VocabularyState,
    -- | A list of objects that describe the vocabularies that match your search criteria.
    vocabularies :: Core.Maybe [Types.VocabularyInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListMedicalVocabulariesResponse' value with any optional fields omitted.
mkListMedicalVocabulariesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMedicalVocabulariesResponse
mkListMedicalVocabulariesResponse responseStatus =
  ListMedicalVocabulariesResponse'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      vocabularies = Core.Nothing,
      responseStatus
    }

-- | The @ListMedicalVocabularies@ operation returns a page of vocabularies at a time. You set the maximum number of vocabularies to return on a page with the @MaxResults@ parameter. If there are more jobs in the list will fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. To return the next page of vocabularies, include the token in the next request to the @ListMedicalVocabularies@ operation .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrrsNextToken :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe Types.NextToken)
lmvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The requested vocabulary state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrrsStatus :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe Types.VocabularyState)
lmvrrsStatus = Lens.field @"status"
{-# DEPRECATED lmvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of objects that describe the vocabularies that match your search criteria.
--
-- /Note:/ Consider using 'vocabularies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrrsVocabularies :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe [Types.VocabularyInfo])
lmvrrsVocabularies = Lens.field @"vocabularies"
{-# DEPRECATED lmvrrsVocabularies "Use generic-lens or generic-optics with 'vocabularies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmvrrsResponseStatus :: Lens.Lens' ListMedicalVocabulariesResponse Core.Int
lmvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
