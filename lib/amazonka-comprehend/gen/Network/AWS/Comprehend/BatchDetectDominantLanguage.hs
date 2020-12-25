{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectDominantLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text for a batch of documents. For a list of languages that Amazon Comprehend can detect, see <https://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages> .
module Network.AWS.Comprehend.BatchDetectDominantLanguage
  ( -- * Creating a request
    BatchDetectDominantLanguage (..),
    mkBatchDetectDominantLanguage,

    -- ** Request lenses
    bddlTextList,

    -- * Destructuring the response
    BatchDetectDominantLanguageResponse (..),
    mkBatchDetectDominantLanguageResponse,

    -- ** Response lenses
    bddlrrsResultList,
    bddlrrsErrorList,
    bddlrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDetectDominantLanguage' smart constructor.
newtype BatchDetectDominantLanguage = BatchDetectDominantLanguage'
  { -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
    textList :: [Types.CustomerInputString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectDominantLanguage' value with any optional fields omitted.
mkBatchDetectDominantLanguage ::
  BatchDetectDominantLanguage
mkBatchDetectDominantLanguage =
  BatchDetectDominantLanguage' {textList = Core.mempty}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlTextList :: Lens.Lens' BatchDetectDominantLanguage [Types.CustomerInputString]
bddlTextList = Lens.field @"textList"
{-# DEPRECATED bddlTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

instance Core.FromJSON BatchDetectDominantLanguage where
  toJSON BatchDetectDominantLanguage {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TextList" Core..= textList)])

instance Core.AWSRequest BatchDetectDominantLanguage where
  type
    Rs BatchDetectDominantLanguage =
      BatchDetectDominantLanguageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.BatchDetectDominantLanguage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectDominantLanguageResponse'
            Core.<$> (x Core..:? "ResultList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ErrorList" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDetectDominantLanguageResponse' smart constructor.
data BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse'
  { -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [Types.BatchDetectDominantLanguageItemResult],
    -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [Types.BatchItemError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectDominantLanguageResponse' value with any optional fields omitted.
mkBatchDetectDominantLanguageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDetectDominantLanguageResponse
mkBatchDetectDominantLanguageResponse responseStatus =
  BatchDetectDominantLanguageResponse'
    { resultList = Core.mempty,
      errorList = Core.mempty,
      responseStatus
    }

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrrsResultList :: Lens.Lens' BatchDetectDominantLanguageResponse [Types.BatchDetectDominantLanguageItemResult]
bddlrrsResultList = Lens.field @"resultList"
{-# DEPRECATED bddlrrsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrrsErrorList :: Lens.Lens' BatchDetectDominantLanguageResponse [Types.BatchItemError]
bddlrrsErrorList = Lens.field @"errorList"
{-# DEPRECATED bddlrrsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrrsResponseStatus :: Lens.Lens' BatchDetectDominantLanguageResponse Core.Int
bddlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bddlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
