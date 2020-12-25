{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for named entities and returns information about them. For more information about named entities, see 'how-entities'
module Network.AWS.Comprehend.BatchDetectEntities
  ( -- * Creating a request
    BatchDetectEntities (..),
    mkBatchDetectEntities,

    -- ** Request lenses
    bdeTextList,
    bdeLanguageCode,

    -- * Destructuring the response
    BatchDetectEntitiesResponse (..),
    mkBatchDetectEntitiesResponse,

    -- ** Response lenses
    bderrsResultList,
    bderrsErrorList,
    bderrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDetectEntities' smart constructor.
data BatchDetectEntities = BatchDetectEntities'
  { -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer than 5,000 bytes of UTF-8 encoded characters.
    textList :: [Types.CustomerInputString],
    -- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
    languageCode :: Types.LanguageCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectEntities' value with any optional fields omitted.
mkBatchDetectEntities ::
  -- | 'languageCode'
  Types.LanguageCode ->
  BatchDetectEntities
mkBatchDetectEntities languageCode =
  BatchDetectEntities' {textList = Core.mempty, languageCode}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer than 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeTextList :: Lens.Lens' BatchDetectEntities [Types.CustomerInputString]
bdeTextList = Lens.field @"textList"
{-# DEPRECATED bdeTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeLanguageCode :: Lens.Lens' BatchDetectEntities Types.LanguageCode
bdeLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED bdeLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Core.FromJSON BatchDetectEntities where
  toJSON BatchDetectEntities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TextList" Core..= textList),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.AWSRequest BatchDetectEntities where
  type Rs BatchDetectEntities = BatchDetectEntitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.BatchDetectEntities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectEntitiesResponse'
            Core.<$> (x Core..:? "ResultList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ErrorList" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchDetectEntitiesResponse' smart constructor.
data BatchDetectEntitiesResponse = BatchDetectEntitiesResponse'
  { -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [Types.BatchDetectEntitiesItemResult],
    -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [Types.BatchItemError],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectEntitiesResponse' value with any optional fields omitted.
mkBatchDetectEntitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchDetectEntitiesResponse
mkBatchDetectEntitiesResponse responseStatus =
  BatchDetectEntitiesResponse'
    { resultList = Core.mempty,
      errorList = Core.mempty,
      responseStatus
    }

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bderrsResultList :: Lens.Lens' BatchDetectEntitiesResponse [Types.BatchDetectEntitiesItemResult]
bderrsResultList = Lens.field @"resultList"
{-# DEPRECATED bderrsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bderrsErrorList :: Lens.Lens' BatchDetectEntitiesResponse [Types.BatchItemError]
bderrsErrorList = Lens.field @"errorList"
{-# DEPRECATED bderrsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bderrsResponseStatus :: Lens.Lens' BatchDetectEntitiesResponse Core.Int
bderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
