{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectKeyPhrases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in a batch of documents.
module Network.AWS.Comprehend.BatchDetectKeyPhrases
    (
    -- * Creating a request
      BatchDetectKeyPhrases (..)
    , mkBatchDetectKeyPhrases
    -- ** Request lenses
    , bdkpTextList
    , bdkpLanguageCode

    -- * Destructuring the response
    , BatchDetectKeyPhrasesResponse (..)
    , mkBatchDetectKeyPhrasesResponse
    -- ** Response lenses
    , bdkprrsResultList
    , bdkprrsErrorList
    , bdkprrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchDetectKeyPhrases' smart constructor.
data BatchDetectKeyPhrases = BatchDetectKeyPhrases'
  { textList :: [Types.CustomerInputString]
    -- ^ A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
  , languageCode :: Types.LanguageCode
    -- ^ The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectKeyPhrases' value with any optional fields omitted.
mkBatchDetectKeyPhrases
    :: Types.LanguageCode -- ^ 'languageCode'
    -> BatchDetectKeyPhrases
mkBatchDetectKeyPhrases languageCode
  = BatchDetectKeyPhrases'{textList = Core.mempty, languageCode}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpTextList :: Lens.Lens' BatchDetectKeyPhrases [Types.CustomerInputString]
bdkpTextList = Lens.field @"textList"
{-# INLINEABLE bdkpTextList #-}
{-# DEPRECATED textList "Use generic-lens or generic-optics with 'textList' instead"  #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpLanguageCode :: Lens.Lens' BatchDetectKeyPhrases Types.LanguageCode
bdkpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE bdkpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

instance Core.ToQuery BatchDetectKeyPhrases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchDetectKeyPhrases where
        toHeaders BatchDetectKeyPhrases{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.BatchDetectKeyPhrases")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchDetectKeyPhrases where
        toJSON BatchDetectKeyPhrases{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TextList" Core..= textList),
                  Core.Just ("LanguageCode" Core..= languageCode)])

instance Core.AWSRequest BatchDetectKeyPhrases where
        type Rs BatchDetectKeyPhrases = BatchDetectKeyPhrasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDetectKeyPhrasesResponse' Core.<$>
                   (x Core..:? "ResultList" Core..!= Core.mempty) Core.<*>
                     x Core..:? "ErrorList" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDetectKeyPhrasesResponse' smart constructor.
data BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse'
  { resultList :: [Types.BatchDetectKeyPhrasesItemResult]
    -- ^ A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
  , errorList :: [Types.BatchItemError]
    -- ^ A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectKeyPhrasesResponse' value with any optional fields omitted.
mkBatchDetectKeyPhrasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDetectKeyPhrasesResponse
mkBatchDetectKeyPhrasesResponse responseStatus
  = BatchDetectKeyPhrasesResponse'{resultList = Core.mempty,
                                   errorList = Core.mempty, responseStatus}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprrsResultList :: Lens.Lens' BatchDetectKeyPhrasesResponse [Types.BatchDetectKeyPhrasesItemResult]
bdkprrsResultList = Lens.field @"resultList"
{-# INLINEABLE bdkprrsResultList #-}
{-# DEPRECATED resultList "Use generic-lens or generic-optics with 'resultList' instead"  #-}

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprrsErrorList :: Lens.Lens' BatchDetectKeyPhrasesResponse [Types.BatchItemError]
bdkprrsErrorList = Lens.field @"errorList"
{-# INLINEABLE bdkprrsErrorList #-}
{-# DEPRECATED errorList "Use generic-lens or generic-optics with 'errorList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprrsResponseStatus :: Lens.Lens' BatchDetectKeyPhrasesResponse Core.Int
bdkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
