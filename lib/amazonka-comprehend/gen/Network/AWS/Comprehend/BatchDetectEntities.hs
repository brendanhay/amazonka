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
    bdeLanguageCode,
    bdeTextList,

    -- * Destructuring the response
    BatchDetectEntitiesResponse (..),
    mkBatchDetectEntitiesResponse,

    -- ** Response lenses
    bdersErrorList,
    bdersResultList,
    bdersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDetectEntities' smart constructor.
data BatchDetectEntities = BatchDetectEntities'
  { -- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
    languageCode :: LanguageCode,
    -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer than 5,000 bytes of UTF-8 encoded characters.
    textList :: [Lude.Sensitive Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectEntities' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
-- * 'textList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer than 5,000 bytes of UTF-8 encoded characters.
mkBatchDetectEntities ::
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectEntities
mkBatchDetectEntities pLanguageCode_ =
  BatchDetectEntities'
    { languageCode = pLanguageCode_,
      textList = Lude.mempty
    }

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeLanguageCode :: Lens.Lens' BatchDetectEntities LanguageCode
bdeLanguageCode = Lens.lens (languageCode :: BatchDetectEntities -> LanguageCode) (\s a -> s {languageCode = a} :: BatchDetectEntities)
{-# DEPRECATED bdeLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer than 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdeTextList :: Lens.Lens' BatchDetectEntities [Lude.Sensitive Lude.Text]
bdeTextList = Lens.lens (textList :: BatchDetectEntities -> [Lude.Sensitive Lude.Text]) (\s a -> s {textList = a} :: BatchDetectEntities)
{-# DEPRECATED bdeTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

instance Lude.AWSRequest BatchDetectEntities where
  type Rs BatchDetectEntities = BatchDetectEntitiesResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDetectEntitiesResponse'
            Lude.<$> (x Lude..?> "ErrorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ResultList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDetectEntities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.BatchDetectEntities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDetectEntities where
  toJSON BatchDetectEntities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("TextList" Lude..= textList)
          ]
      )

instance Lude.ToPath BatchDetectEntities where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDetectEntities where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDetectEntitiesResponse' smart constructor.
data BatchDetectEntitiesResponse = BatchDetectEntitiesResponse'
  { -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError],
    -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [BatchDetectEntitiesItemResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectEntitiesResponse' with the minimum fields required to make a request.
--
-- * 'errorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
-- * 'resultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
-- * 'responseStatus' - The response status code.
mkBatchDetectEntitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDetectEntitiesResponse
mkBatchDetectEntitiesResponse pResponseStatus_ =
  BatchDetectEntitiesResponse'
    { errorList = Lude.mempty,
      resultList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdersErrorList :: Lens.Lens' BatchDetectEntitiesResponse [BatchItemError]
bdersErrorList = Lens.lens (errorList :: BatchDetectEntitiesResponse -> [BatchItemError]) (\s a -> s {errorList = a} :: BatchDetectEntitiesResponse)
{-# DEPRECATED bdersErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdersResultList :: Lens.Lens' BatchDetectEntitiesResponse [BatchDetectEntitiesItemResult]
bdersResultList = Lens.lens (resultList :: BatchDetectEntitiesResponse -> [BatchDetectEntitiesItemResult]) (\s a -> s {resultList = a} :: BatchDetectEntitiesResponse)
{-# DEPRECATED bdersResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdersResponseStatus :: Lens.Lens' BatchDetectEntitiesResponse Lude.Int
bdersResponseStatus = Lens.lens (responseStatus :: BatchDetectEntitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDetectEntitiesResponse)
{-# DEPRECATED bdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
