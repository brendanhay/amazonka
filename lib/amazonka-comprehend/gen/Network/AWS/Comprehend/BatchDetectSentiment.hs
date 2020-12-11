{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectSentiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects a batch of documents and returns an inference of the prevailing sentiment, @POSITIVE@ , @NEUTRAL@ , @MIXED@ , or @NEGATIVE@ , in each one.
module Network.AWS.Comprehend.BatchDetectSentiment
  ( -- * Creating a request
    BatchDetectSentiment (..),
    mkBatchDetectSentiment,

    -- ** Request lenses
    bdsTextList,
    bdsLanguageCode,

    -- * Destructuring the response
    BatchDetectSentimentResponse (..),
    mkBatchDetectSentimentResponse,

    -- ** Response lenses
    bdsrsResponseStatus,
    bdsrsResultList,
    bdsrsErrorList,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDetectSentiment' smart constructor.
data BatchDetectSentiment = BatchDetectSentiment'
  { textList ::
      [Lude.Sensitive Lude.Text],
    languageCode :: LanguageCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSentiment' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
-- * 'textList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkBatchDetectSentiment ::
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectSentiment
mkBatchDetectSentiment pLanguageCode_ =
  BatchDetectSentiment'
    { textList = Lude.mempty,
      languageCode = pLanguageCode_
    }

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsTextList :: Lens.Lens' BatchDetectSentiment [Lude.Sensitive Lude.Text]
bdsTextList = Lens.lens (textList :: BatchDetectSentiment -> [Lude.Sensitive Lude.Text]) (\s a -> s {textList = a} :: BatchDetectSentiment)
{-# DEPRECATED bdsTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsLanguageCode :: Lens.Lens' BatchDetectSentiment LanguageCode
bdsLanguageCode = Lens.lens (languageCode :: BatchDetectSentiment -> LanguageCode) (\s a -> s {languageCode = a} :: BatchDetectSentiment)
{-# DEPRECATED bdsLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest BatchDetectSentiment where
  type Rs BatchDetectSentiment = BatchDetectSentimentResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDetectSentimentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ResultList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ErrorList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders BatchDetectSentiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.BatchDetectSentiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDetectSentiment where
  toJSON BatchDetectSentiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TextList" Lude..= textList),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath BatchDetectSentiment where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDetectSentiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDetectSentimentResponse' smart constructor.
data BatchDetectSentimentResponse = BatchDetectSentimentResponse'
  { responseStatus ::
      Lude.Int,
    resultList ::
      [BatchDetectSentimentItemResult],
    errorList :: [BatchItemError]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSentimentResponse' with the minimum fields required to make a request.
--
-- * 'errorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
-- * 'responseStatus' - The response status code.
-- * 'resultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
mkBatchDetectSentimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDetectSentimentResponse
mkBatchDetectSentimentResponse pResponseStatus_ =
  BatchDetectSentimentResponse'
    { responseStatus = pResponseStatus_,
      resultList = Lude.mempty,
      errorList = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsrsResponseStatus :: Lens.Lens' BatchDetectSentimentResponse Lude.Int
bdsrsResponseStatus = Lens.lens (responseStatus :: BatchDetectSentimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDetectSentimentResponse)
{-# DEPRECATED bdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsrsResultList :: Lens.Lens' BatchDetectSentimentResponse [BatchDetectSentimentItemResult]
bdsrsResultList = Lens.lens (resultList :: BatchDetectSentimentResponse -> [BatchDetectSentimentItemResult]) (\s a -> s {resultList = a} :: BatchDetectSentimentResponse)
{-# DEPRECATED bdsrsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsrsErrorList :: Lens.Lens' BatchDetectSentimentResponse [BatchItemError]
bdsrsErrorList = Lens.lens (errorList :: BatchDetectSentimentResponse -> [BatchItemError]) (\s a -> s {errorList = a} :: BatchDetectSentimentResponse)
{-# DEPRECATED bdsrsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}
