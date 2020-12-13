{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchDetectKeyPhrases (..),
    mkBatchDetectKeyPhrases,

    -- ** Request lenses
    bdkpLanguageCode,
    bdkpTextList,

    -- * Destructuring the response
    BatchDetectKeyPhrasesResponse (..),
    mkBatchDetectKeyPhrasesResponse,

    -- ** Response lenses
    bdkprsErrorList,
    bdkprsResultList,
    bdkprsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDetectKeyPhrases' smart constructor.
data BatchDetectKeyPhrases = BatchDetectKeyPhrases'
  { -- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
    languageCode :: LanguageCode,
    -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    textList :: [Lude.Sensitive Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectKeyPhrases' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
-- * 'textList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkBatchDetectKeyPhrases ::
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectKeyPhrases
mkBatchDetectKeyPhrases pLanguageCode_ =
  BatchDetectKeyPhrases'
    { languageCode = pLanguageCode_,
      textList = Lude.mempty
    }

-- | The language of the input documents. You can specify any of the primary languages supported by Amazon Comprehend. All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpLanguageCode :: Lens.Lens' BatchDetectKeyPhrases LanguageCode
bdkpLanguageCode = Lens.lens (languageCode :: BatchDetectKeyPhrases -> LanguageCode) (\s a -> s {languageCode = a} :: BatchDetectKeyPhrases)
{-# DEPRECATED bdkpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkpTextList :: Lens.Lens' BatchDetectKeyPhrases [Lude.Sensitive Lude.Text]
bdkpTextList = Lens.lens (textList :: BatchDetectKeyPhrases -> [Lude.Sensitive Lude.Text]) (\s a -> s {textList = a} :: BatchDetectKeyPhrases)
{-# DEPRECATED bdkpTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

instance Lude.AWSRequest BatchDetectKeyPhrases where
  type Rs BatchDetectKeyPhrases = BatchDetectKeyPhrasesResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDetectKeyPhrasesResponse'
            Lude.<$> (x Lude..?> "ErrorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ResultList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDetectKeyPhrases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.BatchDetectKeyPhrases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDetectKeyPhrases where
  toJSON BatchDetectKeyPhrases' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("TextList" Lude..= textList)
          ]
      )

instance Lude.ToPath BatchDetectKeyPhrases where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDetectKeyPhrases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDetectKeyPhrasesResponse' smart constructor.
data BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse'
  { -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError],
    -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [BatchDetectKeyPhrasesItemResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectKeyPhrasesResponse' with the minimum fields required to make a request.
--
-- * 'errorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
-- * 'resultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
-- * 'responseStatus' - The response status code.
mkBatchDetectKeyPhrasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDetectKeyPhrasesResponse
mkBatchDetectKeyPhrasesResponse pResponseStatus_ =
  BatchDetectKeyPhrasesResponse'
    { errorList = Lude.mempty,
      resultList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprsErrorList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchItemError]
bdkprsErrorList = Lens.lens (errorList :: BatchDetectKeyPhrasesResponse -> [BatchItemError]) (\s a -> s {errorList = a} :: BatchDetectKeyPhrasesResponse)
{-# DEPRECATED bdkprsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprsResultList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchDetectKeyPhrasesItemResult]
bdkprsResultList = Lens.lens (resultList :: BatchDetectKeyPhrasesResponse -> [BatchDetectKeyPhrasesItemResult]) (\s a -> s {resultList = a} :: BatchDetectKeyPhrasesResponse)
{-# DEPRECATED bdkprsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdkprsResponseStatus :: Lens.Lens' BatchDetectKeyPhrasesResponse Lude.Int
bdkprsResponseStatus = Lens.lens (responseStatus :: BatchDetectKeyPhrasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDetectKeyPhrasesResponse)
{-# DEPRECATED bdkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
