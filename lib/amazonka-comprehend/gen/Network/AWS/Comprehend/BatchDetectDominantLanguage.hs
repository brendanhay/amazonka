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
    bddlrsErrorList,
    bddlrsResultList,
    bddlrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDetectDominantLanguage' smart constructor.
newtype BatchDetectDominantLanguage = BatchDetectDominantLanguage'
  { -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
    textList :: [Lude.Sensitive Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectDominantLanguage' with the minimum fields required to make a request.
--
-- * 'textList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
mkBatchDetectDominantLanguage ::
  BatchDetectDominantLanguage
mkBatchDetectDominantLanguage =
  BatchDetectDominantLanguage' {textList = Lude.mempty}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document should contain at least 20 characters and must contain fewer than 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlTextList :: Lens.Lens' BatchDetectDominantLanguage [Lude.Sensitive Lude.Text]
bddlTextList = Lens.lens (textList :: BatchDetectDominantLanguage -> [Lude.Sensitive Lude.Text]) (\s a -> s {textList = a} :: BatchDetectDominantLanguage)
{-# DEPRECATED bddlTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

instance Lude.AWSRequest BatchDetectDominantLanguage where
  type
    Rs BatchDetectDominantLanguage =
      BatchDetectDominantLanguageResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDetectDominantLanguageResponse'
            Lude.<$> (x Lude..?> "ErrorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ResultList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDetectDominantLanguage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.BatchDetectDominantLanguage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDetectDominantLanguage where
  toJSON BatchDetectDominantLanguage' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TextList" Lude..= textList)])

instance Lude.ToPath BatchDetectDominantLanguage where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDetectDominantLanguage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDetectDominantLanguageResponse' smart constructor.
data BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse'
  { -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError],
    -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [BatchDetectDominantLanguageItemResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectDominantLanguageResponse' with the minimum fields required to make a request.
--
-- * 'errorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
-- * 'resultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
-- * 'responseStatus' - The response status code.
mkBatchDetectDominantLanguageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDetectDominantLanguageResponse
mkBatchDetectDominantLanguageResponse pResponseStatus_ =
  BatchDetectDominantLanguageResponse'
    { errorList = Lude.mempty,
      resultList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrsErrorList :: Lens.Lens' BatchDetectDominantLanguageResponse [BatchItemError]
bddlrsErrorList = Lens.lens (errorList :: BatchDetectDominantLanguageResponse -> [BatchItemError]) (\s a -> s {errorList = a} :: BatchDetectDominantLanguageResponse)
{-# DEPRECATED bddlrsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrsResultList :: Lens.Lens' BatchDetectDominantLanguageResponse [BatchDetectDominantLanguageItemResult]
bddlrsResultList = Lens.lens (resultList :: BatchDetectDominantLanguageResponse -> [BatchDetectDominantLanguageItemResult]) (\s a -> s {resultList = a} :: BatchDetectDominantLanguageResponse)
{-# DEPRECATED bddlrsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bddlrsResponseStatus :: Lens.Lens' BatchDetectDominantLanguageResponse Lude.Int
bddlrsResponseStatus = Lens.lens (responseStatus :: BatchDetectDominantLanguageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDetectDominantLanguageResponse)
{-# DEPRECATED bddlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
