{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectSyntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for the syntax and part of speech of the words in the document and returns information about them. For more information, see 'how-syntax' .
module Network.AWS.Comprehend.BatchDetectSyntax
  ( -- * Creating a request
    BatchDetectSyntax (..),
    mkBatchDetectSyntax,

    -- ** Request lenses
    bLanguageCode,
    bTextList,

    -- * Destructuring the response
    BatchDetectSyntaxResponse (..),
    mkBatchDetectSyntaxResponse,

    -- ** Response lenses
    brsErrorList,
    brsResultList,
    brsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDetectSyntax' smart constructor.
data BatchDetectSyntax = BatchDetectSyntax'
  { -- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
    languageCode :: SyntaxLanguageCode,
    -- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
    textList :: [Lude.Sensitive Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSyntax' with the minimum fields required to make a request.
--
-- * 'languageCode' - The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
-- * 'textList' - A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
mkBatchDetectSyntax ::
  -- | 'languageCode'
  SyntaxLanguageCode ->
  BatchDetectSyntax
mkBatchDetectSyntax pLanguageCode_ =
  BatchDetectSyntax'
    { languageCode = pLanguageCode_,
      textList = Lude.mempty
    }

-- | The language of the input documents. You can specify any of the following languages supported by Amazon Comprehend: German ("de"), English ("en"), Spanish ("es"), French ("fr"), Italian ("it"), or Portuguese ("pt"). All documents must be in the same language.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLanguageCode :: Lens.Lens' BatchDetectSyntax SyntaxLanguageCode
bLanguageCode = Lens.lens (languageCode :: BatchDetectSyntax -> SyntaxLanguageCode) (\s a -> s {languageCode = a} :: BatchDetectSyntax)
{-# DEPRECATED bLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A list containing the text of the input documents. The list can contain a maximum of 25 documents. Each document must contain fewer that 5,000 bytes of UTF-8 encoded characters.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTextList :: Lens.Lens' BatchDetectSyntax [Lude.Sensitive Lude.Text]
bTextList = Lens.lens (textList :: BatchDetectSyntax -> [Lude.Sensitive Lude.Text]) (\s a -> s {textList = a} :: BatchDetectSyntax)
{-# DEPRECATED bTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

instance Lude.AWSRequest BatchDetectSyntax where
  type Rs BatchDetectSyntax = BatchDetectSyntaxResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDetectSyntaxResponse'
            Lude.<$> (x Lude..?> "ErrorList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ResultList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDetectSyntax where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.BatchDetectSyntax" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDetectSyntax where
  toJSON BatchDetectSyntax' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("TextList" Lude..= textList)
          ]
      )

instance Lude.ToPath BatchDetectSyntax where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDetectSyntax where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDetectSyntaxResponse' smart constructor.
data BatchDetectSyntaxResponse = BatchDetectSyntaxResponse'
  { -- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError],
    -- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
    resultList :: [BatchDetectSyntaxItemResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSyntaxResponse' with the minimum fields required to make a request.
--
-- * 'errorList' - A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
-- * 'resultList' - A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
-- * 'responseStatus' - The response status code.
mkBatchDetectSyntaxResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDetectSyntaxResponse
mkBatchDetectSyntaxResponse pResponseStatus_ =
  BatchDetectSyntaxResponse'
    { errorList = Lude.mempty,
      resultList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list containing one object for each document that contained an error. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If there are no errors in the batch, the @ErrorList@ is empty.
--
-- /Note:/ Consider using 'errorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsErrorList :: Lens.Lens' BatchDetectSyntaxResponse [BatchItemError]
brsErrorList = Lens.lens (errorList :: BatchDetectSyntaxResponse -> [BatchItemError]) (\s a -> s {errorList = a} :: BatchDetectSyntaxResponse)
{-# DEPRECATED brsErrorList "Use generic-lens or generic-optics with 'errorList' instead." #-}

-- | A list of objects containing the results of the operation. The results are sorted in ascending order by the @Index@ field and match the order of the documents in the input list. If all of the documents contain an error, the @ResultList@ is empty.
--
-- /Note:/ Consider using 'resultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsResultList :: Lens.Lens' BatchDetectSyntaxResponse [BatchDetectSyntaxItemResult]
brsResultList = Lens.lens (resultList :: BatchDetectSyntaxResponse -> [BatchDetectSyntaxItemResult]) (\s a -> s {resultList = a} :: BatchDetectSyntaxResponse)
{-# DEPRECATED brsResultList "Use generic-lens or generic-optics with 'resultList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brsResponseStatus :: Lens.Lens' BatchDetectSyntaxResponse Lude.Int
brsResponseStatus = Lens.lens (responseStatus :: BatchDetectSyntaxResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDetectSyntaxResponse)
{-# DEPRECATED brsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
