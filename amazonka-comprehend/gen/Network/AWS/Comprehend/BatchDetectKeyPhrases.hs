{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.BatchDetectKeyPhrases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects the key noun phrases found in a batch of documents.
module Network.AWS.Comprehend.BatchDetectKeyPhrases
  ( -- * Creating a Request
    BatchDetectKeyPhrases (..),
    newBatchDetectKeyPhrases,

    -- * Request Lenses
    batchDetectKeyPhrases_textList,
    batchDetectKeyPhrases_languageCode,

    -- * Destructuring the Response
    BatchDetectKeyPhrasesResponse (..),
    newBatchDetectKeyPhrasesResponse,

    -- * Response Lenses
    batchDetectKeyPhrasesResponse_httpStatus,
    batchDetectKeyPhrasesResponse_resultList,
    batchDetectKeyPhrasesResponse_errorList,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDetectKeyPhrases' smart constructor.
data BatchDetectKeyPhrases = BatchDetectKeyPhrases'
  { -- | A list containing the text of the input documents. The list can contain
    -- a maximum of 25 documents. Each document must contain fewer that 5,000
    -- bytes of UTF-8 encoded characters.
    textList :: Core.Sensitive [Core.Sensitive Core.Text],
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectKeyPhrases_textList' - A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer that 5,000
-- bytes of UTF-8 encoded characters.
--
-- 'languageCode', 'batchDetectKeyPhrases_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newBatchDetectKeyPhrases ::
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectKeyPhrases
newBatchDetectKeyPhrases pLanguageCode_ =
  BatchDetectKeyPhrases'
    { textList = Core.mempty,
      languageCode = pLanguageCode_
    }

-- | A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer that 5,000
-- bytes of UTF-8 encoded characters.
batchDetectKeyPhrases_textList :: Lens.Lens' BatchDetectKeyPhrases [Core.Text]
batchDetectKeyPhrases_textList = Lens.lens (\BatchDetectKeyPhrases' {textList} -> textList) (\s@BatchDetectKeyPhrases' {} a -> s {textList = a} :: BatchDetectKeyPhrases) Core.. Core._Sensitive Core.. Lens._Coerce

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
batchDetectKeyPhrases_languageCode :: Lens.Lens' BatchDetectKeyPhrases LanguageCode
batchDetectKeyPhrases_languageCode = Lens.lens (\BatchDetectKeyPhrases' {languageCode} -> languageCode) (\s@BatchDetectKeyPhrases' {} a -> s {languageCode = a} :: BatchDetectKeyPhrases)

instance Core.AWSRequest BatchDetectKeyPhrases where
  type
    AWSResponse BatchDetectKeyPhrases =
      BatchDetectKeyPhrasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectKeyPhrasesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "ResultList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ErrorList" Core..!@ Core.mempty)
      )

instance Core.Hashable BatchDetectKeyPhrases

instance Core.NFData BatchDetectKeyPhrases

instance Core.ToHeaders BatchDetectKeyPhrases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.BatchDetectKeyPhrases" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDetectKeyPhrases where
  toJSON BatchDetectKeyPhrases' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TextList" Core..= textList),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath BatchDetectKeyPhrases where
  toPath = Core.const "/"

instance Core.ToQuery BatchDetectKeyPhrases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDetectKeyPhrasesResponse' smart constructor.
data BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectKeyPhrasesItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectKeyPhrasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectKeyPhrasesResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectKeyPhrasesResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectKeyPhrasesResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectKeyPhrasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDetectKeyPhrasesResponse
newBatchDetectKeyPhrasesResponse pHttpStatus_ =
  BatchDetectKeyPhrasesResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Core.mempty,
      errorList = Core.mempty
    }

-- | The response's http status code.
batchDetectKeyPhrasesResponse_httpStatus :: Lens.Lens' BatchDetectKeyPhrasesResponse Core.Int
batchDetectKeyPhrasesResponse_httpStatus = Lens.lens (\BatchDetectKeyPhrasesResponse' {httpStatus} -> httpStatus) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {httpStatus = a} :: BatchDetectKeyPhrasesResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectKeyPhrasesResponse_resultList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchDetectKeyPhrasesItemResult]
batchDetectKeyPhrasesResponse_resultList = Lens.lens (\BatchDetectKeyPhrasesResponse' {resultList} -> resultList) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {resultList = a} :: BatchDetectKeyPhrasesResponse) Core.. Lens._Coerce

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectKeyPhrasesResponse_errorList :: Lens.Lens' BatchDetectKeyPhrasesResponse [BatchItemError]
batchDetectKeyPhrasesResponse_errorList = Lens.lens (\BatchDetectKeyPhrasesResponse' {errorList} -> errorList) (\s@BatchDetectKeyPhrasesResponse' {} a -> s {errorList = a} :: BatchDetectKeyPhrasesResponse) Core.. Lens._Coerce

instance Core.NFData BatchDetectKeyPhrasesResponse
