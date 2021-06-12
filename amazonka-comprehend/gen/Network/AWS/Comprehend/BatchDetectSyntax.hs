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
-- Module      : Network.AWS.Comprehend.BatchDetectSyntax
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for the syntax and part of
-- speech of the words in the document and returns information about them.
-- For more information, see how-syntax.
module Network.AWS.Comprehend.BatchDetectSyntax
  ( -- * Creating a Request
    BatchDetectSyntax (..),
    newBatchDetectSyntax,

    -- * Request Lenses
    batchDetectSyntax_textList,
    batchDetectSyntax_languageCode,

    -- * Destructuring the Response
    BatchDetectSyntaxResponse (..),
    newBatchDetectSyntaxResponse,

    -- * Response Lenses
    batchDetectSyntaxResponse_httpStatus,
    batchDetectSyntaxResponse_resultList,
    batchDetectSyntaxResponse_errorList,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDetectSyntax' smart constructor.
data BatchDetectSyntax = BatchDetectSyntax'
  { -- | A list containing the text of the input documents. The list can contain
    -- a maximum of 25 documents. Each document must contain fewer that 5,000
    -- bytes of UTF-8 encoded characters.
    textList :: Core.Sensitive [Core.Sensitive Core.Text],
    -- | The language of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\"). All documents must be in the same language.
    languageCode :: SyntaxLanguageCode
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectSyntax' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectSyntax_textList' - A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer that 5,000
-- bytes of UTF-8 encoded characters.
--
-- 'languageCode', 'batchDetectSyntax_languageCode' - The language of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\"). All documents must be in the same language.
newBatchDetectSyntax ::
  -- | 'languageCode'
  SyntaxLanguageCode ->
  BatchDetectSyntax
newBatchDetectSyntax pLanguageCode_ =
  BatchDetectSyntax'
    { textList = Core.mempty,
      languageCode = pLanguageCode_
    }

-- | A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document must contain fewer that 5,000
-- bytes of UTF-8 encoded characters.
batchDetectSyntax_textList :: Lens.Lens' BatchDetectSyntax [Core.Text]
batchDetectSyntax_textList = Lens.lens (\BatchDetectSyntax' {textList} -> textList) (\s@BatchDetectSyntax' {} a -> s {textList = a} :: BatchDetectSyntax) Core.. Core._Sensitive Core.. Lens._Coerce

-- | The language of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\"). All documents must be in the same language.
batchDetectSyntax_languageCode :: Lens.Lens' BatchDetectSyntax SyntaxLanguageCode
batchDetectSyntax_languageCode = Lens.lens (\BatchDetectSyntax' {languageCode} -> languageCode) (\s@BatchDetectSyntax' {} a -> s {languageCode = a} :: BatchDetectSyntax)

instance Core.AWSRequest BatchDetectSyntax where
  type
    AWSResponse BatchDetectSyntax =
      BatchDetectSyntaxResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectSyntaxResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "ResultList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ErrorList" Core..!@ Core.mempty)
      )

instance Core.Hashable BatchDetectSyntax

instance Core.NFData BatchDetectSyntax

instance Core.ToHeaders BatchDetectSyntax where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.BatchDetectSyntax" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDetectSyntax where
  toJSON BatchDetectSyntax' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TextList" Core..= textList),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath BatchDetectSyntax where
  toPath = Core.const "/"

instance Core.ToQuery BatchDetectSyntax where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDetectSyntaxResponse' smart constructor.
data BatchDetectSyntaxResponse = BatchDetectSyntaxResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectSyntaxItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectSyntaxResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectSyntaxResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectSyntaxResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectSyntaxResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectSyntaxResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDetectSyntaxResponse
newBatchDetectSyntaxResponse pHttpStatus_ =
  BatchDetectSyntaxResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Core.mempty,
      errorList = Core.mempty
    }

-- | The response's http status code.
batchDetectSyntaxResponse_httpStatus :: Lens.Lens' BatchDetectSyntaxResponse Core.Int
batchDetectSyntaxResponse_httpStatus = Lens.lens (\BatchDetectSyntaxResponse' {httpStatus} -> httpStatus) (\s@BatchDetectSyntaxResponse' {} a -> s {httpStatus = a} :: BatchDetectSyntaxResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectSyntaxResponse_resultList :: Lens.Lens' BatchDetectSyntaxResponse [BatchDetectSyntaxItemResult]
batchDetectSyntaxResponse_resultList = Lens.lens (\BatchDetectSyntaxResponse' {resultList} -> resultList) (\s@BatchDetectSyntaxResponse' {} a -> s {resultList = a} :: BatchDetectSyntaxResponse) Core.. Lens._Coerce

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectSyntaxResponse_errorList :: Lens.Lens' BatchDetectSyntaxResponse [BatchItemError]
batchDetectSyntaxResponse_errorList = Lens.lens (\BatchDetectSyntaxResponse' {errorList} -> errorList) (\s@BatchDetectSyntaxResponse' {} a -> s {errorList = a} :: BatchDetectSyntaxResponse) Core.. Lens._Coerce

instance Core.NFData BatchDetectSyntaxResponse
