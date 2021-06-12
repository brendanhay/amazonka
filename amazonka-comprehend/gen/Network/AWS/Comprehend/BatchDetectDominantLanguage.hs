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
-- Module      : Network.AWS.Comprehend.BatchDetectDominantLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the dominant language of the input text for a batch of
-- documents. For a list of languages that Amazon Comprehend can detect,
-- see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html Amazon Comprehend Supported Languages>.
module Network.AWS.Comprehend.BatchDetectDominantLanguage
  ( -- * Creating a Request
    BatchDetectDominantLanguage (..),
    newBatchDetectDominantLanguage,

    -- * Request Lenses
    batchDetectDominantLanguage_textList,

    -- * Destructuring the Response
    BatchDetectDominantLanguageResponse (..),
    newBatchDetectDominantLanguageResponse,

    -- * Response Lenses
    batchDetectDominantLanguageResponse_httpStatus,
    batchDetectDominantLanguageResponse_resultList,
    batchDetectDominantLanguageResponse_errorList,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDetectDominantLanguage' smart constructor.
data BatchDetectDominantLanguage = BatchDetectDominantLanguage'
  { -- | A list containing the text of the input documents. The list can contain
    -- a maximum of 25 documents. Each document should contain at least 20
    -- characters and must contain fewer than 5,000 bytes of UTF-8 encoded
    -- characters.
    textList :: Core.Sensitive [Core.Sensitive Core.Text]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectDominantLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectDominantLanguage_textList' - A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document should contain at least 20
-- characters and must contain fewer than 5,000 bytes of UTF-8 encoded
-- characters.
newBatchDetectDominantLanguage ::
  BatchDetectDominantLanguage
newBatchDetectDominantLanguage =
  BatchDetectDominantLanguage'
    { textList =
        Core.mempty
    }

-- | A list containing the text of the input documents. The list can contain
-- a maximum of 25 documents. Each document should contain at least 20
-- characters and must contain fewer than 5,000 bytes of UTF-8 encoded
-- characters.
batchDetectDominantLanguage_textList :: Lens.Lens' BatchDetectDominantLanguage [Core.Text]
batchDetectDominantLanguage_textList = Lens.lens (\BatchDetectDominantLanguage' {textList} -> textList) (\s@BatchDetectDominantLanguage' {} a -> s {textList = a} :: BatchDetectDominantLanguage) Core.. Core._Sensitive Core.. Lens._Coerce

instance Core.AWSRequest BatchDetectDominantLanguage where
  type
    AWSResponse BatchDetectDominantLanguage =
      BatchDetectDominantLanguageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectDominantLanguageResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "ResultList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ErrorList" Core..!@ Core.mempty)
      )

instance Core.Hashable BatchDetectDominantLanguage

instance Core.NFData BatchDetectDominantLanguage

instance Core.ToHeaders BatchDetectDominantLanguage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.BatchDetectDominantLanguage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDetectDominantLanguage where
  toJSON BatchDetectDominantLanguage' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TextList" Core..= textList)]
      )

instance Core.ToPath BatchDetectDominantLanguage where
  toPath = Core.const "/"

instance Core.ToQuery BatchDetectDominantLanguage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDetectDominantLanguageResponse' smart constructor.
data BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectDominantLanguageItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectDominantLanguageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectDominantLanguageResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectDominantLanguageResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectDominantLanguageResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectDominantLanguageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDetectDominantLanguageResponse
newBatchDetectDominantLanguageResponse pHttpStatus_ =
  BatchDetectDominantLanguageResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Core.mempty,
      errorList = Core.mempty
    }

-- | The response's http status code.
batchDetectDominantLanguageResponse_httpStatus :: Lens.Lens' BatchDetectDominantLanguageResponse Core.Int
batchDetectDominantLanguageResponse_httpStatus = Lens.lens (\BatchDetectDominantLanguageResponse' {httpStatus} -> httpStatus) (\s@BatchDetectDominantLanguageResponse' {} a -> s {httpStatus = a} :: BatchDetectDominantLanguageResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectDominantLanguageResponse_resultList :: Lens.Lens' BatchDetectDominantLanguageResponse [BatchDetectDominantLanguageItemResult]
batchDetectDominantLanguageResponse_resultList = Lens.lens (\BatchDetectDominantLanguageResponse' {resultList} -> resultList) (\s@BatchDetectDominantLanguageResponse' {} a -> s {resultList = a} :: BatchDetectDominantLanguageResponse) Core.. Lens._Coerce

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectDominantLanguageResponse_errorList :: Lens.Lens' BatchDetectDominantLanguageResponse [BatchItemError]
batchDetectDominantLanguageResponse_errorList = Lens.lens (\BatchDetectDominantLanguageResponse' {errorList} -> errorList) (\s@BatchDetectDominantLanguageResponse' {} a -> s {errorList = a} :: BatchDetectDominantLanguageResponse) Core.. Lens._Coerce

instance
  Core.NFData
    BatchDetectDominantLanguageResponse
