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
-- Module      : Amazonka.Comprehend.BatchDetectEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for named entities and returns
-- information about them. For more information about named entities, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-entities.html Entities>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.BatchDetectEntities
  ( -- * Creating a Request
    BatchDetectEntities (..),
    newBatchDetectEntities,

    -- * Request Lenses
    batchDetectEntities_textList,
    batchDetectEntities_languageCode,

    -- * Destructuring the Response
    BatchDetectEntitiesResponse (..),
    newBatchDetectEntitiesResponse,

    -- * Response Lenses
    batchDetectEntitiesResponse_httpStatus,
    batchDetectEntitiesResponse_resultList,
    batchDetectEntitiesResponse_errorList,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDetectEntities' smart constructor.
data BatchDetectEntities = BatchDetectEntities'
  { -- | A list containing the UTF-8 encoded text of the input documents. The
    -- list can contain a maximum of 25 documents. The maximum size of each
    -- document is 5 KB.
    textList :: Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)),
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectEntities_textList' - A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
--
-- 'languageCode', 'batchDetectEntities_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
newBatchDetectEntities ::
  -- | 'textList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  BatchDetectEntities
newBatchDetectEntities pTextList_ pLanguageCode_ =
  BatchDetectEntities'
    { textList =
        Data._Sensitive Prelude.. Lens.coerced
          Lens.# pTextList_,
      languageCode = pLanguageCode_
    }

-- | A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size of each
-- document is 5 KB.
batchDetectEntities_textList :: Lens.Lens' BatchDetectEntities (Prelude.NonEmpty Prelude.Text)
batchDetectEntities_textList = Lens.lens (\BatchDetectEntities' {textList} -> textList) (\s@BatchDetectEntities' {} a -> s {textList = a} :: BatchDetectEntities) Prelude.. Data._Sensitive Prelude.. Lens.coerced

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
batchDetectEntities_languageCode :: Lens.Lens' BatchDetectEntities LanguageCode
batchDetectEntities_languageCode = Lens.lens (\BatchDetectEntities' {languageCode} -> languageCode) (\s@BatchDetectEntities' {} a -> s {languageCode = a} :: BatchDetectEntities)

instance Core.AWSRequest BatchDetectEntities where
  type
    AWSResponse BatchDetectEntities =
      BatchDetectEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectEntitiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResultList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDetectEntities where
  hashWithSalt _salt BatchDetectEntities' {..} =
    _salt `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData BatchDetectEntities where
  rnf BatchDetectEntities' {..} =
    Prelude.rnf textList
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders BatchDetectEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.BatchDetectEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDetectEntities where
  toJSON BatchDetectEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Data..= textList),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath BatchDetectEntities where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDetectEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectEntitiesResponse' smart constructor.
data BatchDetectEntitiesResponse = BatchDetectEntitiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects containing the results of the operation. The results
    -- are sorted in ascending order by the @Index@ field and match the order
    -- of the documents in the input list. If all of the documents contain an
    -- error, the @ResultList@ is empty.
    resultList :: [BatchDetectEntitiesItemResult],
    -- | A list containing one object for each document that contained an error.
    -- The results are sorted in ascending order by the @Index@ field and match
    -- the order of the documents in the input list. If there are no errors in
    -- the batch, the @ErrorList@ is empty.
    errorList :: [BatchItemError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDetectEntitiesResponse_httpStatus' - The response's http status code.
--
-- 'resultList', 'batchDetectEntitiesResponse_resultList' - A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
--
-- 'errorList', 'batchDetectEntitiesResponse_errorList' - A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
newBatchDetectEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDetectEntitiesResponse
newBatchDetectEntitiesResponse pHttpStatus_ =
  BatchDetectEntitiesResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectEntitiesResponse_httpStatus :: Lens.Lens' BatchDetectEntitiesResponse Prelude.Int
batchDetectEntitiesResponse_httpStatus = Lens.lens (\BatchDetectEntitiesResponse' {httpStatus} -> httpStatus) (\s@BatchDetectEntitiesResponse' {} a -> s {httpStatus = a} :: BatchDetectEntitiesResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectEntitiesResponse_resultList :: Lens.Lens' BatchDetectEntitiesResponse [BatchDetectEntitiesItemResult]
batchDetectEntitiesResponse_resultList = Lens.lens (\BatchDetectEntitiesResponse' {resultList} -> resultList) (\s@BatchDetectEntitiesResponse' {} a -> s {resultList = a} :: BatchDetectEntitiesResponse) Prelude.. Lens.coerced

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectEntitiesResponse_errorList :: Lens.Lens' BatchDetectEntitiesResponse [BatchItemError]
batchDetectEntitiesResponse_errorList = Lens.lens (\BatchDetectEntitiesResponse' {errorList} -> errorList) (\s@BatchDetectEntitiesResponse' {} a -> s {errorList = a} :: BatchDetectEntitiesResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchDetectEntitiesResponse where
  rnf BatchDetectEntitiesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resultList
      `Prelude.seq` Prelude.rnf errorList
