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
-- Module      : Amazonka.Comprehend.BatchDetectSyntax
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the text of a batch of documents for the syntax and part of
-- speech of the words in the document and returns information about them.
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.BatchDetectSyntax
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDetectSyntax' smart constructor.
data BatchDetectSyntax = BatchDetectSyntax'
  { -- | A list containing the UTF-8 encoded text of the input documents. The
    -- list can contain a maximum of 25 documents. The maximum size for each
    -- document is 5 KB.
    textList :: Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)),
    -- | The language of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\"). All documents must be in the same language.
    languageCode :: SyntaxLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectSyntax' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'batchDetectSyntax_textList' - A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size for each
-- document is 5 KB.
--
-- 'languageCode', 'batchDetectSyntax_languageCode' - The language of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\"). All documents must be in the same language.
newBatchDetectSyntax ::
  -- | 'textList'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'languageCode'
  SyntaxLanguageCode ->
  BatchDetectSyntax
newBatchDetectSyntax pTextList_ pLanguageCode_ =
  BatchDetectSyntax'
    { textList =
        Data._Sensitive Prelude.. Lens.coerced
          Lens.# pTextList_,
      languageCode = pLanguageCode_
    }

-- | A list containing the UTF-8 encoded text of the input documents. The
-- list can contain a maximum of 25 documents. The maximum size for each
-- document is 5 KB.
batchDetectSyntax_textList :: Lens.Lens' BatchDetectSyntax (Prelude.NonEmpty Prelude.Text)
batchDetectSyntax_textList = Lens.lens (\BatchDetectSyntax' {textList} -> textList) (\s@BatchDetectSyntax' {} a -> s {textList = a} :: BatchDetectSyntax) Prelude.. Data._Sensitive Prelude.. Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDetectSyntaxResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ResultList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDetectSyntax where
  hashWithSalt _salt BatchDetectSyntax' {..} =
    _salt `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData BatchDetectSyntax where
  rnf BatchDetectSyntax' {..} =
    Prelude.rnf textList
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders BatchDetectSyntax where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.BatchDetectSyntax" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDetectSyntax where
  toJSON BatchDetectSyntax' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TextList" Data..= textList),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath BatchDetectSyntax where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDetectSyntax where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDetectSyntaxResponse' smart constructor.
data BatchDetectSyntaxResponse = BatchDetectSyntaxResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchDetectSyntaxResponse
newBatchDetectSyntaxResponse pHttpStatus_ =
  BatchDetectSyntaxResponse'
    { httpStatus =
        pHttpStatus_,
      resultList = Prelude.mempty,
      errorList = Prelude.mempty
    }

-- | The response's http status code.
batchDetectSyntaxResponse_httpStatus :: Lens.Lens' BatchDetectSyntaxResponse Prelude.Int
batchDetectSyntaxResponse_httpStatus = Lens.lens (\BatchDetectSyntaxResponse' {httpStatus} -> httpStatus) (\s@BatchDetectSyntaxResponse' {} a -> s {httpStatus = a} :: BatchDetectSyntaxResponse)

-- | A list of objects containing the results of the operation. The results
-- are sorted in ascending order by the @Index@ field and match the order
-- of the documents in the input list. If all of the documents contain an
-- error, the @ResultList@ is empty.
batchDetectSyntaxResponse_resultList :: Lens.Lens' BatchDetectSyntaxResponse [BatchDetectSyntaxItemResult]
batchDetectSyntaxResponse_resultList = Lens.lens (\BatchDetectSyntaxResponse' {resultList} -> resultList) (\s@BatchDetectSyntaxResponse' {} a -> s {resultList = a} :: BatchDetectSyntaxResponse) Prelude.. Lens.coerced

-- | A list containing one object for each document that contained an error.
-- The results are sorted in ascending order by the @Index@ field and match
-- the order of the documents in the input list. If there are no errors in
-- the batch, the @ErrorList@ is empty.
batchDetectSyntaxResponse_errorList :: Lens.Lens' BatchDetectSyntaxResponse [BatchItemError]
batchDetectSyntaxResponse_errorList = Lens.lens (\BatchDetectSyntaxResponse' {errorList} -> errorList) (\s@BatchDetectSyntaxResponse' {} a -> s {errorList = a} :: BatchDetectSyntaxResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchDetectSyntaxResponse where
  rnf BatchDetectSyntaxResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resultList
      `Prelude.seq` Prelude.rnf errorList
