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
-- Module      : Amazonka.Textract.AnalyzeExpense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @AnalyzeExpense@ synchronously analyzes an input document for
-- financially related relationships between text.
--
-- Information is returned as @ExpenseDocuments@ and seperated as follows:
--
-- -   @LineItemGroups@- A data set containing @LineItems@ which store
--     information about the lines of text, such as an item purchased and
--     its price on a receipt.
--
-- -   @SummaryFields@- Contains all other information a receipt, such as
--     header information or the vendors name.
module Amazonka.Textract.AnalyzeExpense
  ( -- * Creating a Request
    AnalyzeExpense (..),
    newAnalyzeExpense,

    -- * Request Lenses
    analyzeExpense_document,

    -- * Destructuring the Response
    AnalyzeExpenseResponse (..),
    newAnalyzeExpenseResponse,

    -- * Response Lenses
    analyzeExpenseResponse_documentMetadata,
    analyzeExpenseResponse_expenseDocuments,
    analyzeExpenseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newAnalyzeExpense' smart constructor.
data AnalyzeExpense = AnalyzeExpense'
  { document :: Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeExpense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'analyzeExpense_document' - Undocumented member.
newAnalyzeExpense ::
  -- | 'document'
  Document ->
  AnalyzeExpense
newAnalyzeExpense pDocument_ =
  AnalyzeExpense' {document = pDocument_}

-- | Undocumented member.
analyzeExpense_document :: Lens.Lens' AnalyzeExpense Document
analyzeExpense_document = Lens.lens (\AnalyzeExpense' {document} -> document) (\s@AnalyzeExpense' {} a -> s {document = a} :: AnalyzeExpense)

instance Core.AWSRequest AnalyzeExpense where
  type
    AWSResponse AnalyzeExpense =
      AnalyzeExpenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AnalyzeExpenseResponse'
            Prelude.<$> (x Data..?> "DocumentMetadata")
            Prelude.<*> ( x
                            Data..?> "ExpenseDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AnalyzeExpense where
  hashWithSalt _salt AnalyzeExpense' {..} =
    _salt `Prelude.hashWithSalt` document

instance Prelude.NFData AnalyzeExpense where
  rnf AnalyzeExpense' {..} = Prelude.rnf document

instance Data.ToHeaders AnalyzeExpense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Textract.AnalyzeExpense" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AnalyzeExpense where
  toJSON AnalyzeExpense' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Document" Data..= document)]
      )

instance Data.ToPath AnalyzeExpense where
  toPath = Prelude.const "/"

instance Data.ToQuery AnalyzeExpense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAnalyzeExpenseResponse' smart constructor.
data AnalyzeExpenseResponse = AnalyzeExpenseResponse'
  { documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The expenses detected by Amazon Textract.
    expenseDocuments :: Prelude.Maybe [ExpenseDocument],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeExpenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentMetadata', 'analyzeExpenseResponse_documentMetadata' - Undocumented member.
--
-- 'expenseDocuments', 'analyzeExpenseResponse_expenseDocuments' - The expenses detected by Amazon Textract.
--
-- 'httpStatus', 'analyzeExpenseResponse_httpStatus' - The response's http status code.
newAnalyzeExpenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AnalyzeExpenseResponse
newAnalyzeExpenseResponse pHttpStatus_ =
  AnalyzeExpenseResponse'
    { documentMetadata =
        Prelude.Nothing,
      expenseDocuments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
analyzeExpenseResponse_documentMetadata :: Lens.Lens' AnalyzeExpenseResponse (Prelude.Maybe DocumentMetadata)
analyzeExpenseResponse_documentMetadata = Lens.lens (\AnalyzeExpenseResponse' {documentMetadata} -> documentMetadata) (\s@AnalyzeExpenseResponse' {} a -> s {documentMetadata = a} :: AnalyzeExpenseResponse)

-- | The expenses detected by Amazon Textract.
analyzeExpenseResponse_expenseDocuments :: Lens.Lens' AnalyzeExpenseResponse (Prelude.Maybe [ExpenseDocument])
analyzeExpenseResponse_expenseDocuments = Lens.lens (\AnalyzeExpenseResponse' {expenseDocuments} -> expenseDocuments) (\s@AnalyzeExpenseResponse' {} a -> s {expenseDocuments = a} :: AnalyzeExpenseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
analyzeExpenseResponse_httpStatus :: Lens.Lens' AnalyzeExpenseResponse Prelude.Int
analyzeExpenseResponse_httpStatus = Lens.lens (\AnalyzeExpenseResponse' {httpStatus} -> httpStatus) (\s@AnalyzeExpenseResponse' {} a -> s {httpStatus = a} :: AnalyzeExpenseResponse)

instance Prelude.NFData AnalyzeExpenseResponse where
  rnf AnalyzeExpenseResponse' {..} =
    Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf expenseDocuments
      `Prelude.seq` Prelude.rnf httpStatus
