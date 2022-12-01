{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.Types.BatchGetDocumentStatusResponseError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BatchGetDocumentStatusResponseError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides a response when the status of a document could not be
-- retrieved.
--
-- /See:/ 'newBatchGetDocumentStatusResponseError' smart constructor.
data BatchGetDocumentStatusResponseError = BatchGetDocumentStatusResponseError'
  { -- | States that the API could not get the status of a document. This could
    -- be because the request is not valid or there is a system error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates the source of the error.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The unique identifier of the document whose status could not be
    -- retrieved.
    documentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDocumentStatusResponseError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchGetDocumentStatusResponseError_errorMessage' - States that the API could not get the status of a document. This could
-- be because the request is not valid or there is a system error.
--
-- 'errorCode', 'batchGetDocumentStatusResponseError_errorCode' - Indicates the source of the error.
--
-- 'documentId', 'batchGetDocumentStatusResponseError_documentId' - The unique identifier of the document whose status could not be
-- retrieved.
newBatchGetDocumentStatusResponseError ::
  BatchGetDocumentStatusResponseError
newBatchGetDocumentStatusResponseError =
  BatchGetDocumentStatusResponseError'
    { errorMessage =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      documentId = Prelude.Nothing
    }

-- | States that the API could not get the status of a document. This could
-- be because the request is not valid or there is a system error.
batchGetDocumentStatusResponseError_errorMessage :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe Prelude.Text)
batchGetDocumentStatusResponseError_errorMessage = Lens.lens (\BatchGetDocumentStatusResponseError' {errorMessage} -> errorMessage) (\s@BatchGetDocumentStatusResponseError' {} a -> s {errorMessage = a} :: BatchGetDocumentStatusResponseError)

-- | Indicates the source of the error.
batchGetDocumentStatusResponseError_errorCode :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe ErrorCode)
batchGetDocumentStatusResponseError_errorCode = Lens.lens (\BatchGetDocumentStatusResponseError' {errorCode} -> errorCode) (\s@BatchGetDocumentStatusResponseError' {} a -> s {errorCode = a} :: BatchGetDocumentStatusResponseError)

-- | The unique identifier of the document whose status could not be
-- retrieved.
batchGetDocumentStatusResponseError_documentId :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe Prelude.Text)
batchGetDocumentStatusResponseError_documentId = Lens.lens (\BatchGetDocumentStatusResponseError' {documentId} -> documentId) (\s@BatchGetDocumentStatusResponseError' {} a -> s {documentId = a} :: BatchGetDocumentStatusResponseError)

instance
  Core.FromJSON
    BatchGetDocumentStatusResponseError
  where
  parseJSON =
    Core.withObject
      "BatchGetDocumentStatusResponseError"
      ( \x ->
          BatchGetDocumentStatusResponseError'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "DocumentId")
      )

instance
  Prelude.Hashable
    BatchGetDocumentStatusResponseError
  where
  hashWithSalt
    _salt
    BatchGetDocumentStatusResponseError' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` documentId

instance
  Prelude.NFData
    BatchGetDocumentStatusResponseError
  where
  rnf BatchGetDocumentStatusResponseError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf documentId
