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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BatchGetDocumentStatusResponseError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides a response when the status of a document could not be
-- retrieved.
--
-- /See:/ 'newBatchGetDocumentStatusResponseError' smart constructor.
data BatchGetDocumentStatusResponseError = BatchGetDocumentStatusResponseError'
  { -- | The identifier of the document whose status could not be retrieved.
    documentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the source of the error.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | States that the API could not get the status of a document. This could
    -- be because the request is not valid or there is a system error.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'documentId', 'batchGetDocumentStatusResponseError_documentId' - The identifier of the document whose status could not be retrieved.
--
-- 'errorCode', 'batchGetDocumentStatusResponseError_errorCode' - Indicates the source of the error.
--
-- 'errorMessage', 'batchGetDocumentStatusResponseError_errorMessage' - States that the API could not get the status of a document. This could
-- be because the request is not valid or there is a system error.
newBatchGetDocumentStatusResponseError ::
  BatchGetDocumentStatusResponseError
newBatchGetDocumentStatusResponseError =
  BatchGetDocumentStatusResponseError'
    { documentId =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The identifier of the document whose status could not be retrieved.
batchGetDocumentStatusResponseError_documentId :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe Prelude.Text)
batchGetDocumentStatusResponseError_documentId = Lens.lens (\BatchGetDocumentStatusResponseError' {documentId} -> documentId) (\s@BatchGetDocumentStatusResponseError' {} a -> s {documentId = a} :: BatchGetDocumentStatusResponseError)

-- | Indicates the source of the error.
batchGetDocumentStatusResponseError_errorCode :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe ErrorCode)
batchGetDocumentStatusResponseError_errorCode = Lens.lens (\BatchGetDocumentStatusResponseError' {errorCode} -> errorCode) (\s@BatchGetDocumentStatusResponseError' {} a -> s {errorCode = a} :: BatchGetDocumentStatusResponseError)

-- | States that the API could not get the status of a document. This could
-- be because the request is not valid or there is a system error.
batchGetDocumentStatusResponseError_errorMessage :: Lens.Lens' BatchGetDocumentStatusResponseError (Prelude.Maybe Prelude.Text)
batchGetDocumentStatusResponseError_errorMessage = Lens.lens (\BatchGetDocumentStatusResponseError' {errorMessage} -> errorMessage) (\s@BatchGetDocumentStatusResponseError' {} a -> s {errorMessage = a} :: BatchGetDocumentStatusResponseError)

instance
  Data.FromJSON
    BatchGetDocumentStatusResponseError
  where
  parseJSON =
    Data.withObject
      "BatchGetDocumentStatusResponseError"
      ( \x ->
          BatchGetDocumentStatusResponseError'
            Prelude.<$> (x Data..:? "DocumentId")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance
  Prelude.Hashable
    BatchGetDocumentStatusResponseError
  where
  hashWithSalt
    _salt
    BatchGetDocumentStatusResponseError' {..} =
      _salt
        `Prelude.hashWithSalt` documentId
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage

instance
  Prelude.NFData
    BatchGetDocumentStatusResponseError
  where
  rnf BatchGetDocumentStatusResponseError' {..} =
    Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
