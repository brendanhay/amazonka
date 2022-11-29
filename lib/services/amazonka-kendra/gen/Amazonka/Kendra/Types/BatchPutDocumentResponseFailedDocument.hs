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
-- Module      : Amazonka.Kendra.Types.BatchPutDocumentResponseFailedDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BatchPutDocumentResponseFailedDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a document that could not be indexed.
--
-- /See:/ 'newBatchPutDocumentResponseFailedDocument' smart constructor.
data BatchPutDocumentResponseFailedDocument = BatchPutDocumentResponseFailedDocument'
  { -- | A description of the reason why the document could not be indexed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the document.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of error that caused the document to fail to be indexed.
    errorCode :: Prelude.Maybe ErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutDocumentResponseFailedDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchPutDocumentResponseFailedDocument_errorMessage' - A description of the reason why the document could not be indexed.
--
-- 'id', 'batchPutDocumentResponseFailedDocument_id' - The unique identifier of the document.
--
-- 'errorCode', 'batchPutDocumentResponseFailedDocument_errorCode' - The type of error that caused the document to fail to be indexed.
newBatchPutDocumentResponseFailedDocument ::
  BatchPutDocumentResponseFailedDocument
newBatchPutDocumentResponseFailedDocument =
  BatchPutDocumentResponseFailedDocument'
    { errorMessage =
        Prelude.Nothing,
      id = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A description of the reason why the document could not be indexed.
batchPutDocumentResponseFailedDocument_errorMessage :: Lens.Lens' BatchPutDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchPutDocumentResponseFailedDocument_errorMessage = Lens.lens (\BatchPutDocumentResponseFailedDocument' {errorMessage} -> errorMessage) (\s@BatchPutDocumentResponseFailedDocument' {} a -> s {errorMessage = a} :: BatchPutDocumentResponseFailedDocument)

-- | The unique identifier of the document.
batchPutDocumentResponseFailedDocument_id :: Lens.Lens' BatchPutDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchPutDocumentResponseFailedDocument_id = Lens.lens (\BatchPutDocumentResponseFailedDocument' {id} -> id) (\s@BatchPutDocumentResponseFailedDocument' {} a -> s {id = a} :: BatchPutDocumentResponseFailedDocument)

-- | The type of error that caused the document to fail to be indexed.
batchPutDocumentResponseFailedDocument_errorCode :: Lens.Lens' BatchPutDocumentResponseFailedDocument (Prelude.Maybe ErrorCode)
batchPutDocumentResponseFailedDocument_errorCode = Lens.lens (\BatchPutDocumentResponseFailedDocument' {errorCode} -> errorCode) (\s@BatchPutDocumentResponseFailedDocument' {} a -> s {errorCode = a} :: BatchPutDocumentResponseFailedDocument)

instance
  Core.FromJSON
    BatchPutDocumentResponseFailedDocument
  where
  parseJSON =
    Core.withObject
      "BatchPutDocumentResponseFailedDocument"
      ( \x ->
          BatchPutDocumentResponseFailedDocument'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    BatchPutDocumentResponseFailedDocument
  where
  hashWithSalt
    _salt
    BatchPutDocumentResponseFailedDocument' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    BatchPutDocumentResponseFailedDocument
  where
  rnf BatchPutDocumentResponseFailedDocument' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf errorCode
