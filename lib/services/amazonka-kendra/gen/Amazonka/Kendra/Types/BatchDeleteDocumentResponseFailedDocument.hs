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
-- Module      : Amazonka.Kendra.Types.BatchDeleteDocumentResponseFailedDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.BatchDeleteDocumentResponseFailedDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Provides information about documents that could not be removed from an
-- index by the @BatchDeleteDocument@ API.
--
-- /See:/ 'newBatchDeleteDocumentResponseFailedDocument' smart constructor.
data BatchDeleteDocumentResponseFailedDocument = BatchDeleteDocumentResponseFailedDocument'
  { -- | The error code for why the document couldn\'t be removed from the index.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | An explanation for why the document couldn\'t be removed from the index.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the document that couldn\'t be removed from the index.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDocumentResponseFailedDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'batchDeleteDocumentResponseFailedDocument_errorCode' - The error code for why the document couldn\'t be removed from the index.
--
-- 'errorMessage', 'batchDeleteDocumentResponseFailedDocument_errorMessage' - An explanation for why the document couldn\'t be removed from the index.
--
-- 'id', 'batchDeleteDocumentResponseFailedDocument_id' - The identifier of the document that couldn\'t be removed from the index.
newBatchDeleteDocumentResponseFailedDocument ::
  BatchDeleteDocumentResponseFailedDocument
newBatchDeleteDocumentResponseFailedDocument =
  BatchDeleteDocumentResponseFailedDocument'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The error code for why the document couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_errorCode :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe ErrorCode)
batchDeleteDocumentResponseFailedDocument_errorCode = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {errorCode} -> errorCode) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {errorCode = a} :: BatchDeleteDocumentResponseFailedDocument)

-- | An explanation for why the document couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_errorMessage :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchDeleteDocumentResponseFailedDocument_errorMessage = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {errorMessage} -> errorMessage) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {errorMessage = a} :: BatchDeleteDocumentResponseFailedDocument)

-- | The identifier of the document that couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_id :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchDeleteDocumentResponseFailedDocument_id = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {id} -> id) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {id = a} :: BatchDeleteDocumentResponseFailedDocument)

instance
  Data.FromJSON
    BatchDeleteDocumentResponseFailedDocument
  where
  parseJSON =
    Data.withObject
      "BatchDeleteDocumentResponseFailedDocument"
      ( \x ->
          BatchDeleteDocumentResponseFailedDocument'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Id")
      )

instance
  Prelude.Hashable
    BatchDeleteDocumentResponseFailedDocument
  where
  hashWithSalt
    _salt
    BatchDeleteDocumentResponseFailedDocument' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    BatchDeleteDocumentResponseFailedDocument
  where
  rnf BatchDeleteDocumentResponseFailedDocument' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
