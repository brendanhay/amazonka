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
-- Module      : Network.AWS.Kendra.Types.BatchDeleteDocumentResponseFailedDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.BatchDeleteDocumentResponseFailedDocument where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about documents that could not be removed from an
-- index by the @BatchDeleteDocument@ operation.
--
-- /See:/ 'newBatchDeleteDocumentResponseFailedDocument' smart constructor.
data BatchDeleteDocumentResponseFailedDocument = BatchDeleteDocumentResponseFailedDocument'
  { -- | The error code for why the document couldn\'t be removed from the index.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The identifier of the document that couldn\'t be removed from the index.
    id :: Prelude.Maybe Prelude.Text,
    -- | An explanation for why the document couldn\'t be removed from the index.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'id', 'batchDeleteDocumentResponseFailedDocument_id' - The identifier of the document that couldn\'t be removed from the index.
--
-- 'errorMessage', 'batchDeleteDocumentResponseFailedDocument_errorMessage' - An explanation for why the document couldn\'t be removed from the index.
newBatchDeleteDocumentResponseFailedDocument ::
  BatchDeleteDocumentResponseFailedDocument
newBatchDeleteDocumentResponseFailedDocument =
  BatchDeleteDocumentResponseFailedDocument'
    { errorCode =
        Prelude.Nothing,
      id = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code for why the document couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_errorCode :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe ErrorCode)
batchDeleteDocumentResponseFailedDocument_errorCode = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {errorCode} -> errorCode) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {errorCode = a} :: BatchDeleteDocumentResponseFailedDocument)

-- | The identifier of the document that couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_id :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchDeleteDocumentResponseFailedDocument_id = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {id} -> id) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {id = a} :: BatchDeleteDocumentResponseFailedDocument)

-- | An explanation for why the document couldn\'t be removed from the index.
batchDeleteDocumentResponseFailedDocument_errorMessage :: Lens.Lens' BatchDeleteDocumentResponseFailedDocument (Prelude.Maybe Prelude.Text)
batchDeleteDocumentResponseFailedDocument_errorMessage = Lens.lens (\BatchDeleteDocumentResponseFailedDocument' {errorMessage} -> errorMessage) (\s@BatchDeleteDocumentResponseFailedDocument' {} a -> s {errorMessage = a} :: BatchDeleteDocumentResponseFailedDocument)

instance
  Core.FromJSON
    BatchDeleteDocumentResponseFailedDocument
  where
  parseJSON =
    Core.withObject
      "BatchDeleteDocumentResponseFailedDocument"
      ( \x ->
          BatchDeleteDocumentResponseFailedDocument'
            Prelude.<$> (x Core..:? "ErrorCode")
              Prelude.<*> (x Core..:? "Id")
              Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance
  Prelude.Hashable
    BatchDeleteDocumentResponseFailedDocument

instance
  Prelude.NFData
    BatchDeleteDocumentResponseFailedDocument
