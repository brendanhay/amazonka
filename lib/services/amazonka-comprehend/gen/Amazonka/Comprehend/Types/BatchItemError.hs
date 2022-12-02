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
-- Module      : Amazonka.Comprehend.Types.BatchItemError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchItemError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an error that occurred while processing a document in a batch.
-- The operation returns on @BatchItemError@ object for each document that
-- contained an error.
--
-- /See:/ 'newBatchItemError' smart constructor.
data BatchItemError = BatchItemError'
  { -- | A text description of the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | The numeric error code of the error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchItemError_errorMessage' - A text description of the error.
--
-- 'index', 'batchItemError_index' - The zero-based index of the document in the input list.
--
-- 'errorCode', 'batchItemError_errorCode' - The numeric error code of the error.
newBatchItemError ::
  BatchItemError
newBatchItemError =
  BatchItemError'
    { errorMessage = Prelude.Nothing,
      index = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A text description of the error.
batchItemError_errorMessage :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Text)
batchItemError_errorMessage = Lens.lens (\BatchItemError' {errorMessage} -> errorMessage) (\s@BatchItemError' {} a -> s {errorMessage = a} :: BatchItemError)

-- | The zero-based index of the document in the input list.
batchItemError_index :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Int)
batchItemError_index = Lens.lens (\BatchItemError' {index} -> index) (\s@BatchItemError' {} a -> s {index = a} :: BatchItemError)

-- | The numeric error code of the error.
batchItemError_errorCode :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Text)
batchItemError_errorCode = Lens.lens (\BatchItemError' {errorCode} -> errorCode) (\s@BatchItemError' {} a -> s {errorCode = a} :: BatchItemError)

instance Data.FromJSON BatchItemError where
  parseJSON =
    Data.withObject
      "BatchItemError"
      ( \x ->
          BatchItemError'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable BatchItemError where
  hashWithSalt _salt BatchItemError' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData BatchItemError where
  rnf BatchItemError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf index
      `Prelude.seq` Prelude.rnf errorCode
