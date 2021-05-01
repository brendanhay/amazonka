{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Comprehend.Types.BatchItemError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchItemError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an error that occurred while processing a document in a batch.
-- The operation returns on @BatchItemError@ object for each document that
-- contained an error.
--
-- /See:/ 'newBatchItemError' smart constructor.
data BatchItemError = BatchItemError'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | A text description of the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The numeric error code of the error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchItemError_index' - The zero-based index of the document in the input list.
--
-- 'errorMessage', 'batchItemError_errorMessage' - A text description of the error.
--
-- 'errorCode', 'batchItemError_errorCode' - The numeric error code of the error.
newBatchItemError ::
  BatchItemError
newBatchItemError =
  BatchItemError'
    { index = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchItemError_index :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Int)
batchItemError_index = Lens.lens (\BatchItemError' {index} -> index) (\s@BatchItemError' {} a -> s {index = a} :: BatchItemError)

-- | A text description of the error.
batchItemError_errorMessage :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Text)
batchItemError_errorMessage = Lens.lens (\BatchItemError' {errorMessage} -> errorMessage) (\s@BatchItemError' {} a -> s {errorMessage = a} :: BatchItemError)

-- | The numeric error code of the error.
batchItemError_errorCode :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Text)
batchItemError_errorCode = Lens.lens (\BatchItemError' {errorCode} -> errorCode) (\s@BatchItemError' {} a -> s {errorCode = a} :: BatchItemError)

instance Prelude.FromJSON BatchItemError where
  parseJSON =
    Prelude.withObject
      "BatchItemError"
      ( \x ->
          BatchItemError'
            Prelude.<$> (x Prelude..:? "Index")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable BatchItemError

instance Prelude.NFData BatchItemError
