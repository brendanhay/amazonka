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
-- Module      : Amazonka.CodeCommit.Types.BatchDescribeMergeConflictsError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.BatchDescribeMergeConflictsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about errors in a BatchDescribeMergeConflicts
-- operation.
--
-- /See:/ 'newBatchDescribeMergeConflictsError' smart constructor.
data BatchDescribeMergeConflictsError = BatchDescribeMergeConflictsError'
  { -- | The path to the file.
    filePath :: Prelude.Text,
    -- | The name of the exception.
    exceptionName :: Prelude.Text,
    -- | The message provided by the exception.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeMergeConflictsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'batchDescribeMergeConflictsError_filePath' - The path to the file.
--
-- 'exceptionName', 'batchDescribeMergeConflictsError_exceptionName' - The name of the exception.
--
-- 'message', 'batchDescribeMergeConflictsError_message' - The message provided by the exception.
newBatchDescribeMergeConflictsError ::
  -- | 'filePath'
  Prelude.Text ->
  -- | 'exceptionName'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  BatchDescribeMergeConflictsError
newBatchDescribeMergeConflictsError
  pFilePath_
  pExceptionName_
  pMessage_ =
    BatchDescribeMergeConflictsError'
      { filePath =
          pFilePath_,
        exceptionName = pExceptionName_,
        message = pMessage_
      }

-- | The path to the file.
batchDescribeMergeConflictsError_filePath :: Lens.Lens' BatchDescribeMergeConflictsError Prelude.Text
batchDescribeMergeConflictsError_filePath = Lens.lens (\BatchDescribeMergeConflictsError' {filePath} -> filePath) (\s@BatchDescribeMergeConflictsError' {} a -> s {filePath = a} :: BatchDescribeMergeConflictsError)

-- | The name of the exception.
batchDescribeMergeConflictsError_exceptionName :: Lens.Lens' BatchDescribeMergeConflictsError Prelude.Text
batchDescribeMergeConflictsError_exceptionName = Lens.lens (\BatchDescribeMergeConflictsError' {exceptionName} -> exceptionName) (\s@BatchDescribeMergeConflictsError' {} a -> s {exceptionName = a} :: BatchDescribeMergeConflictsError)

-- | The message provided by the exception.
batchDescribeMergeConflictsError_message :: Lens.Lens' BatchDescribeMergeConflictsError Prelude.Text
batchDescribeMergeConflictsError_message = Lens.lens (\BatchDescribeMergeConflictsError' {message} -> message) (\s@BatchDescribeMergeConflictsError' {} a -> s {message = a} :: BatchDescribeMergeConflictsError)

instance
  Data.FromJSON
    BatchDescribeMergeConflictsError
  where
  parseJSON =
    Data.withObject
      "BatchDescribeMergeConflictsError"
      ( \x ->
          BatchDescribeMergeConflictsError'
            Prelude.<$> (x Data..: "filePath")
            Prelude.<*> (x Data..: "exceptionName")
            Prelude.<*> (x Data..: "message")
      )

instance
  Prelude.Hashable
    BatchDescribeMergeConflictsError
  where
  hashWithSalt
    _salt
    BatchDescribeMergeConflictsError' {..} =
      _salt
        `Prelude.hashWithSalt` filePath
        `Prelude.hashWithSalt` exceptionName
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    BatchDescribeMergeConflictsError
  where
  rnf BatchDescribeMergeConflictsError' {..} =
    Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf exceptionName
      `Prelude.seq` Prelude.rnf message
