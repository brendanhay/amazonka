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
-- Module      : Amazonka.Discovery.Types.BatchDeleteImportDataError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.BatchDeleteImportDataError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types.BatchDeleteImportDataErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Error messages returned for each import task that you deleted as a
-- response for this command.
--
-- /See:/ 'newBatchDeleteImportDataError' smart constructor.
data BatchDeleteImportDataError = BatchDeleteImportDataError'
  { -- | The unique import ID associated with the error that occurred.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | The type of error that occurred for a specific import task.
    errorCode :: Prelude.Maybe BatchDeleteImportDataErrorCode,
    -- | The description of the error that occurred for a specific import task.
    errorDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteImportDataError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importTaskId', 'batchDeleteImportDataError_importTaskId' - The unique import ID associated with the error that occurred.
--
-- 'errorCode', 'batchDeleteImportDataError_errorCode' - The type of error that occurred for a specific import task.
--
-- 'errorDescription', 'batchDeleteImportDataError_errorDescription' - The description of the error that occurred for a specific import task.
newBatchDeleteImportDataError ::
  BatchDeleteImportDataError
newBatchDeleteImportDataError =
  BatchDeleteImportDataError'
    { importTaskId =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorDescription = Prelude.Nothing
    }

-- | The unique import ID associated with the error that occurred.
batchDeleteImportDataError_importTaskId :: Lens.Lens' BatchDeleteImportDataError (Prelude.Maybe Prelude.Text)
batchDeleteImportDataError_importTaskId = Lens.lens (\BatchDeleteImportDataError' {importTaskId} -> importTaskId) (\s@BatchDeleteImportDataError' {} a -> s {importTaskId = a} :: BatchDeleteImportDataError)

-- | The type of error that occurred for a specific import task.
batchDeleteImportDataError_errorCode :: Lens.Lens' BatchDeleteImportDataError (Prelude.Maybe BatchDeleteImportDataErrorCode)
batchDeleteImportDataError_errorCode = Lens.lens (\BatchDeleteImportDataError' {errorCode} -> errorCode) (\s@BatchDeleteImportDataError' {} a -> s {errorCode = a} :: BatchDeleteImportDataError)

-- | The description of the error that occurred for a specific import task.
batchDeleteImportDataError_errorDescription :: Lens.Lens' BatchDeleteImportDataError (Prelude.Maybe Prelude.Text)
batchDeleteImportDataError_errorDescription = Lens.lens (\BatchDeleteImportDataError' {errorDescription} -> errorDescription) (\s@BatchDeleteImportDataError' {} a -> s {errorDescription = a} :: BatchDeleteImportDataError)

instance Data.FromJSON BatchDeleteImportDataError where
  parseJSON =
    Data.withObject
      "BatchDeleteImportDataError"
      ( \x ->
          BatchDeleteImportDataError'
            Prelude.<$> (x Data..:? "importTaskId")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorDescription")
      )

instance Prelude.Hashable BatchDeleteImportDataError where
  hashWithSalt _salt BatchDeleteImportDataError' {..} =
    _salt `Prelude.hashWithSalt` importTaskId
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorDescription

instance Prelude.NFData BatchDeleteImportDataError where
  rnf BatchDeleteImportDataError' {..} =
    Prelude.rnf importTaskId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorDescription
