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
-- Module      : Amazonka.CloudDirectory.Types.BatchReadOperationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchReadOperationResponse where

import Amazonka.CloudDirectory.Types.BatchReadException
import Amazonka.CloudDirectory.Types.BatchReadSuccessfulResponse
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @BatchRead@ response operation.
--
-- /See:/ 'newBatchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { -- | Identifies which operation in a batch has failed.
    exceptionResponse :: Prelude.Maybe BatchReadException,
    -- | Identifies which operation in a batch has succeeded.
    successfulResponse :: Prelude.Maybe BatchReadSuccessfulResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchReadOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionResponse', 'batchReadOperationResponse_exceptionResponse' - Identifies which operation in a batch has failed.
--
-- 'successfulResponse', 'batchReadOperationResponse_successfulResponse' - Identifies which operation in a batch has succeeded.
newBatchReadOperationResponse ::
  BatchReadOperationResponse
newBatchReadOperationResponse =
  BatchReadOperationResponse'
    { exceptionResponse =
        Prelude.Nothing,
      successfulResponse = Prelude.Nothing
    }

-- | Identifies which operation in a batch has failed.
batchReadOperationResponse_exceptionResponse :: Lens.Lens' BatchReadOperationResponse (Prelude.Maybe BatchReadException)
batchReadOperationResponse_exceptionResponse = Lens.lens (\BatchReadOperationResponse' {exceptionResponse} -> exceptionResponse) (\s@BatchReadOperationResponse' {} a -> s {exceptionResponse = a} :: BatchReadOperationResponse)

-- | Identifies which operation in a batch has succeeded.
batchReadOperationResponse_successfulResponse :: Lens.Lens' BatchReadOperationResponse (Prelude.Maybe BatchReadSuccessfulResponse)
batchReadOperationResponse_successfulResponse = Lens.lens (\BatchReadOperationResponse' {successfulResponse} -> successfulResponse) (\s@BatchReadOperationResponse' {} a -> s {successfulResponse = a} :: BatchReadOperationResponse)

instance Data.FromJSON BatchReadOperationResponse where
  parseJSON =
    Data.withObject
      "BatchReadOperationResponse"
      ( \x ->
          BatchReadOperationResponse'
            Prelude.<$> (x Data..:? "ExceptionResponse")
            Prelude.<*> (x Data..:? "SuccessfulResponse")
      )

instance Prelude.Hashable BatchReadOperationResponse where
  hashWithSalt _salt BatchReadOperationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` exceptionResponse
      `Prelude.hashWithSalt` successfulResponse

instance Prelude.NFData BatchReadOperationResponse where
  rnf BatchReadOperationResponse' {..} =
    Prelude.rnf exceptionResponse
      `Prelude.seq` Prelude.rnf successfulResponse
