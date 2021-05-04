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
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperationResponse where

import Network.AWS.CloudDirectory.Types.BatchReadException
import Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @BatchRead@ response operation.
--
-- /See:/ 'newBatchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { -- | Identifies which operation in a batch has succeeded.
    successfulResponse :: Prelude.Maybe BatchReadSuccessfulResponse,
    -- | Identifies which operation in a batch has failed.
    exceptionResponse :: Prelude.Maybe BatchReadException
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchReadOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulResponse', 'batchReadOperationResponse_successfulResponse' - Identifies which operation in a batch has succeeded.
--
-- 'exceptionResponse', 'batchReadOperationResponse_exceptionResponse' - Identifies which operation in a batch has failed.
newBatchReadOperationResponse ::
  BatchReadOperationResponse
newBatchReadOperationResponse =
  BatchReadOperationResponse'
    { successfulResponse =
        Prelude.Nothing,
      exceptionResponse = Prelude.Nothing
    }

-- | Identifies which operation in a batch has succeeded.
batchReadOperationResponse_successfulResponse :: Lens.Lens' BatchReadOperationResponse (Prelude.Maybe BatchReadSuccessfulResponse)
batchReadOperationResponse_successfulResponse = Lens.lens (\BatchReadOperationResponse' {successfulResponse} -> successfulResponse) (\s@BatchReadOperationResponse' {} a -> s {successfulResponse = a} :: BatchReadOperationResponse)

-- | Identifies which operation in a batch has failed.
batchReadOperationResponse_exceptionResponse :: Lens.Lens' BatchReadOperationResponse (Prelude.Maybe BatchReadException)
batchReadOperationResponse_exceptionResponse = Lens.lens (\BatchReadOperationResponse' {exceptionResponse} -> exceptionResponse) (\s@BatchReadOperationResponse' {} a -> s {exceptionResponse = a} :: BatchReadOperationResponse)

instance Prelude.FromJSON BatchReadOperationResponse where
  parseJSON =
    Prelude.withObject
      "BatchReadOperationResponse"
      ( \x ->
          BatchReadOperationResponse'
            Prelude.<$> (x Prelude..:? "SuccessfulResponse")
            Prelude.<*> (x Prelude..:? "ExceptionResponse")
      )

instance Prelude.Hashable BatchReadOperationResponse

instance Prelude.NFData BatchReadOperationResponse
