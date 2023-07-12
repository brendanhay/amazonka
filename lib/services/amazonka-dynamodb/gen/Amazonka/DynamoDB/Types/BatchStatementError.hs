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
-- Module      : Amazonka.DynamoDB.Types.BatchStatementError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BatchStatementError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BatchStatementErrorCodeEnum
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | An error associated with a statement in a PartiQL batch that was run.
--
-- /See:/ 'newBatchStatementError' smart constructor.
data BatchStatementError = BatchStatementError'
  { -- | The error code associated with the failed PartiQL batch statement.
    code :: Prelude.Maybe BatchStatementErrorCodeEnum,
    -- | The error message associated with the PartiQL batch response.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStatementError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'batchStatementError_code' - The error code associated with the failed PartiQL batch statement.
--
-- 'message', 'batchStatementError_message' - The error message associated with the PartiQL batch response.
newBatchStatementError ::
  BatchStatementError
newBatchStatementError =
  BatchStatementError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code associated with the failed PartiQL batch statement.
batchStatementError_code :: Lens.Lens' BatchStatementError (Prelude.Maybe BatchStatementErrorCodeEnum)
batchStatementError_code = Lens.lens (\BatchStatementError' {code} -> code) (\s@BatchStatementError' {} a -> s {code = a} :: BatchStatementError)

-- | The error message associated with the PartiQL batch response.
batchStatementError_message :: Lens.Lens' BatchStatementError (Prelude.Maybe Prelude.Text)
batchStatementError_message = Lens.lens (\BatchStatementError' {message} -> message) (\s@BatchStatementError' {} a -> s {message = a} :: BatchStatementError)

instance Data.FromJSON BatchStatementError where
  parseJSON =
    Data.withObject
      "BatchStatementError"
      ( \x ->
          BatchStatementError'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable BatchStatementError where
  hashWithSalt _salt BatchStatementError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData BatchStatementError where
  rnf BatchStatementError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
