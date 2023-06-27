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
-- Module      : Amazonka.FraudDetector.Types.BatchGetVariableError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.BatchGetVariableError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the error of the batch get variable API.
--
-- /See:/ 'newBatchGetVariableError' smart constructor.
data BatchGetVariableError = BatchGetVariableError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Int,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetVariableError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'batchGetVariableError_code' - The error code.
--
-- 'message', 'batchGetVariableError_message' - The error message.
--
-- 'name', 'batchGetVariableError_name' - The error name.
newBatchGetVariableError ::
  BatchGetVariableError
newBatchGetVariableError =
  BatchGetVariableError'
    { code = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The error code.
batchGetVariableError_code :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Int)
batchGetVariableError_code = Lens.lens (\BatchGetVariableError' {code} -> code) (\s@BatchGetVariableError' {} a -> s {code = a} :: BatchGetVariableError)

-- | The error message.
batchGetVariableError_message :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Text)
batchGetVariableError_message = Lens.lens (\BatchGetVariableError' {message} -> message) (\s@BatchGetVariableError' {} a -> s {message = a} :: BatchGetVariableError)

-- | The error name.
batchGetVariableError_name :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Text)
batchGetVariableError_name = Lens.lens (\BatchGetVariableError' {name} -> name) (\s@BatchGetVariableError' {} a -> s {name = a} :: BatchGetVariableError)

instance Data.FromJSON BatchGetVariableError where
  parseJSON =
    Data.withObject
      "BatchGetVariableError"
      ( \x ->
          BatchGetVariableError'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable BatchGetVariableError where
  hashWithSalt _salt BatchGetVariableError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name

instance Prelude.NFData BatchGetVariableError where
  rnf BatchGetVariableError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
