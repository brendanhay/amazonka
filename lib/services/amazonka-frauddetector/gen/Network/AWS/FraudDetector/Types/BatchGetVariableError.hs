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
-- Module      : Network.AWS.FraudDetector.Types.BatchGetVariableError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.BatchGetVariableError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the error of the batch get variable API.
--
-- /See:/ 'newBatchGetVariableError' smart constructor.
data BatchGetVariableError = BatchGetVariableError'
  { -- | The error name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Int,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
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
-- 'name', 'batchGetVariableError_name' - The error name.
--
-- 'code', 'batchGetVariableError_code' - The error code.
--
-- 'message', 'batchGetVariableError_message' - The error message.
newBatchGetVariableError ::
  BatchGetVariableError
newBatchGetVariableError =
  BatchGetVariableError'
    { name = Prelude.Nothing,
      code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error name.
batchGetVariableError_name :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Text)
batchGetVariableError_name = Lens.lens (\BatchGetVariableError' {name} -> name) (\s@BatchGetVariableError' {} a -> s {name = a} :: BatchGetVariableError)

-- | The error code.
batchGetVariableError_code :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Int)
batchGetVariableError_code = Lens.lens (\BatchGetVariableError' {code} -> code) (\s@BatchGetVariableError' {} a -> s {code = a} :: BatchGetVariableError)

-- | The error message.
batchGetVariableError_message :: Lens.Lens' BatchGetVariableError (Prelude.Maybe Prelude.Text)
batchGetVariableError_message = Lens.lens (\BatchGetVariableError' {message} -> message) (\s@BatchGetVariableError' {} a -> s {message = a} :: BatchGetVariableError)

instance Core.FromJSON BatchGetVariableError where
  parseJSON =
    Core.withObject
      "BatchGetVariableError"
      ( \x ->
          BatchGetVariableError'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "code")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable BatchGetVariableError

instance Prelude.NFData BatchGetVariableError
