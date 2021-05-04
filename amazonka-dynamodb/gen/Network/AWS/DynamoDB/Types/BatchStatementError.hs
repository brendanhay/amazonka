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
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementError where

import Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An error associated with a statement in a PartiQL batch that was run.
--
-- /See:/ 'newBatchStatementError' smart constructor.
data BatchStatementError = BatchStatementError'
  { -- | The error message associated with the PartiQL batch resposne.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code associated with the failed PartiQL batch statement.
    code :: Prelude.Maybe BatchStatementErrorCodeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchStatementError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchStatementError_message' - The error message associated with the PartiQL batch resposne.
--
-- 'code', 'batchStatementError_code' - The error code associated with the failed PartiQL batch statement.
newBatchStatementError ::
  BatchStatementError
newBatchStatementError =
  BatchStatementError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message associated with the PartiQL batch resposne.
batchStatementError_message :: Lens.Lens' BatchStatementError (Prelude.Maybe Prelude.Text)
batchStatementError_message = Lens.lens (\BatchStatementError' {message} -> message) (\s@BatchStatementError' {} a -> s {message = a} :: BatchStatementError)

-- | The error code associated with the failed PartiQL batch statement.
batchStatementError_code :: Lens.Lens' BatchStatementError (Prelude.Maybe BatchStatementErrorCodeEnum)
batchStatementError_code = Lens.lens (\BatchStatementError' {code} -> code) (\s@BatchStatementError' {} a -> s {code = a} :: BatchStatementError)

instance Prelude.FromJSON BatchStatementError where
  parseJSON =
    Prelude.withObject
      "BatchStatementError"
      ( \x ->
          BatchStatementError'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Code")
      )

instance Prelude.Hashable BatchStatementError

instance Prelude.NFData BatchStatementError
