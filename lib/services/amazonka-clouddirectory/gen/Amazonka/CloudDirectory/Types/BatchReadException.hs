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
-- Module      : Amazonka.CloudDirectory.Types.BatchReadException
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchReadException where

import Amazonka.CloudDirectory.Types.BatchReadExceptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The batch read exception structure, which contains the exception type
-- and message.
--
-- /See:/ 'newBatchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { -- | A type of exception, such as @InvalidArnException@.
    type' :: Prelude.Maybe BatchReadExceptionType,
    -- | An exception message that is associated with the failure.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchReadException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'batchReadException_type' - A type of exception, such as @InvalidArnException@.
--
-- 'message', 'batchReadException_message' - An exception message that is associated with the failure.
newBatchReadException ::
  BatchReadException
newBatchReadException =
  BatchReadException'
    { type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | A type of exception, such as @InvalidArnException@.
batchReadException_type :: Lens.Lens' BatchReadException (Prelude.Maybe BatchReadExceptionType)
batchReadException_type = Lens.lens (\BatchReadException' {type'} -> type') (\s@BatchReadException' {} a -> s {type' = a} :: BatchReadException)

-- | An exception message that is associated with the failure.
batchReadException_message :: Lens.Lens' BatchReadException (Prelude.Maybe Prelude.Text)
batchReadException_message = Lens.lens (\BatchReadException' {message} -> message) (\s@BatchReadException' {} a -> s {message = a} :: BatchReadException)

instance Core.FromJSON BatchReadException where
  parseJSON =
    Core.withObject
      "BatchReadException"
      ( \x ->
          BatchReadException'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable BatchReadException where
  hashWithSalt salt' BatchReadException' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData BatchReadException where
  rnf BatchReadException' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf message
