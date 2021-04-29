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
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadException
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadException where

import Network.AWS.CloudDirectory.Types.BatchReadExceptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The batch read exception structure, which contains the exception type
-- and message.
--
-- /See:/ 'newBatchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { -- | An exception message that is associated with the failure.
    message :: Prelude.Maybe Prelude.Text,
    -- | A type of exception, such as @InvalidArnException@.
    type' :: Prelude.Maybe BatchReadExceptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchReadException' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchReadException_message' - An exception message that is associated with the failure.
--
-- 'type'', 'batchReadException_type' - A type of exception, such as @InvalidArnException@.
newBatchReadException ::
  BatchReadException
newBatchReadException =
  BatchReadException'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An exception message that is associated with the failure.
batchReadException_message :: Lens.Lens' BatchReadException (Prelude.Maybe Prelude.Text)
batchReadException_message = Lens.lens (\BatchReadException' {message} -> message) (\s@BatchReadException' {} a -> s {message = a} :: BatchReadException)

-- | A type of exception, such as @InvalidArnException@.
batchReadException_type :: Lens.Lens' BatchReadException (Prelude.Maybe BatchReadExceptionType)
batchReadException_type = Lens.lens (\BatchReadException' {type'} -> type') (\s@BatchReadException' {} a -> s {type' = a} :: BatchReadException)

instance Prelude.FromJSON BatchReadException where
  parseJSON =
    Prelude.withObject
      "BatchReadException"
      ( \x ->
          BatchReadException'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable BatchReadException

instance Prelude.NFData BatchReadException
