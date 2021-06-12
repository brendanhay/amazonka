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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The batch read exception structure, which contains the exception type
-- and message.
--
-- /See:/ 'newBatchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { -- | An exception message that is associated with the failure.
    message :: Core.Maybe Core.Text,
    -- | A type of exception, such as @InvalidArnException@.
    type' :: Core.Maybe BatchReadExceptionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { message = Core.Nothing,
      type' = Core.Nothing
    }

-- | An exception message that is associated with the failure.
batchReadException_message :: Lens.Lens' BatchReadException (Core.Maybe Core.Text)
batchReadException_message = Lens.lens (\BatchReadException' {message} -> message) (\s@BatchReadException' {} a -> s {message = a} :: BatchReadException)

-- | A type of exception, such as @InvalidArnException@.
batchReadException_type :: Lens.Lens' BatchReadException (Core.Maybe BatchReadExceptionType)
batchReadException_type = Lens.lens (\BatchReadException' {type'} -> type') (\s@BatchReadException' {} a -> s {type' = a} :: BatchReadException)

instance Core.FromJSON BatchReadException where
  parseJSON =
    Core.withObject
      "BatchReadException"
      ( \x ->
          BatchReadException'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable BatchReadException

instance Core.NFData BatchReadException
