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
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreStateError' smart constructor.
data EnableFastSnapshotRestoreStateError = EnableFastSnapshotRestoreStateError'
  { -- | The error message.
    message :: Core.Maybe Core.Text,
    -- | The error code.
    code :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreStateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'enableFastSnapshotRestoreStateError_message' - The error message.
--
-- 'code', 'enableFastSnapshotRestoreStateError_code' - The error code.
newEnableFastSnapshotRestoreStateError ::
  EnableFastSnapshotRestoreStateError
newEnableFastSnapshotRestoreStateError =
  EnableFastSnapshotRestoreStateError'
    { message =
        Core.Nothing,
      code = Core.Nothing
    }

-- | The error message.
enableFastSnapshotRestoreStateError_message :: Lens.Lens' EnableFastSnapshotRestoreStateError (Core.Maybe Core.Text)
enableFastSnapshotRestoreStateError_message = Lens.lens (\EnableFastSnapshotRestoreStateError' {message} -> message) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {message = a} :: EnableFastSnapshotRestoreStateError)

-- | The error code.
enableFastSnapshotRestoreStateError_code :: Lens.Lens' EnableFastSnapshotRestoreStateError (Core.Maybe Core.Text)
enableFastSnapshotRestoreStateError_code = Lens.lens (\EnableFastSnapshotRestoreStateError' {code} -> code) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {code = a} :: EnableFastSnapshotRestoreStateError)

instance
  Core.FromXML
    EnableFastSnapshotRestoreStateError
  where
  parseXML x =
    EnableFastSnapshotRestoreStateError'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance
  Core.Hashable
    EnableFastSnapshotRestoreStateError

instance
  Core.NFData
    EnableFastSnapshotRestoreStateError
