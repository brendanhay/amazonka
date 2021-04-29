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
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreStateError' smart constructor.
data EnableFastSnapshotRestoreStateError = EnableFastSnapshotRestoreStateError'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message.
enableFastSnapshotRestoreStateError_message :: Lens.Lens' EnableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateError_message = Lens.lens (\EnableFastSnapshotRestoreStateError' {message} -> message) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {message = a} :: EnableFastSnapshotRestoreStateError)

-- | The error code.
enableFastSnapshotRestoreStateError_code :: Lens.Lens' EnableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateError_code = Lens.lens (\EnableFastSnapshotRestoreStateError' {code} -> code) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {code = a} :: EnableFastSnapshotRestoreStateError)

instance
  Prelude.FromXML
    EnableFastSnapshotRestoreStateError
  where
  parseXML x =
    EnableFastSnapshotRestoreStateError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreStateError

instance
  Prelude.NFData
    EnableFastSnapshotRestoreStateError
