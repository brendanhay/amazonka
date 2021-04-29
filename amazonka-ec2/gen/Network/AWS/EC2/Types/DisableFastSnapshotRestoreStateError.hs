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
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an error that occurred when disabling fast snapshot restores.
--
-- /See:/ 'newDisableFastSnapshotRestoreStateError' smart constructor.
data DisableFastSnapshotRestoreStateError = DisableFastSnapshotRestoreStateError'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoreStateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'disableFastSnapshotRestoreStateError_message' - The error message.
--
-- 'code', 'disableFastSnapshotRestoreStateError_code' - The error code.
newDisableFastSnapshotRestoreStateError ::
  DisableFastSnapshotRestoreStateError
newDisableFastSnapshotRestoreStateError =
  DisableFastSnapshotRestoreStateError'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The error message.
disableFastSnapshotRestoreStateError_message :: Lens.Lens' DisableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreStateError_message = Lens.lens (\DisableFastSnapshotRestoreStateError' {message} -> message) (\s@DisableFastSnapshotRestoreStateError' {} a -> s {message = a} :: DisableFastSnapshotRestoreStateError)

-- | The error code.
disableFastSnapshotRestoreStateError_code :: Lens.Lens' DisableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreStateError_code = Lens.lens (\DisableFastSnapshotRestoreStateError' {code} -> code) (\s@DisableFastSnapshotRestoreStateError' {} a -> s {code = a} :: DisableFastSnapshotRestoreStateError)

instance
  Prelude.FromXML
    DisableFastSnapshotRestoreStateError
  where
  parseXML x =
    DisableFastSnapshotRestoreStateError'
      Prelude.<$> (x Prelude..@? "message")
      Prelude.<*> (x Prelude..@? "code")

instance
  Prelude.Hashable
    DisableFastSnapshotRestoreStateError

instance
  Prelude.NFData
    DisableFastSnapshotRestoreStateError
