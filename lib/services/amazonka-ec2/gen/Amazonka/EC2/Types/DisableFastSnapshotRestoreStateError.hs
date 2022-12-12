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
-- Module      : Amazonka.EC2.Types.DisableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DisableFastSnapshotRestoreStateError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an error that occurred when disabling fast snapshot restores.
--
-- /See:/ 'newDisableFastSnapshotRestoreStateError' smart constructor.
data DisableFastSnapshotRestoreStateError = DisableFastSnapshotRestoreStateError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoreStateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'disableFastSnapshotRestoreStateError_code' - The error code.
--
-- 'message', 'disableFastSnapshotRestoreStateError_message' - The error message.
newDisableFastSnapshotRestoreStateError ::
  DisableFastSnapshotRestoreStateError
newDisableFastSnapshotRestoreStateError =
  DisableFastSnapshotRestoreStateError'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
disableFastSnapshotRestoreStateError_code :: Lens.Lens' DisableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreStateError_code = Lens.lens (\DisableFastSnapshotRestoreStateError' {code} -> code) (\s@DisableFastSnapshotRestoreStateError' {} a -> s {code = a} :: DisableFastSnapshotRestoreStateError)

-- | The error message.
disableFastSnapshotRestoreStateError_message :: Lens.Lens' DisableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreStateError_message = Lens.lens (\DisableFastSnapshotRestoreStateError' {message} -> message) (\s@DisableFastSnapshotRestoreStateError' {} a -> s {message = a} :: DisableFastSnapshotRestoreStateError)

instance
  Data.FromXML
    DisableFastSnapshotRestoreStateError
  where
  parseXML x =
    DisableFastSnapshotRestoreStateError'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    DisableFastSnapshotRestoreStateError
  where
  hashWithSalt
    _salt
    DisableFastSnapshotRestoreStateError' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    DisableFastSnapshotRestoreStateError
  where
  rnf DisableFastSnapshotRestoreStateError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
