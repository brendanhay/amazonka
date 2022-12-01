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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceDataReplicationError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceDataReplicationError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.FailbackReplicationError
import qualified Amazonka.Prelude as Prelude

-- | Error in data replication.
--
-- /See:/ 'newRecoveryInstanceDataReplicationError' smart constructor.
data RecoveryInstanceDataReplicationError = RecoveryInstanceDataReplicationError'
  { -- | Error in data replication.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | Error in data replication.
    error :: Prelude.Maybe FailbackReplicationError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceDataReplicationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawError', 'recoveryInstanceDataReplicationError_rawError' - Error in data replication.
--
-- 'error', 'recoveryInstanceDataReplicationError_error' - Error in data replication.
newRecoveryInstanceDataReplicationError ::
  RecoveryInstanceDataReplicationError
newRecoveryInstanceDataReplicationError =
  RecoveryInstanceDataReplicationError'
    { rawError =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | Error in data replication.
recoveryInstanceDataReplicationError_rawError :: Lens.Lens' RecoveryInstanceDataReplicationError (Prelude.Maybe Prelude.Text)
recoveryInstanceDataReplicationError_rawError = Lens.lens (\RecoveryInstanceDataReplicationError' {rawError} -> rawError) (\s@RecoveryInstanceDataReplicationError' {} a -> s {rawError = a} :: RecoveryInstanceDataReplicationError)

-- | Error in data replication.
recoveryInstanceDataReplicationError_error :: Lens.Lens' RecoveryInstanceDataReplicationError (Prelude.Maybe FailbackReplicationError)
recoveryInstanceDataReplicationError_error = Lens.lens (\RecoveryInstanceDataReplicationError' {error} -> error) (\s@RecoveryInstanceDataReplicationError' {} a -> s {error = a} :: RecoveryInstanceDataReplicationError)

instance
  Core.FromJSON
    RecoveryInstanceDataReplicationError
  where
  parseJSON =
    Core.withObject
      "RecoveryInstanceDataReplicationError"
      ( \x ->
          RecoveryInstanceDataReplicationError'
            Prelude.<$> (x Core..:? "rawError")
            Prelude.<*> (x Core..:? "error")
      )

instance
  Prelude.Hashable
    RecoveryInstanceDataReplicationError
  where
  hashWithSalt
    _salt
    RecoveryInstanceDataReplicationError' {..} =
      _salt `Prelude.hashWithSalt` rawError
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    RecoveryInstanceDataReplicationError
  where
  rnf RecoveryInstanceDataReplicationError' {..} =
    Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf error
