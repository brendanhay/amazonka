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
-- Module      : Amazonka.DrS.Types.RecoveryLifeCycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryLifeCycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.RecoveryResult
import qualified Amazonka.Prelude as Prelude

-- | An object representing the Source Network recovery Lifecycle.
--
-- /See:/ 'newRecoveryLifeCycle' smart constructor.
data RecoveryLifeCycle = RecoveryLifeCycle'
  { -- | The date and time the last Source Network recovery was initiated.
    apiCallDateTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the Job that was used to last recover the Source Network.
    jobID :: Prelude.Maybe Prelude.Text,
    -- | The status of the last recovery status of this Source Network.
    lastRecoveryResult :: Prelude.Maybe RecoveryResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryLifeCycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDateTime', 'recoveryLifeCycle_apiCallDateTime' - The date and time the last Source Network recovery was initiated.
--
-- 'jobID', 'recoveryLifeCycle_jobID' - The ID of the Job that was used to last recover the Source Network.
--
-- 'lastRecoveryResult', 'recoveryLifeCycle_lastRecoveryResult' - The status of the last recovery status of this Source Network.
newRecoveryLifeCycle ::
  RecoveryLifeCycle
newRecoveryLifeCycle =
  RecoveryLifeCycle'
    { apiCallDateTime =
        Prelude.Nothing,
      jobID = Prelude.Nothing,
      lastRecoveryResult = Prelude.Nothing
    }

-- | The date and time the last Source Network recovery was initiated.
recoveryLifeCycle_apiCallDateTime :: Lens.Lens' RecoveryLifeCycle (Prelude.Maybe Prelude.UTCTime)
recoveryLifeCycle_apiCallDateTime = Lens.lens (\RecoveryLifeCycle' {apiCallDateTime} -> apiCallDateTime) (\s@RecoveryLifeCycle' {} a -> s {apiCallDateTime = a} :: RecoveryLifeCycle) Prelude.. Lens.mapping Data._Time

-- | The ID of the Job that was used to last recover the Source Network.
recoveryLifeCycle_jobID :: Lens.Lens' RecoveryLifeCycle (Prelude.Maybe Prelude.Text)
recoveryLifeCycle_jobID = Lens.lens (\RecoveryLifeCycle' {jobID} -> jobID) (\s@RecoveryLifeCycle' {} a -> s {jobID = a} :: RecoveryLifeCycle)

-- | The status of the last recovery status of this Source Network.
recoveryLifeCycle_lastRecoveryResult :: Lens.Lens' RecoveryLifeCycle (Prelude.Maybe RecoveryResult)
recoveryLifeCycle_lastRecoveryResult = Lens.lens (\RecoveryLifeCycle' {lastRecoveryResult} -> lastRecoveryResult) (\s@RecoveryLifeCycle' {} a -> s {lastRecoveryResult = a} :: RecoveryLifeCycle)

instance Data.FromJSON RecoveryLifeCycle where
  parseJSON =
    Data.withObject
      "RecoveryLifeCycle"
      ( \x ->
          RecoveryLifeCycle'
            Prelude.<$> (x Data..:? "apiCallDateTime")
            Prelude.<*> (x Data..:? "jobID")
            Prelude.<*> (x Data..:? "lastRecoveryResult")
      )

instance Prelude.Hashable RecoveryLifeCycle where
  hashWithSalt _salt RecoveryLifeCycle' {..} =
    _salt
      `Prelude.hashWithSalt` apiCallDateTime
      `Prelude.hashWithSalt` jobID
      `Prelude.hashWithSalt` lastRecoveryResult

instance Prelude.NFData RecoveryLifeCycle where
  rnf RecoveryLifeCycle' {..} =
    Prelude.rnf apiCallDateTime
      `Prelude.seq` Prelude.rnf jobID
      `Prelude.seq` Prelude.rnf lastRecoveryResult
