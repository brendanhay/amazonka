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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AutoTuneState
import qualified Amazonka.Prelude as Prelude

-- | The current status of Auto-Tune for the domain. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
--
-- /See:/ 'newAutoTuneStatus' smart constructor.
data AutoTuneStatus = AutoTuneStatus'
  { -- | Any errors that occurred while enabling or disabling Auto-Tune.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the domain is being deleted.
    pendingDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The latest version of the Auto-Tune options.
    updateVersion :: Prelude.Maybe Prelude.Natural,
    -- | Date and time when Auto-Tune was enabled for the domain.
    creationDate :: Data.POSIX,
    -- | Date and time when the Auto-Tune options were last updated for the
    -- domain.
    updateDate :: Data.POSIX,
    -- | The current state of Auto-Tune on the domain.
    state :: AutoTuneState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'autoTuneStatus_errorMessage' - Any errors that occurred while enabling or disabling Auto-Tune.
--
-- 'pendingDeletion', 'autoTuneStatus_pendingDeletion' - Indicates whether the domain is being deleted.
--
-- 'updateVersion', 'autoTuneStatus_updateVersion' - The latest version of the Auto-Tune options.
--
-- 'creationDate', 'autoTuneStatus_creationDate' - Date and time when Auto-Tune was enabled for the domain.
--
-- 'updateDate', 'autoTuneStatus_updateDate' - Date and time when the Auto-Tune options were last updated for the
-- domain.
--
-- 'state', 'autoTuneStatus_state' - The current state of Auto-Tune on the domain.
newAutoTuneStatus ::
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  -- | 'state'
  AutoTuneState ->
  AutoTuneStatus
newAutoTuneStatus pCreationDate_ pUpdateDate_ pState_ =
  AutoTuneStatus'
    { errorMessage = Prelude.Nothing,
      pendingDeletion = Prelude.Nothing,
      updateVersion = Prelude.Nothing,
      creationDate = Data._Time Lens.# pCreationDate_,
      updateDate = Data._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Any errors that occurred while enabling or disabling Auto-Tune.
autoTuneStatus_errorMessage :: Lens.Lens' AutoTuneStatus (Prelude.Maybe Prelude.Text)
autoTuneStatus_errorMessage = Lens.lens (\AutoTuneStatus' {errorMessage} -> errorMessage) (\s@AutoTuneStatus' {} a -> s {errorMessage = a} :: AutoTuneStatus)

-- | Indicates whether the domain is being deleted.
autoTuneStatus_pendingDeletion :: Lens.Lens' AutoTuneStatus (Prelude.Maybe Prelude.Bool)
autoTuneStatus_pendingDeletion = Lens.lens (\AutoTuneStatus' {pendingDeletion} -> pendingDeletion) (\s@AutoTuneStatus' {} a -> s {pendingDeletion = a} :: AutoTuneStatus)

-- | The latest version of the Auto-Tune options.
autoTuneStatus_updateVersion :: Lens.Lens' AutoTuneStatus (Prelude.Maybe Prelude.Natural)
autoTuneStatus_updateVersion = Lens.lens (\AutoTuneStatus' {updateVersion} -> updateVersion) (\s@AutoTuneStatus' {} a -> s {updateVersion = a} :: AutoTuneStatus)

-- | Date and time when Auto-Tune was enabled for the domain.
autoTuneStatus_creationDate :: Lens.Lens' AutoTuneStatus Prelude.UTCTime
autoTuneStatus_creationDate = Lens.lens (\AutoTuneStatus' {creationDate} -> creationDate) (\s@AutoTuneStatus' {} a -> s {creationDate = a} :: AutoTuneStatus) Prelude.. Data._Time

-- | Date and time when the Auto-Tune options were last updated for the
-- domain.
autoTuneStatus_updateDate :: Lens.Lens' AutoTuneStatus Prelude.UTCTime
autoTuneStatus_updateDate = Lens.lens (\AutoTuneStatus' {updateDate} -> updateDate) (\s@AutoTuneStatus' {} a -> s {updateDate = a} :: AutoTuneStatus) Prelude.. Data._Time

-- | The current state of Auto-Tune on the domain.
autoTuneStatus_state :: Lens.Lens' AutoTuneStatus AutoTuneState
autoTuneStatus_state = Lens.lens (\AutoTuneStatus' {state} -> state) (\s@AutoTuneStatus' {} a -> s {state = a} :: AutoTuneStatus)

instance Data.FromJSON AutoTuneStatus where
  parseJSON =
    Data.withObject
      "AutoTuneStatus"
      ( \x ->
          AutoTuneStatus'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "PendingDeletion")
            Prelude.<*> (x Data..:? "UpdateVersion")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "UpdateDate")
            Prelude.<*> (x Data..: "State")
      )

instance Prelude.Hashable AutoTuneStatus where
  hashWithSalt _salt AutoTuneStatus' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` pendingDeletion
      `Prelude.hashWithSalt` updateVersion
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` state

instance Prelude.NFData AutoTuneStatus where
  rnf AutoTuneStatus' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf pendingDeletion
      `Prelude.seq` Prelude.rnf updateVersion
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf state
