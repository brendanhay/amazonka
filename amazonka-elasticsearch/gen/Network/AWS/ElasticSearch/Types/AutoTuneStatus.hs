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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.AutoTuneState
import qualified Network.AWS.Lens as Lens

-- | Provides the current status of the Auto-Tune options.
--
-- /See:/ 'newAutoTuneStatus' smart constructor.
data AutoTuneStatus = AutoTuneStatus'
  { -- | Specifies the Auto-Tune options latest version.
    updateVersion :: Core.Maybe Core.Natural,
    -- | Specifies the error message while enabling or disabling the Auto-Tune
    -- options.
    errorMessage :: Core.Maybe Core.Text,
    -- | Indicates whether the Elasticsearch domain is being deleted.
    pendingDeletion :: Core.Maybe Core.Bool,
    -- | Timestamp which tells Auto-Tune options creation date .
    creationDate :: Core.POSIX,
    -- | Timestamp which tells Auto-Tune options last updated time.
    updateDate :: Core.POSIX,
    -- | Specifies the @AutoTuneState@ for the Elasticsearch domain.
    state :: AutoTuneState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoTuneStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateVersion', 'autoTuneStatus_updateVersion' - Specifies the Auto-Tune options latest version.
--
-- 'errorMessage', 'autoTuneStatus_errorMessage' - Specifies the error message while enabling or disabling the Auto-Tune
-- options.
--
-- 'pendingDeletion', 'autoTuneStatus_pendingDeletion' - Indicates whether the Elasticsearch domain is being deleted.
--
-- 'creationDate', 'autoTuneStatus_creationDate' - Timestamp which tells Auto-Tune options creation date .
--
-- 'updateDate', 'autoTuneStatus_updateDate' - Timestamp which tells Auto-Tune options last updated time.
--
-- 'state', 'autoTuneStatus_state' - Specifies the @AutoTuneState@ for the Elasticsearch domain.
newAutoTuneStatus ::
  -- | 'creationDate'
  Core.UTCTime ->
  -- | 'updateDate'
  Core.UTCTime ->
  -- | 'state'
  AutoTuneState ->
  AutoTuneStatus
newAutoTuneStatus pCreationDate_ pUpdateDate_ pState_ =
  AutoTuneStatus'
    { updateVersion = Core.Nothing,
      errorMessage = Core.Nothing,
      pendingDeletion = Core.Nothing,
      creationDate = Core._Time Lens.# pCreationDate_,
      updateDate = Core._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Specifies the Auto-Tune options latest version.
autoTuneStatus_updateVersion :: Lens.Lens' AutoTuneStatus (Core.Maybe Core.Natural)
autoTuneStatus_updateVersion = Lens.lens (\AutoTuneStatus' {updateVersion} -> updateVersion) (\s@AutoTuneStatus' {} a -> s {updateVersion = a} :: AutoTuneStatus)

-- | Specifies the error message while enabling or disabling the Auto-Tune
-- options.
autoTuneStatus_errorMessage :: Lens.Lens' AutoTuneStatus (Core.Maybe Core.Text)
autoTuneStatus_errorMessage = Lens.lens (\AutoTuneStatus' {errorMessage} -> errorMessage) (\s@AutoTuneStatus' {} a -> s {errorMessage = a} :: AutoTuneStatus)

-- | Indicates whether the Elasticsearch domain is being deleted.
autoTuneStatus_pendingDeletion :: Lens.Lens' AutoTuneStatus (Core.Maybe Core.Bool)
autoTuneStatus_pendingDeletion = Lens.lens (\AutoTuneStatus' {pendingDeletion} -> pendingDeletion) (\s@AutoTuneStatus' {} a -> s {pendingDeletion = a} :: AutoTuneStatus)

-- | Timestamp which tells Auto-Tune options creation date .
autoTuneStatus_creationDate :: Lens.Lens' AutoTuneStatus Core.UTCTime
autoTuneStatus_creationDate = Lens.lens (\AutoTuneStatus' {creationDate} -> creationDate) (\s@AutoTuneStatus' {} a -> s {creationDate = a} :: AutoTuneStatus) Core.. Core._Time

-- | Timestamp which tells Auto-Tune options last updated time.
autoTuneStatus_updateDate :: Lens.Lens' AutoTuneStatus Core.UTCTime
autoTuneStatus_updateDate = Lens.lens (\AutoTuneStatus' {updateDate} -> updateDate) (\s@AutoTuneStatus' {} a -> s {updateDate = a} :: AutoTuneStatus) Core.. Core._Time

-- | Specifies the @AutoTuneState@ for the Elasticsearch domain.
autoTuneStatus_state :: Lens.Lens' AutoTuneStatus AutoTuneState
autoTuneStatus_state = Lens.lens (\AutoTuneStatus' {state} -> state) (\s@AutoTuneStatus' {} a -> s {state = a} :: AutoTuneStatus)

instance Core.FromJSON AutoTuneStatus where
  parseJSON =
    Core.withObject
      "AutoTuneStatus"
      ( \x ->
          AutoTuneStatus'
            Core.<$> (x Core..:? "UpdateVersion")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "PendingDeletion")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "UpdateDate")
            Core.<*> (x Core..: "State")
      )

instance Core.Hashable AutoTuneStatus

instance Core.NFData AutoTuneStatus
