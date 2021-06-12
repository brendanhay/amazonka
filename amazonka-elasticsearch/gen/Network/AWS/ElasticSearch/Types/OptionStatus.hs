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
-- Module      : Network.AWS.ElasticSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.OptionState
import qualified Network.AWS.Lens as Lens

-- | Provides the current status of the entity.
--
-- /See:/ 'newOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Specifies the latest version for the entity.
    updateVersion :: Core.Maybe Core.Natural,
    -- | Indicates whether the Elasticsearch domain is being deleted.
    pendingDeletion :: Core.Maybe Core.Bool,
    -- | Timestamp which tells the creation date for the entity.
    creationDate :: Core.POSIX,
    -- | Timestamp which tells the last updated time for the entity.
    updateDate :: Core.POSIX,
    -- | Provides the @OptionState@ for the Elasticsearch domain.
    state :: OptionState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OptionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateVersion', 'optionStatus_updateVersion' - Specifies the latest version for the entity.
--
-- 'pendingDeletion', 'optionStatus_pendingDeletion' - Indicates whether the Elasticsearch domain is being deleted.
--
-- 'creationDate', 'optionStatus_creationDate' - Timestamp which tells the creation date for the entity.
--
-- 'updateDate', 'optionStatus_updateDate' - Timestamp which tells the last updated time for the entity.
--
-- 'state', 'optionStatus_state' - Provides the @OptionState@ for the Elasticsearch domain.
newOptionStatus ::
  -- | 'creationDate'
  Core.UTCTime ->
  -- | 'updateDate'
  Core.UTCTime ->
  -- | 'state'
  OptionState ->
  OptionStatus
newOptionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { updateVersion = Core.Nothing,
      pendingDeletion = Core.Nothing,
      creationDate = Core._Time Lens.# pCreationDate_,
      updateDate = Core._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Specifies the latest version for the entity.
optionStatus_updateVersion :: Lens.Lens' OptionStatus (Core.Maybe Core.Natural)
optionStatus_updateVersion = Lens.lens (\OptionStatus' {updateVersion} -> updateVersion) (\s@OptionStatus' {} a -> s {updateVersion = a} :: OptionStatus)

-- | Indicates whether the Elasticsearch domain is being deleted.
optionStatus_pendingDeletion :: Lens.Lens' OptionStatus (Core.Maybe Core.Bool)
optionStatus_pendingDeletion = Lens.lens (\OptionStatus' {pendingDeletion} -> pendingDeletion) (\s@OptionStatus' {} a -> s {pendingDeletion = a} :: OptionStatus)

-- | Timestamp which tells the creation date for the entity.
optionStatus_creationDate :: Lens.Lens' OptionStatus Core.UTCTime
optionStatus_creationDate = Lens.lens (\OptionStatus' {creationDate} -> creationDate) (\s@OptionStatus' {} a -> s {creationDate = a} :: OptionStatus) Core.. Core._Time

-- | Timestamp which tells the last updated time for the entity.
optionStatus_updateDate :: Lens.Lens' OptionStatus Core.UTCTime
optionStatus_updateDate = Lens.lens (\OptionStatus' {updateDate} -> updateDate) (\s@OptionStatus' {} a -> s {updateDate = a} :: OptionStatus) Core.. Core._Time

-- | Provides the @OptionState@ for the Elasticsearch domain.
optionStatus_state :: Lens.Lens' OptionStatus OptionState
optionStatus_state = Lens.lens (\OptionStatus' {state} -> state) (\s@OptionStatus' {} a -> s {state = a} :: OptionStatus)

instance Core.FromJSON OptionStatus where
  parseJSON =
    Core.withObject
      "OptionStatus"
      ( \x ->
          OptionStatus'
            Core.<$> (x Core..:? "UpdateVersion")
            Core.<*> (x Core..:? "PendingDeletion")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "UpdateDate")
            Core.<*> (x Core..: "State")
      )

instance Core.Hashable OptionStatus

instance Core.NFData OptionStatus
