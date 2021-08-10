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
import qualified Network.AWS.Prelude as Prelude

-- | Provides the current status of the entity.
--
-- /See:/ 'newOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Specifies the latest version for the entity.
    updateVersion :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the Elasticsearch domain is being deleted.
    pendingDeletion :: Prelude.Maybe Prelude.Bool,
    -- | Timestamp which tells the creation date for the entity.
    creationDate :: Core.POSIX,
    -- | Timestamp which tells the last updated time for the entity.
    updateDate :: Core.POSIX,
    -- | Provides the @OptionState@ for the Elasticsearch domain.
    state :: OptionState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.UTCTime ->
  -- | 'updateDate'
  Prelude.UTCTime ->
  -- | 'state'
  OptionState ->
  OptionStatus
newOptionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { updateVersion = Prelude.Nothing,
      pendingDeletion = Prelude.Nothing,
      creationDate = Core._Time Lens.# pCreationDate_,
      updateDate = Core._Time Lens.# pUpdateDate_,
      state = pState_
    }

-- | Specifies the latest version for the entity.
optionStatus_updateVersion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Natural)
optionStatus_updateVersion = Lens.lens (\OptionStatus' {updateVersion} -> updateVersion) (\s@OptionStatus' {} a -> s {updateVersion = a} :: OptionStatus)

-- | Indicates whether the Elasticsearch domain is being deleted.
optionStatus_pendingDeletion :: Lens.Lens' OptionStatus (Prelude.Maybe Prelude.Bool)
optionStatus_pendingDeletion = Lens.lens (\OptionStatus' {pendingDeletion} -> pendingDeletion) (\s@OptionStatus' {} a -> s {pendingDeletion = a} :: OptionStatus)

-- | Timestamp which tells the creation date for the entity.
optionStatus_creationDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_creationDate = Lens.lens (\OptionStatus' {creationDate} -> creationDate) (\s@OptionStatus' {} a -> s {creationDate = a} :: OptionStatus) Prelude.. Core._Time

-- | Timestamp which tells the last updated time for the entity.
optionStatus_updateDate :: Lens.Lens' OptionStatus Prelude.UTCTime
optionStatus_updateDate = Lens.lens (\OptionStatus' {updateDate} -> updateDate) (\s@OptionStatus' {} a -> s {updateDate = a} :: OptionStatus) Prelude.. Core._Time

-- | Provides the @OptionState@ for the Elasticsearch domain.
optionStatus_state :: Lens.Lens' OptionStatus OptionState
optionStatus_state = Lens.lens (\OptionStatus' {state} -> state) (\s@OptionStatus' {} a -> s {state = a} :: OptionStatus)

instance Core.FromJSON OptionStatus where
  parseJSON =
    Core.withObject
      "OptionStatus"
      ( \x ->
          OptionStatus'
            Prelude.<$> (x Core..:? "UpdateVersion")
            Prelude.<*> (x Core..:? "PendingDeletion")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "UpdateDate")
            Prelude.<*> (x Core..: "State")
      )

instance Prelude.Hashable OptionStatus

instance Prelude.NFData OptionStatus
