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
-- Module      : Amazonka.AuditManager.Types.EvidenceFinderEnablement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.EvidenceFinderEnablement where

import Amazonka.AuditManager.Types.EvidenceFinderBackfillStatus
import Amazonka.AuditManager.Types.EvidenceFinderEnablementStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings object that specifies whether evidence finder is enabled.
-- This object also describes the related event data store, and the
-- backfill status for populating the event data store with evidence data.
--
-- /See:/ 'newEvidenceFinderEnablement' smart constructor.
data EvidenceFinderEnablement = EvidenceFinderEnablement'
  { -- | The current status of the evidence data backfill process.
    --
    -- The backfill starts after you enable evidence finder. During this task,
    -- Audit Manager populates an event data store with your past two years’
    -- worth of evidence data so that your evidence can be queried.
    --
    -- -   @NOT_STARTED@ means that the backfill hasn’t started yet.
    --
    -- -   @IN_PROGRESS@ means that the backfill is in progress. This can take
    --     up to 7 days to complete, depending on the amount of evidence data.
    --
    -- -   @COMPLETED@ means that the backfill is complete. All of your past
    --     evidence is now queryable.
    backfillStatus :: Prelude.Maybe EvidenceFinderBackfillStatus,
    -- | The current status of the evidence finder feature and the related event
    -- data store.
    --
    -- -   @ENABLE_IN_PROGRESS@ means that you requested to enable evidence
    --     finder. An event data store is currently being created to support
    --     evidence finder queries.
    --
    -- -   @ENABLED@ means that an event data store was successfully created
    --     and evidence finder is enabled. We recommend that you wait 7 days
    --     until the event data store is backfilled with your past two years’
    --     worth of evidence data. You can use evidence finder in the meantime,
    --     but not all data might be available until the backfill is complete.
    --
    -- -   @DISABLE_IN_PROGRESS@ means that you requested to disable evidence
    --     finder, and your request is pending the deletion of the event data
    --     store.
    --
    -- -   @DISABLED@ means that you have permanently disabled evidence finder
    --     and the event data store has been deleted. You can\'t re-enable
    --     evidence finder after this point.
    enablementStatus :: Prelude.Maybe EvidenceFinderEnablementStatus,
    -- | Represents any errors that occurred when enabling or disabling evidence
    -- finder.
    error :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CloudTrail Lake event data store
    -- that’s used by evidence finder. The event data store is the lake of
    -- evidence data that evidence finder runs queries against.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvidenceFinderEnablement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backfillStatus', 'evidenceFinderEnablement_backfillStatus' - The current status of the evidence data backfill process.
--
-- The backfill starts after you enable evidence finder. During this task,
-- Audit Manager populates an event data store with your past two years’
-- worth of evidence data so that your evidence can be queried.
--
-- -   @NOT_STARTED@ means that the backfill hasn’t started yet.
--
-- -   @IN_PROGRESS@ means that the backfill is in progress. This can take
--     up to 7 days to complete, depending on the amount of evidence data.
--
-- -   @COMPLETED@ means that the backfill is complete. All of your past
--     evidence is now queryable.
--
-- 'enablementStatus', 'evidenceFinderEnablement_enablementStatus' - The current status of the evidence finder feature and the related event
-- data store.
--
-- -   @ENABLE_IN_PROGRESS@ means that you requested to enable evidence
--     finder. An event data store is currently being created to support
--     evidence finder queries.
--
-- -   @ENABLED@ means that an event data store was successfully created
--     and evidence finder is enabled. We recommend that you wait 7 days
--     until the event data store is backfilled with your past two years’
--     worth of evidence data. You can use evidence finder in the meantime,
--     but not all data might be available until the backfill is complete.
--
-- -   @DISABLE_IN_PROGRESS@ means that you requested to disable evidence
--     finder, and your request is pending the deletion of the event data
--     store.
--
-- -   @DISABLED@ means that you have permanently disabled evidence finder
--     and the event data store has been deleted. You can\'t re-enable
--     evidence finder after this point.
--
-- 'error', 'evidenceFinderEnablement_error' - Represents any errors that occurred when enabling or disabling evidence
-- finder.
--
-- 'eventDataStoreArn', 'evidenceFinderEnablement_eventDataStoreArn' - The Amazon Resource Name (ARN) of the CloudTrail Lake event data store
-- that’s used by evidence finder. The event data store is the lake of
-- evidence data that evidence finder runs queries against.
newEvidenceFinderEnablement ::
  EvidenceFinderEnablement
newEvidenceFinderEnablement =
  EvidenceFinderEnablement'
    { backfillStatus =
        Prelude.Nothing,
      enablementStatus = Prelude.Nothing,
      error = Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing
    }

-- | The current status of the evidence data backfill process.
--
-- The backfill starts after you enable evidence finder. During this task,
-- Audit Manager populates an event data store with your past two years’
-- worth of evidence data so that your evidence can be queried.
--
-- -   @NOT_STARTED@ means that the backfill hasn’t started yet.
--
-- -   @IN_PROGRESS@ means that the backfill is in progress. This can take
--     up to 7 days to complete, depending on the amount of evidence data.
--
-- -   @COMPLETED@ means that the backfill is complete. All of your past
--     evidence is now queryable.
evidenceFinderEnablement_backfillStatus :: Lens.Lens' EvidenceFinderEnablement (Prelude.Maybe EvidenceFinderBackfillStatus)
evidenceFinderEnablement_backfillStatus = Lens.lens (\EvidenceFinderEnablement' {backfillStatus} -> backfillStatus) (\s@EvidenceFinderEnablement' {} a -> s {backfillStatus = a} :: EvidenceFinderEnablement)

-- | The current status of the evidence finder feature and the related event
-- data store.
--
-- -   @ENABLE_IN_PROGRESS@ means that you requested to enable evidence
--     finder. An event data store is currently being created to support
--     evidence finder queries.
--
-- -   @ENABLED@ means that an event data store was successfully created
--     and evidence finder is enabled. We recommend that you wait 7 days
--     until the event data store is backfilled with your past two years’
--     worth of evidence data. You can use evidence finder in the meantime,
--     but not all data might be available until the backfill is complete.
--
-- -   @DISABLE_IN_PROGRESS@ means that you requested to disable evidence
--     finder, and your request is pending the deletion of the event data
--     store.
--
-- -   @DISABLED@ means that you have permanently disabled evidence finder
--     and the event data store has been deleted. You can\'t re-enable
--     evidence finder after this point.
evidenceFinderEnablement_enablementStatus :: Lens.Lens' EvidenceFinderEnablement (Prelude.Maybe EvidenceFinderEnablementStatus)
evidenceFinderEnablement_enablementStatus = Lens.lens (\EvidenceFinderEnablement' {enablementStatus} -> enablementStatus) (\s@EvidenceFinderEnablement' {} a -> s {enablementStatus = a} :: EvidenceFinderEnablement)

-- | Represents any errors that occurred when enabling or disabling evidence
-- finder.
evidenceFinderEnablement_error :: Lens.Lens' EvidenceFinderEnablement (Prelude.Maybe Prelude.Text)
evidenceFinderEnablement_error = Lens.lens (\EvidenceFinderEnablement' {error} -> error) (\s@EvidenceFinderEnablement' {} a -> s {error = a} :: EvidenceFinderEnablement)

-- | The Amazon Resource Name (ARN) of the CloudTrail Lake event data store
-- that’s used by evidence finder. The event data store is the lake of
-- evidence data that evidence finder runs queries against.
evidenceFinderEnablement_eventDataStoreArn :: Lens.Lens' EvidenceFinderEnablement (Prelude.Maybe Prelude.Text)
evidenceFinderEnablement_eventDataStoreArn = Lens.lens (\EvidenceFinderEnablement' {eventDataStoreArn} -> eventDataStoreArn) (\s@EvidenceFinderEnablement' {} a -> s {eventDataStoreArn = a} :: EvidenceFinderEnablement)

instance Data.FromJSON EvidenceFinderEnablement where
  parseJSON =
    Data.withObject
      "EvidenceFinderEnablement"
      ( \x ->
          EvidenceFinderEnablement'
            Prelude.<$> (x Data..:? "backfillStatus")
            Prelude.<*> (x Data..:? "enablementStatus")
            Prelude.<*> (x Data..:? "error")
            Prelude.<*> (x Data..:? "eventDataStoreArn")
      )

instance Prelude.Hashable EvidenceFinderEnablement where
  hashWithSalt _salt EvidenceFinderEnablement' {..} =
    _salt
      `Prelude.hashWithSalt` backfillStatus
      `Prelude.hashWithSalt` enablementStatus
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` eventDataStoreArn

instance Prelude.NFData EvidenceFinderEnablement where
  rnf EvidenceFinderEnablement' {..} =
    Prelude.rnf backfillStatus `Prelude.seq`
      Prelude.rnf enablementStatus `Prelude.seq`
        Prelude.rnf error `Prelude.seq`
          Prelude.rnf eventDataStoreArn
