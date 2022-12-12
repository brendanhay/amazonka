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
-- Module      : Amazonka.CodeCommit.Types.PullRequestEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PullRequestEvent where

import Amazonka.CodeCommit.Types.ApprovalRuleEventMetadata
import Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Amazonka.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestCreatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestEventType
import Amazonka.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a pull request event.
--
-- /See:/ 'newPullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { -- | The Amazon Resource Name (ARN) of the user whose actions resulted in the
    -- event. Examples include updating the pull request with more commits or
    -- changing the status of a pull request.
    actorArn :: Prelude.Maybe Prelude.Text,
    -- | Information about a pull request event.
    approvalRuleEventMetadata :: Prelude.Maybe ApprovalRuleEventMetadata,
    -- | Information about an approval rule override event for a pull request.
    approvalRuleOverriddenEventMetadata :: Prelude.Maybe ApprovalRuleOverriddenEventMetadata,
    -- | Information about an approval state change for a pull request.
    approvalStateChangedEventMetadata :: Prelude.Maybe ApprovalStateChangedEventMetadata,
    -- | The day and time of the pull request event, in timestamp format.
    eventDate :: Prelude.Maybe Data.POSIX,
    -- | Information about the source and destination branches for the pull
    -- request.
    pullRequestCreatedEventMetadata :: Prelude.Maybe PullRequestCreatedEventMetadata,
    -- | The type of the pull request event (for example, a status change event
    -- (PULL_REQUEST_STATUS_CHANGED) or update event
    -- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
    pullRequestEventType :: Prelude.Maybe PullRequestEventType,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the change in mergability state for the pull request
    -- event.
    pullRequestMergedStateChangedEventMetadata :: Prelude.Maybe PullRequestMergedStateChangedEventMetadata,
    -- | Information about the updated source branch for the pull request event.
    pullRequestSourceReferenceUpdatedEventMetadata :: Prelude.Maybe PullRequestSourceReferenceUpdatedEventMetadata,
    -- | Information about the change in status for the pull request event.
    pullRequestStatusChangedEventMetadata :: Prelude.Maybe PullRequestStatusChangedEventMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PullRequestEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actorArn', 'pullRequestEvent_actorArn' - The Amazon Resource Name (ARN) of the user whose actions resulted in the
-- event. Examples include updating the pull request with more commits or
-- changing the status of a pull request.
--
-- 'approvalRuleEventMetadata', 'pullRequestEvent_approvalRuleEventMetadata' - Information about a pull request event.
--
-- 'approvalRuleOverriddenEventMetadata', 'pullRequestEvent_approvalRuleOverriddenEventMetadata' - Information about an approval rule override event for a pull request.
--
-- 'approvalStateChangedEventMetadata', 'pullRequestEvent_approvalStateChangedEventMetadata' - Information about an approval state change for a pull request.
--
-- 'eventDate', 'pullRequestEvent_eventDate' - The day and time of the pull request event, in timestamp format.
--
-- 'pullRequestCreatedEventMetadata', 'pullRequestEvent_pullRequestCreatedEventMetadata' - Information about the source and destination branches for the pull
-- request.
--
-- 'pullRequestEventType', 'pullRequestEvent_pullRequestEventType' - The type of the pull request event (for example, a status change event
-- (PULL_REQUEST_STATUS_CHANGED) or update event
-- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
--
-- 'pullRequestId', 'pullRequestEvent_pullRequestId' - The system-generated ID of the pull request.
--
-- 'pullRequestMergedStateChangedEventMetadata', 'pullRequestEvent_pullRequestMergedStateChangedEventMetadata' - Information about the change in mergability state for the pull request
-- event.
--
-- 'pullRequestSourceReferenceUpdatedEventMetadata', 'pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata' - Information about the updated source branch for the pull request event.
--
-- 'pullRequestStatusChangedEventMetadata', 'pullRequestEvent_pullRequestStatusChangedEventMetadata' - Information about the change in status for the pull request event.
newPullRequestEvent ::
  PullRequestEvent
newPullRequestEvent =
  PullRequestEvent'
    { actorArn = Prelude.Nothing,
      approvalRuleEventMetadata = Prelude.Nothing,
      approvalRuleOverriddenEventMetadata =
        Prelude.Nothing,
      approvalStateChangedEventMetadata = Prelude.Nothing,
      eventDate = Prelude.Nothing,
      pullRequestCreatedEventMetadata = Prelude.Nothing,
      pullRequestEventType = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      pullRequestMergedStateChangedEventMetadata =
        Prelude.Nothing,
      pullRequestSourceReferenceUpdatedEventMetadata =
        Prelude.Nothing,
      pullRequestStatusChangedEventMetadata =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the
-- event. Examples include updating the pull request with more commits or
-- changing the status of a pull request.
pullRequestEvent_actorArn :: Lens.Lens' PullRequestEvent (Prelude.Maybe Prelude.Text)
pullRequestEvent_actorArn = Lens.lens (\PullRequestEvent' {actorArn} -> actorArn) (\s@PullRequestEvent' {} a -> s {actorArn = a} :: PullRequestEvent)

-- | Information about a pull request event.
pullRequestEvent_approvalRuleEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe ApprovalRuleEventMetadata)
pullRequestEvent_approvalRuleEventMetadata = Lens.lens (\PullRequestEvent' {approvalRuleEventMetadata} -> approvalRuleEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalRuleEventMetadata = a} :: PullRequestEvent)

-- | Information about an approval rule override event for a pull request.
pullRequestEvent_approvalRuleOverriddenEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe ApprovalRuleOverriddenEventMetadata)
pullRequestEvent_approvalRuleOverriddenEventMetadata = Lens.lens (\PullRequestEvent' {approvalRuleOverriddenEventMetadata} -> approvalRuleOverriddenEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalRuleOverriddenEventMetadata = a} :: PullRequestEvent)

-- | Information about an approval state change for a pull request.
pullRequestEvent_approvalStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe ApprovalStateChangedEventMetadata)
pullRequestEvent_approvalStateChangedEventMetadata = Lens.lens (\PullRequestEvent' {approvalStateChangedEventMetadata} -> approvalStateChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalStateChangedEventMetadata = a} :: PullRequestEvent)

-- | The day and time of the pull request event, in timestamp format.
pullRequestEvent_eventDate :: Lens.Lens' PullRequestEvent (Prelude.Maybe Prelude.UTCTime)
pullRequestEvent_eventDate = Lens.lens (\PullRequestEvent' {eventDate} -> eventDate) (\s@PullRequestEvent' {} a -> s {eventDate = a} :: PullRequestEvent) Prelude.. Lens.mapping Data._Time

-- | Information about the source and destination branches for the pull
-- request.
pullRequestEvent_pullRequestCreatedEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe PullRequestCreatedEventMetadata)
pullRequestEvent_pullRequestCreatedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestCreatedEventMetadata} -> pullRequestCreatedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestCreatedEventMetadata = a} :: PullRequestEvent)

-- | The type of the pull request event (for example, a status change event
-- (PULL_REQUEST_STATUS_CHANGED) or update event
-- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
pullRequestEvent_pullRequestEventType :: Lens.Lens' PullRequestEvent (Prelude.Maybe PullRequestEventType)
pullRequestEvent_pullRequestEventType = Lens.lens (\PullRequestEvent' {pullRequestEventType} -> pullRequestEventType) (\s@PullRequestEvent' {} a -> s {pullRequestEventType = a} :: PullRequestEvent)

-- | The system-generated ID of the pull request.
pullRequestEvent_pullRequestId :: Lens.Lens' PullRequestEvent (Prelude.Maybe Prelude.Text)
pullRequestEvent_pullRequestId = Lens.lens (\PullRequestEvent' {pullRequestId} -> pullRequestId) (\s@PullRequestEvent' {} a -> s {pullRequestId = a} :: PullRequestEvent)

-- | Information about the change in mergability state for the pull request
-- event.
pullRequestEvent_pullRequestMergedStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe PullRequestMergedStateChangedEventMetadata)
pullRequestEvent_pullRequestMergedStateChangedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestMergedStateChangedEventMetadata} -> pullRequestMergedStateChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestMergedStateChangedEventMetadata = a} :: PullRequestEvent)

-- | Information about the updated source branch for the pull request event.
pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe PullRequestSourceReferenceUpdatedEventMetadata)
pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestSourceReferenceUpdatedEventMetadata} -> pullRequestSourceReferenceUpdatedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestSourceReferenceUpdatedEventMetadata = a} :: PullRequestEvent)

-- | Information about the change in status for the pull request event.
pullRequestEvent_pullRequestStatusChangedEventMetadata :: Lens.Lens' PullRequestEvent (Prelude.Maybe PullRequestStatusChangedEventMetadata)
pullRequestEvent_pullRequestStatusChangedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestStatusChangedEventMetadata} -> pullRequestStatusChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestStatusChangedEventMetadata = a} :: PullRequestEvent)

instance Data.FromJSON PullRequestEvent where
  parseJSON =
    Data.withObject
      "PullRequestEvent"
      ( \x ->
          PullRequestEvent'
            Prelude.<$> (x Data..:? "actorArn")
            Prelude.<*> (x Data..:? "approvalRuleEventMetadata")
            Prelude.<*> (x Data..:? "approvalRuleOverriddenEventMetadata")
            Prelude.<*> (x Data..:? "approvalStateChangedEventMetadata")
            Prelude.<*> (x Data..:? "eventDate")
            Prelude.<*> (x Data..:? "pullRequestCreatedEventMetadata")
            Prelude.<*> (x Data..:? "pullRequestEventType")
            Prelude.<*> (x Data..:? "pullRequestId")
            Prelude.<*> ( x
                            Data..:? "pullRequestMergedStateChangedEventMetadata"
                        )
            Prelude.<*> ( x
                            Data..:? "pullRequestSourceReferenceUpdatedEventMetadata"
                        )
            Prelude.<*> (x Data..:? "pullRequestStatusChangedEventMetadata")
      )

instance Prelude.Hashable PullRequestEvent where
  hashWithSalt _salt PullRequestEvent' {..} =
    _salt `Prelude.hashWithSalt` actorArn
      `Prelude.hashWithSalt` approvalRuleEventMetadata
      `Prelude.hashWithSalt` approvalRuleOverriddenEventMetadata
      `Prelude.hashWithSalt` approvalStateChangedEventMetadata
      `Prelude.hashWithSalt` eventDate
      `Prelude.hashWithSalt` pullRequestCreatedEventMetadata
      `Prelude.hashWithSalt` pullRequestEventType
      `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` pullRequestMergedStateChangedEventMetadata
      `Prelude.hashWithSalt` pullRequestSourceReferenceUpdatedEventMetadata
      `Prelude.hashWithSalt` pullRequestStatusChangedEventMetadata

instance Prelude.NFData PullRequestEvent where
  rnf PullRequestEvent' {..} =
    Prelude.rnf actorArn
      `Prelude.seq` Prelude.rnf approvalRuleEventMetadata
      `Prelude.seq` Prelude.rnf approvalRuleOverriddenEventMetadata
      `Prelude.seq` Prelude.rnf approvalStateChangedEventMetadata
      `Prelude.seq` Prelude.rnf eventDate
      `Prelude.seq` Prelude.rnf pullRequestCreatedEventMetadata
      `Prelude.seq` Prelude.rnf pullRequestEventType
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf
        pullRequestMergedStateChangedEventMetadata
      `Prelude.seq` Prelude.rnf
        pullRequestSourceReferenceUpdatedEventMetadata
      `Prelude.seq` Prelude.rnf
        pullRequestStatusChangedEventMetadata
