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
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestEvent where

import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEventType
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a pull request event.
--
-- /See:/ 'newPullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { -- | Information about the change in mergability state for the pull request
    -- event.
    pullRequestMergedStateChangedEventMetadata :: Core.Maybe PullRequestMergedStateChangedEventMetadata,
    -- | Information about the updated source branch for the pull request event.
    pullRequestSourceReferenceUpdatedEventMetadata :: Core.Maybe PullRequestSourceReferenceUpdatedEventMetadata,
    -- | Information about an approval state change for a pull request.
    approvalStateChangedEventMetadata :: Core.Maybe ApprovalStateChangedEventMetadata,
    -- | The type of the pull request event (for example, a status change event
    -- (PULL_REQUEST_STATUS_CHANGED) or update event
    -- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
    pullRequestEventType :: Core.Maybe PullRequestEventType,
    -- | The day and time of the pull request event, in timestamp format.
    eventDate :: Core.Maybe Core.POSIX,
    -- | Information about the source and destination branches for the pull
    -- request.
    pullRequestCreatedEventMetadata :: Core.Maybe PullRequestCreatedEventMetadata,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Core.Maybe Core.Text,
    -- | Information about an approval rule override event for a pull request.
    approvalRuleOverriddenEventMetadata :: Core.Maybe ApprovalRuleOverriddenEventMetadata,
    -- | The Amazon Resource Name (ARN) of the user whose actions resulted in the
    -- event. Examples include updating the pull request with more commits or
    -- changing the status of a pull request.
    actorArn :: Core.Maybe Core.Text,
    -- | Information about the change in status for the pull request event.
    pullRequestStatusChangedEventMetadata :: Core.Maybe PullRequestStatusChangedEventMetadata,
    -- | Information about a pull request event.
    approvalRuleEventMetadata :: Core.Maybe ApprovalRuleEventMetadata
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PullRequestEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestMergedStateChangedEventMetadata', 'pullRequestEvent_pullRequestMergedStateChangedEventMetadata' - Information about the change in mergability state for the pull request
-- event.
--
-- 'pullRequestSourceReferenceUpdatedEventMetadata', 'pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata' - Information about the updated source branch for the pull request event.
--
-- 'approvalStateChangedEventMetadata', 'pullRequestEvent_approvalStateChangedEventMetadata' - Information about an approval state change for a pull request.
--
-- 'pullRequestEventType', 'pullRequestEvent_pullRequestEventType' - The type of the pull request event (for example, a status change event
-- (PULL_REQUEST_STATUS_CHANGED) or update event
-- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
--
-- 'eventDate', 'pullRequestEvent_eventDate' - The day and time of the pull request event, in timestamp format.
--
-- 'pullRequestCreatedEventMetadata', 'pullRequestEvent_pullRequestCreatedEventMetadata' - Information about the source and destination branches for the pull
-- request.
--
-- 'pullRequestId', 'pullRequestEvent_pullRequestId' - The system-generated ID of the pull request.
--
-- 'approvalRuleOverriddenEventMetadata', 'pullRequestEvent_approvalRuleOverriddenEventMetadata' - Information about an approval rule override event for a pull request.
--
-- 'actorArn', 'pullRequestEvent_actorArn' - The Amazon Resource Name (ARN) of the user whose actions resulted in the
-- event. Examples include updating the pull request with more commits or
-- changing the status of a pull request.
--
-- 'pullRequestStatusChangedEventMetadata', 'pullRequestEvent_pullRequestStatusChangedEventMetadata' - Information about the change in status for the pull request event.
--
-- 'approvalRuleEventMetadata', 'pullRequestEvent_approvalRuleEventMetadata' - Information about a pull request event.
newPullRequestEvent ::
  PullRequestEvent
newPullRequestEvent =
  PullRequestEvent'
    { pullRequestMergedStateChangedEventMetadata =
        Core.Nothing,
      pullRequestSourceReferenceUpdatedEventMetadata =
        Core.Nothing,
      approvalStateChangedEventMetadata = Core.Nothing,
      pullRequestEventType = Core.Nothing,
      eventDate = Core.Nothing,
      pullRequestCreatedEventMetadata = Core.Nothing,
      pullRequestId = Core.Nothing,
      approvalRuleOverriddenEventMetadata = Core.Nothing,
      actorArn = Core.Nothing,
      pullRequestStatusChangedEventMetadata = Core.Nothing,
      approvalRuleEventMetadata = Core.Nothing
    }

-- | Information about the change in mergability state for the pull request
-- event.
pullRequestEvent_pullRequestMergedStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe PullRequestMergedStateChangedEventMetadata)
pullRequestEvent_pullRequestMergedStateChangedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestMergedStateChangedEventMetadata} -> pullRequestMergedStateChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestMergedStateChangedEventMetadata = a} :: PullRequestEvent)

-- | Information about the updated source branch for the pull request event.
pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe PullRequestSourceReferenceUpdatedEventMetadata)
pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestSourceReferenceUpdatedEventMetadata} -> pullRequestSourceReferenceUpdatedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestSourceReferenceUpdatedEventMetadata = a} :: PullRequestEvent)

-- | Information about an approval state change for a pull request.
pullRequestEvent_approvalStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe ApprovalStateChangedEventMetadata)
pullRequestEvent_approvalStateChangedEventMetadata = Lens.lens (\PullRequestEvent' {approvalStateChangedEventMetadata} -> approvalStateChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalStateChangedEventMetadata = a} :: PullRequestEvent)

-- | The type of the pull request event (for example, a status change event
-- (PULL_REQUEST_STATUS_CHANGED) or update event
-- (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
pullRequestEvent_pullRequestEventType :: Lens.Lens' PullRequestEvent (Core.Maybe PullRequestEventType)
pullRequestEvent_pullRequestEventType = Lens.lens (\PullRequestEvent' {pullRequestEventType} -> pullRequestEventType) (\s@PullRequestEvent' {} a -> s {pullRequestEventType = a} :: PullRequestEvent)

-- | The day and time of the pull request event, in timestamp format.
pullRequestEvent_eventDate :: Lens.Lens' PullRequestEvent (Core.Maybe Core.UTCTime)
pullRequestEvent_eventDate = Lens.lens (\PullRequestEvent' {eventDate} -> eventDate) (\s@PullRequestEvent' {} a -> s {eventDate = a} :: PullRequestEvent) Core.. Lens.mapping Core._Time

-- | Information about the source and destination branches for the pull
-- request.
pullRequestEvent_pullRequestCreatedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe PullRequestCreatedEventMetadata)
pullRequestEvent_pullRequestCreatedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestCreatedEventMetadata} -> pullRequestCreatedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestCreatedEventMetadata = a} :: PullRequestEvent)

-- | The system-generated ID of the pull request.
pullRequestEvent_pullRequestId :: Lens.Lens' PullRequestEvent (Core.Maybe Core.Text)
pullRequestEvent_pullRequestId = Lens.lens (\PullRequestEvent' {pullRequestId} -> pullRequestId) (\s@PullRequestEvent' {} a -> s {pullRequestId = a} :: PullRequestEvent)

-- | Information about an approval rule override event for a pull request.
pullRequestEvent_approvalRuleOverriddenEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe ApprovalRuleOverriddenEventMetadata)
pullRequestEvent_approvalRuleOverriddenEventMetadata = Lens.lens (\PullRequestEvent' {approvalRuleOverriddenEventMetadata} -> approvalRuleOverriddenEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalRuleOverriddenEventMetadata = a} :: PullRequestEvent)

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the
-- event. Examples include updating the pull request with more commits or
-- changing the status of a pull request.
pullRequestEvent_actorArn :: Lens.Lens' PullRequestEvent (Core.Maybe Core.Text)
pullRequestEvent_actorArn = Lens.lens (\PullRequestEvent' {actorArn} -> actorArn) (\s@PullRequestEvent' {} a -> s {actorArn = a} :: PullRequestEvent)

-- | Information about the change in status for the pull request event.
pullRequestEvent_pullRequestStatusChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe PullRequestStatusChangedEventMetadata)
pullRequestEvent_pullRequestStatusChangedEventMetadata = Lens.lens (\PullRequestEvent' {pullRequestStatusChangedEventMetadata} -> pullRequestStatusChangedEventMetadata) (\s@PullRequestEvent' {} a -> s {pullRequestStatusChangedEventMetadata = a} :: PullRequestEvent)

-- | Information about a pull request event.
pullRequestEvent_approvalRuleEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe ApprovalRuleEventMetadata)
pullRequestEvent_approvalRuleEventMetadata = Lens.lens (\PullRequestEvent' {approvalRuleEventMetadata} -> approvalRuleEventMetadata) (\s@PullRequestEvent' {} a -> s {approvalRuleEventMetadata = a} :: PullRequestEvent)

instance Core.FromJSON PullRequestEvent where
  parseJSON =
    Core.withObject
      "PullRequestEvent"
      ( \x ->
          PullRequestEvent'
            Core.<$> ( x
                         Core..:? "pullRequestMergedStateChangedEventMetadata"
                     )
            Core.<*> ( x
                         Core..:? "pullRequestSourceReferenceUpdatedEventMetadata"
                     )
            Core.<*> (x Core..:? "approvalStateChangedEventMetadata")
            Core.<*> (x Core..:? "pullRequestEventType")
            Core.<*> (x Core..:? "eventDate")
            Core.<*> (x Core..:? "pullRequestCreatedEventMetadata")
            Core.<*> (x Core..:? "pullRequestId")
            Core.<*> (x Core..:? "approvalRuleOverriddenEventMetadata")
            Core.<*> (x Core..:? "actorArn")
            Core.<*> (x Core..:? "pullRequestStatusChangedEventMetadata")
            Core.<*> (x Core..:? "approvalRuleEventMetadata")
      )

instance Core.Hashable PullRequestEvent

instance Core.NFData PullRequestEvent
