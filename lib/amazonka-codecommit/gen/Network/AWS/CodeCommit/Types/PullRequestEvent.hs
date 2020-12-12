{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestEvent
  ( PullRequestEvent (..),

    -- * Smart constructor
    mkPullRequestEvent,

    -- * Lenses
    prePullRequestMergedStateChangedEventMetadata,
    prePullRequestCreatedEventMetadata,
    preApprovalRuleEventMetadata,
    prePullRequestEventType,
    prePullRequestStatusChangedEventMetadata,
    preActorARN,
    prePullRequestId,
    preEventDate,
    preApprovalStateChangedEventMetadata,
    prePullRequestSourceReferenceUpdatedEventMetadata,
    preApprovalRuleOverriddenEventMetadata,
  )
where

import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEventType
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a pull request event.
--
-- /See:/ 'mkPullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { pullRequestMergedStateChangedEventMetadata ::
      Lude.Maybe PullRequestMergedStateChangedEventMetadata,
    pullRequestCreatedEventMetadata ::
      Lude.Maybe PullRequestCreatedEventMetadata,
    approvalRuleEventMetadata ::
      Lude.Maybe ApprovalRuleEventMetadata,
    pullRequestEventType :: Lude.Maybe PullRequestEventType,
    pullRequestStatusChangedEventMetadata ::
      Lude.Maybe PullRequestStatusChangedEventMetadata,
    actorARN :: Lude.Maybe Lude.Text,
    pullRequestId :: Lude.Maybe Lude.Text,
    eventDate :: Lude.Maybe Lude.Timestamp,
    approvalStateChangedEventMetadata ::
      Lude.Maybe ApprovalStateChangedEventMetadata,
    pullRequestSourceReferenceUpdatedEventMetadata ::
      Lude.Maybe PullRequestSourceReferenceUpdatedEventMetadata,
    approvalRuleOverriddenEventMetadata ::
      Lude.Maybe ApprovalRuleOverriddenEventMetadata
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PullRequestEvent' with the minimum fields required to make a request.
--
-- * 'actorARN' - The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
-- * 'approvalRuleEventMetadata' - Information about a pull request event.
-- * 'approvalRuleOverriddenEventMetadata' - Information about an approval rule override event for a pull request.
-- * 'approvalStateChangedEventMetadata' - Information about an approval state change for a pull request.
-- * 'eventDate' - The day and time of the pull request event, in timestamp format.
-- * 'pullRequestCreatedEventMetadata' - Information about the source and destination branches for the pull request.
-- * 'pullRequestEventType' - The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
-- * 'pullRequestId' - The system-generated ID of the pull request.
-- * 'pullRequestMergedStateChangedEventMetadata' - Information about the change in mergability state for the pull request event.
-- * 'pullRequestSourceReferenceUpdatedEventMetadata' - Information about the updated source branch for the pull request event.
-- * 'pullRequestStatusChangedEventMetadata' - Information about the change in status for the pull request event.
mkPullRequestEvent ::
  PullRequestEvent
mkPullRequestEvent =
  PullRequestEvent'
    { pullRequestMergedStateChangedEventMetadata =
        Lude.Nothing,
      pullRequestCreatedEventMetadata = Lude.Nothing,
      approvalRuleEventMetadata = Lude.Nothing,
      pullRequestEventType = Lude.Nothing,
      pullRequestStatusChangedEventMetadata = Lude.Nothing,
      actorARN = Lude.Nothing,
      pullRequestId = Lude.Nothing,
      eventDate = Lude.Nothing,
      approvalStateChangedEventMetadata = Lude.Nothing,
      pullRequestSourceReferenceUpdatedEventMetadata = Lude.Nothing,
      approvalRuleOverriddenEventMetadata = Lude.Nothing
    }

-- | Information about the change in mergability state for the pull request event.
--
-- /Note:/ Consider using 'pullRequestMergedStateChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestMergedStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe PullRequestMergedStateChangedEventMetadata)
prePullRequestMergedStateChangedEventMetadata = Lens.lens (pullRequestMergedStateChangedEventMetadata :: PullRequestEvent -> Lude.Maybe PullRequestMergedStateChangedEventMetadata) (\s a -> s {pullRequestMergedStateChangedEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestMergedStateChangedEventMetadata "Use generic-lens or generic-optics with 'pullRequestMergedStateChangedEventMetadata' instead." #-}

-- | Information about the source and destination branches for the pull request.
--
-- /Note:/ Consider using 'pullRequestCreatedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestCreatedEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe PullRequestCreatedEventMetadata)
prePullRequestCreatedEventMetadata = Lens.lens (pullRequestCreatedEventMetadata :: PullRequestEvent -> Lude.Maybe PullRequestCreatedEventMetadata) (\s a -> s {pullRequestCreatedEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestCreatedEventMetadata "Use generic-lens or generic-optics with 'pullRequestCreatedEventMetadata' instead." #-}

-- | Information about a pull request event.
--
-- /Note:/ Consider using 'approvalRuleEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalRuleEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe ApprovalRuleEventMetadata)
preApprovalRuleEventMetadata = Lens.lens (approvalRuleEventMetadata :: PullRequestEvent -> Lude.Maybe ApprovalRuleEventMetadata) (\s a -> s {approvalRuleEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED preApprovalRuleEventMetadata "Use generic-lens or generic-optics with 'approvalRuleEventMetadata' instead." #-}

-- | The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
--
-- /Note:/ Consider using 'pullRequestEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestEventType :: Lens.Lens' PullRequestEvent (Lude.Maybe PullRequestEventType)
prePullRequestEventType = Lens.lens (pullRequestEventType :: PullRequestEvent -> Lude.Maybe PullRequestEventType) (\s a -> s {pullRequestEventType = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestEventType "Use generic-lens or generic-optics with 'pullRequestEventType' instead." #-}

-- | Information about the change in status for the pull request event.
--
-- /Note:/ Consider using 'pullRequestStatusChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestStatusChangedEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe PullRequestStatusChangedEventMetadata)
prePullRequestStatusChangedEventMetadata = Lens.lens (pullRequestStatusChangedEventMetadata :: PullRequestEvent -> Lude.Maybe PullRequestStatusChangedEventMetadata) (\s a -> s {pullRequestStatusChangedEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestStatusChangedEventMetadata "Use generic-lens or generic-optics with 'pullRequestStatusChangedEventMetadata' instead." #-}

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- /Note:/ Consider using 'actorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preActorARN :: Lens.Lens' PullRequestEvent (Lude.Maybe Lude.Text)
preActorARN = Lens.lens (actorARN :: PullRequestEvent -> Lude.Maybe Lude.Text) (\s a -> s {actorARN = a} :: PullRequestEvent)
{-# DEPRECATED preActorARN "Use generic-lens or generic-optics with 'actorARN' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestId :: Lens.Lens' PullRequestEvent (Lude.Maybe Lude.Text)
prePullRequestId = Lens.lens (pullRequestId :: PullRequestEvent -> Lude.Maybe Lude.Text) (\s a -> s {pullRequestId = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The day and time of the pull request event, in timestamp format.
--
-- /Note:/ Consider using 'eventDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preEventDate :: Lens.Lens' PullRequestEvent (Lude.Maybe Lude.Timestamp)
preEventDate = Lens.lens (eventDate :: PullRequestEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventDate = a} :: PullRequestEvent)
{-# DEPRECATED preEventDate "Use generic-lens or generic-optics with 'eventDate' instead." #-}

-- | Information about an approval state change for a pull request.
--
-- /Note:/ Consider using 'approvalStateChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe ApprovalStateChangedEventMetadata)
preApprovalStateChangedEventMetadata = Lens.lens (approvalStateChangedEventMetadata :: PullRequestEvent -> Lude.Maybe ApprovalStateChangedEventMetadata) (\s a -> s {approvalStateChangedEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED preApprovalStateChangedEventMetadata "Use generic-lens or generic-optics with 'approvalStateChangedEventMetadata' instead." #-}

-- | Information about the updated source branch for the pull request event.
--
-- /Note:/ Consider using 'pullRequestSourceReferenceUpdatedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestSourceReferenceUpdatedEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe PullRequestSourceReferenceUpdatedEventMetadata)
prePullRequestSourceReferenceUpdatedEventMetadata = Lens.lens (pullRequestSourceReferenceUpdatedEventMetadata :: PullRequestEvent -> Lude.Maybe PullRequestSourceReferenceUpdatedEventMetadata) (\s a -> s {pullRequestSourceReferenceUpdatedEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED prePullRequestSourceReferenceUpdatedEventMetadata "Use generic-lens or generic-optics with 'pullRequestSourceReferenceUpdatedEventMetadata' instead." #-}

-- | Information about an approval rule override event for a pull request.
--
-- /Note:/ Consider using 'approvalRuleOverriddenEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalRuleOverriddenEventMetadata :: Lens.Lens' PullRequestEvent (Lude.Maybe ApprovalRuleOverriddenEventMetadata)
preApprovalRuleOverriddenEventMetadata = Lens.lens (approvalRuleOverriddenEventMetadata :: PullRequestEvent -> Lude.Maybe ApprovalRuleOverriddenEventMetadata) (\s a -> s {approvalRuleOverriddenEventMetadata = a} :: PullRequestEvent)
{-# DEPRECATED preApprovalRuleOverriddenEventMetadata "Use generic-lens or generic-optics with 'approvalRuleOverriddenEventMetadata' instead." #-}

instance Lude.FromJSON PullRequestEvent where
  parseJSON =
    Lude.withObject
      "PullRequestEvent"
      ( \x ->
          PullRequestEvent'
            Lude.<$> (x Lude..:? "pullRequestMergedStateChangedEventMetadata")
            Lude.<*> (x Lude..:? "pullRequestCreatedEventMetadata")
            Lude.<*> (x Lude..:? "approvalRuleEventMetadata")
            Lude.<*> (x Lude..:? "pullRequestEventType")
            Lude.<*> (x Lude..:? "pullRequestStatusChangedEventMetadata")
            Lude.<*> (x Lude..:? "actorArn")
            Lude.<*> (x Lude..:? "pullRequestId")
            Lude.<*> (x Lude..:? "eventDate")
            Lude.<*> (x Lude..:? "approvalStateChangedEventMetadata")
            Lude.<*> (x Lude..:? "pullRequestSourceReferenceUpdatedEventMetadata")
            Lude.<*> (x Lude..:? "approvalRuleOverriddenEventMetadata")
      )
