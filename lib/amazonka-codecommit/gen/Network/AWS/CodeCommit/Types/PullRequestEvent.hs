{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestEvent
  ( PullRequestEvent (..)
  -- * Smart constructor
  , mkPullRequestEvent
  -- * Lenses
  , preActorArn
  , preApprovalRuleEventMetadata
  , preApprovalRuleOverriddenEventMetadata
  , preApprovalStateChangedEventMetadata
  , preEventDate
  , prePullRequestCreatedEventMetadata
  , prePullRequestEventType
  , prePullRequestId
  , prePullRequestMergedStateChangedEventMetadata
  , prePullRequestSourceReferenceUpdatedEventMetadata
  , prePullRequestStatusChangedEventMetadata
  ) where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestEventType as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestId as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata as Types
import qualified Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a pull request event.
--
-- /See:/ 'mkPullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { actorArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
  , approvalRuleEventMetadata :: Core.Maybe Types.ApprovalRuleEventMetadata
    -- ^ Information about a pull request event.
  , approvalRuleOverriddenEventMetadata :: Core.Maybe Types.ApprovalRuleOverriddenEventMetadata
    -- ^ Information about an approval rule override event for a pull request.
  , approvalStateChangedEventMetadata :: Core.Maybe Types.ApprovalStateChangedEventMetadata
    -- ^ Information about an approval state change for a pull request.
  , eventDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The day and time of the pull request event, in timestamp format.
  , pullRequestCreatedEventMetadata :: Core.Maybe Types.PullRequestCreatedEventMetadata
    -- ^ Information about the source and destination branches for the pull request.
  , pullRequestEventType :: Core.Maybe Types.PullRequestEventType
    -- ^ The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
  , pullRequestId :: Core.Maybe Types.PullRequestId
    -- ^ The system-generated ID of the pull request.
  , pullRequestMergedStateChangedEventMetadata :: Core.Maybe Types.PullRequestMergedStateChangedEventMetadata
    -- ^ Information about the change in mergability state for the pull request event.
  , pullRequestSourceReferenceUpdatedEventMetadata :: Core.Maybe Types.PullRequestSourceReferenceUpdatedEventMetadata
    -- ^ Information about the updated source branch for the pull request event. 
  , pullRequestStatusChangedEventMetadata :: Core.Maybe Types.PullRequestStatusChangedEventMetadata
    -- ^ Information about the change in status for the pull request event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PullRequestEvent' value with any optional fields omitted.
mkPullRequestEvent
    :: PullRequestEvent
mkPullRequestEvent
  = PullRequestEvent'{actorArn = Core.Nothing,
                      approvalRuleEventMetadata = Core.Nothing,
                      approvalRuleOverriddenEventMetadata = Core.Nothing,
                      approvalStateChangedEventMetadata = Core.Nothing,
                      eventDate = Core.Nothing,
                      pullRequestCreatedEventMetadata = Core.Nothing,
                      pullRequestEventType = Core.Nothing, pullRequestId = Core.Nothing,
                      pullRequestMergedStateChangedEventMetadata = Core.Nothing,
                      pullRequestSourceReferenceUpdatedEventMetadata = Core.Nothing,
                      pullRequestStatusChangedEventMetadata = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- /Note:/ Consider using 'actorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preActorArn :: Lens.Lens' PullRequestEvent (Core.Maybe Types.Arn)
preActorArn = Lens.field @"actorArn"
{-# INLINEABLE preActorArn #-}
{-# DEPRECATED actorArn "Use generic-lens or generic-optics with 'actorArn' instead"  #-}

-- | Information about a pull request event.
--
-- /Note:/ Consider using 'approvalRuleEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalRuleEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.ApprovalRuleEventMetadata)
preApprovalRuleEventMetadata = Lens.field @"approvalRuleEventMetadata"
{-# INLINEABLE preApprovalRuleEventMetadata #-}
{-# DEPRECATED approvalRuleEventMetadata "Use generic-lens or generic-optics with 'approvalRuleEventMetadata' instead"  #-}

-- | Information about an approval rule override event for a pull request.
--
-- /Note:/ Consider using 'approvalRuleOverriddenEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalRuleOverriddenEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.ApprovalRuleOverriddenEventMetadata)
preApprovalRuleOverriddenEventMetadata = Lens.field @"approvalRuleOverriddenEventMetadata"
{-# INLINEABLE preApprovalRuleOverriddenEventMetadata #-}
{-# DEPRECATED approvalRuleOverriddenEventMetadata "Use generic-lens or generic-optics with 'approvalRuleOverriddenEventMetadata' instead"  #-}

-- | Information about an approval state change for a pull request.
--
-- /Note:/ Consider using 'approvalStateChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preApprovalStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.ApprovalStateChangedEventMetadata)
preApprovalStateChangedEventMetadata = Lens.field @"approvalStateChangedEventMetadata"
{-# INLINEABLE preApprovalStateChangedEventMetadata #-}
{-# DEPRECATED approvalStateChangedEventMetadata "Use generic-lens or generic-optics with 'approvalStateChangedEventMetadata' instead"  #-}

-- | The day and time of the pull request event, in timestamp format.
--
-- /Note:/ Consider using 'eventDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preEventDate :: Lens.Lens' PullRequestEvent (Core.Maybe Core.NominalDiffTime)
preEventDate = Lens.field @"eventDate"
{-# INLINEABLE preEventDate #-}
{-# DEPRECATED eventDate "Use generic-lens or generic-optics with 'eventDate' instead"  #-}

-- | Information about the source and destination branches for the pull request.
--
-- /Note:/ Consider using 'pullRequestCreatedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestCreatedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestCreatedEventMetadata)
prePullRequestCreatedEventMetadata = Lens.field @"pullRequestCreatedEventMetadata"
{-# INLINEABLE prePullRequestCreatedEventMetadata #-}
{-# DEPRECATED pullRequestCreatedEventMetadata "Use generic-lens or generic-optics with 'pullRequestCreatedEventMetadata' instead"  #-}

-- | The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
--
-- /Note:/ Consider using 'pullRequestEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestEventType :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestEventType)
prePullRequestEventType = Lens.field @"pullRequestEventType"
{-# INLINEABLE prePullRequestEventType #-}
{-# DEPRECATED pullRequestEventType "Use generic-lens or generic-optics with 'pullRequestEventType' instead"  #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestId :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestId)
prePullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE prePullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | Information about the change in mergability state for the pull request event.
--
-- /Note:/ Consider using 'pullRequestMergedStateChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestMergedStateChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestMergedStateChangedEventMetadata)
prePullRequestMergedStateChangedEventMetadata = Lens.field @"pullRequestMergedStateChangedEventMetadata"
{-# INLINEABLE prePullRequestMergedStateChangedEventMetadata #-}
{-# DEPRECATED pullRequestMergedStateChangedEventMetadata "Use generic-lens or generic-optics with 'pullRequestMergedStateChangedEventMetadata' instead"  #-}

-- | Information about the updated source branch for the pull request event. 
--
-- /Note:/ Consider using 'pullRequestSourceReferenceUpdatedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestSourceReferenceUpdatedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestSourceReferenceUpdatedEventMetadata)
prePullRequestSourceReferenceUpdatedEventMetadata = Lens.field @"pullRequestSourceReferenceUpdatedEventMetadata"
{-# INLINEABLE prePullRequestSourceReferenceUpdatedEventMetadata #-}
{-# DEPRECATED pullRequestSourceReferenceUpdatedEventMetadata "Use generic-lens or generic-optics with 'pullRequestSourceReferenceUpdatedEventMetadata' instead"  #-}

-- | Information about the change in status for the pull request event.
--
-- /Note:/ Consider using 'pullRequestStatusChangedEventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prePullRequestStatusChangedEventMetadata :: Lens.Lens' PullRequestEvent (Core.Maybe Types.PullRequestStatusChangedEventMetadata)
prePullRequestStatusChangedEventMetadata = Lens.field @"pullRequestStatusChangedEventMetadata"
{-# INLINEABLE prePullRequestStatusChangedEventMetadata #-}
{-# DEPRECATED pullRequestStatusChangedEventMetadata "Use generic-lens or generic-optics with 'pullRequestStatusChangedEventMetadata' instead"  #-}

instance Core.FromJSON PullRequestEvent where
        parseJSON
          = Core.withObject "PullRequestEvent" Core.$
              \ x ->
                PullRequestEvent' Core.<$>
                  (x Core..:? "actorArn") Core.<*>
                    x Core..:? "approvalRuleEventMetadata"
                    Core.<*> x Core..:? "approvalRuleOverriddenEventMetadata"
                    Core.<*> x Core..:? "approvalStateChangedEventMetadata"
                    Core.<*> x Core..:? "eventDate"
                    Core.<*> x Core..:? "pullRequestCreatedEventMetadata"
                    Core.<*> x Core..:? "pullRequestEventType"
                    Core.<*> x Core..:? "pullRequestId"
                    Core.<*> x Core..:? "pullRequestMergedStateChangedEventMetadata"
                    Core.<*>
                    x Core..:? "pullRequestSourceReferenceUpdatedEventMetadata"
                    Core.<*> x Core..:? "pullRequestStatusChangedEventMetadata"
