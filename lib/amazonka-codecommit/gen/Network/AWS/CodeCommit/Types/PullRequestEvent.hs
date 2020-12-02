{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a pull request event.
--
--
--
-- /See:/ 'pullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { _prePullRequestMergedStateChangedEventMetadata ::
      !(Maybe PullRequestMergedStateChangedEventMetadata),
    _prePullRequestCreatedEventMetadata ::
      !(Maybe PullRequestCreatedEventMetadata),
    _preApprovalRuleEventMetadata ::
      !(Maybe ApprovalRuleEventMetadata),
    _prePullRequestEventType :: !(Maybe PullRequestEventType),
    _prePullRequestStatusChangedEventMetadata ::
      !(Maybe PullRequestStatusChangedEventMetadata),
    _preActorARN :: !(Maybe Text),
    _prePullRequestId :: !(Maybe Text),
    _preEventDate :: !(Maybe POSIX),
    _preApprovalStateChangedEventMetadata ::
      !(Maybe ApprovalStateChangedEventMetadata),
    _prePullRequestSourceReferenceUpdatedEventMetadata ::
      !(Maybe PullRequestSourceReferenceUpdatedEventMetadata),
    _preApprovalRuleOverriddenEventMetadata ::
      !(Maybe ApprovalRuleOverriddenEventMetadata)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PullRequestEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prePullRequestMergedStateChangedEventMetadata' - Information about the change in mergability state for the pull request event.
--
-- * 'prePullRequestCreatedEventMetadata' - Information about the source and destination branches for the pull request.
--
-- * 'preApprovalRuleEventMetadata' - Information about a pull request event.
--
-- * 'prePullRequestEventType' - The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
--
-- * 'prePullRequestStatusChangedEventMetadata' - Information about the change in status for the pull request event.
--
-- * 'preActorARN' - The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- * 'prePullRequestId' - The system-generated ID of the pull request.
--
-- * 'preEventDate' - The day and time of the pull request event, in timestamp format.
--
-- * 'preApprovalStateChangedEventMetadata' - Information about an approval state change for a pull request.
--
-- * 'prePullRequestSourceReferenceUpdatedEventMetadata' - Information about the updated source branch for the pull request event.
--
-- * 'preApprovalRuleOverriddenEventMetadata' - Information about an approval rule override event for a pull request.
pullRequestEvent ::
  PullRequestEvent
pullRequestEvent =
  PullRequestEvent'
    { _prePullRequestMergedStateChangedEventMetadata =
        Nothing,
      _prePullRequestCreatedEventMetadata = Nothing,
      _preApprovalRuleEventMetadata = Nothing,
      _prePullRequestEventType = Nothing,
      _prePullRequestStatusChangedEventMetadata = Nothing,
      _preActorARN = Nothing,
      _prePullRequestId = Nothing,
      _preEventDate = Nothing,
      _preApprovalStateChangedEventMetadata = Nothing,
      _prePullRequestSourceReferenceUpdatedEventMetadata = Nothing,
      _preApprovalRuleOverriddenEventMetadata = Nothing
    }

-- | Information about the change in mergability state for the pull request event.
prePullRequestMergedStateChangedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestMergedStateChangedEventMetadata)
prePullRequestMergedStateChangedEventMetadata = lens _prePullRequestMergedStateChangedEventMetadata (\s a -> s {_prePullRequestMergedStateChangedEventMetadata = a})

-- | Information about the source and destination branches for the pull request.
prePullRequestCreatedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestCreatedEventMetadata)
prePullRequestCreatedEventMetadata = lens _prePullRequestCreatedEventMetadata (\s a -> s {_prePullRequestCreatedEventMetadata = a})

-- | Information about a pull request event.
preApprovalRuleEventMetadata :: Lens' PullRequestEvent (Maybe ApprovalRuleEventMetadata)
preApprovalRuleEventMetadata = lens _preApprovalRuleEventMetadata (\s a -> s {_preApprovalRuleEventMetadata = a})

-- | The type of the pull request event (for example, a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED)).
prePullRequestEventType :: Lens' PullRequestEvent (Maybe PullRequestEventType)
prePullRequestEventType = lens _prePullRequestEventType (\s a -> s {_prePullRequestEventType = a})

-- | Information about the change in status for the pull request event.
prePullRequestStatusChangedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestStatusChangedEventMetadata)
prePullRequestStatusChangedEventMetadata = lens _prePullRequestStatusChangedEventMetadata (\s a -> s {_prePullRequestStatusChangedEventMetadata = a})

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
preActorARN :: Lens' PullRequestEvent (Maybe Text)
preActorARN = lens _preActorARN (\s a -> s {_preActorARN = a})

-- | The system-generated ID of the pull request.
prePullRequestId :: Lens' PullRequestEvent (Maybe Text)
prePullRequestId = lens _prePullRequestId (\s a -> s {_prePullRequestId = a})

-- | The day and time of the pull request event, in timestamp format.
preEventDate :: Lens' PullRequestEvent (Maybe UTCTime)
preEventDate = lens _preEventDate (\s a -> s {_preEventDate = a}) . mapping _Time

-- | Information about an approval state change for a pull request.
preApprovalStateChangedEventMetadata :: Lens' PullRequestEvent (Maybe ApprovalStateChangedEventMetadata)
preApprovalStateChangedEventMetadata = lens _preApprovalStateChangedEventMetadata (\s a -> s {_preApprovalStateChangedEventMetadata = a})

-- | Information about the updated source branch for the pull request event.
prePullRequestSourceReferenceUpdatedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestSourceReferenceUpdatedEventMetadata)
prePullRequestSourceReferenceUpdatedEventMetadata = lens _prePullRequestSourceReferenceUpdatedEventMetadata (\s a -> s {_prePullRequestSourceReferenceUpdatedEventMetadata = a})

-- | Information about an approval rule override event for a pull request.
preApprovalRuleOverriddenEventMetadata :: Lens' PullRequestEvent (Maybe ApprovalRuleOverriddenEventMetadata)
preApprovalRuleOverriddenEventMetadata = lens _preApprovalRuleOverriddenEventMetadata (\s a -> s {_preApprovalRuleOverriddenEventMetadata = a})

instance FromJSON PullRequestEvent where
  parseJSON =
    withObject
      "PullRequestEvent"
      ( \x ->
          PullRequestEvent'
            <$> (x .:? "pullRequestMergedStateChangedEventMetadata")
            <*> (x .:? "pullRequestCreatedEventMetadata")
            <*> (x .:? "approvalRuleEventMetadata")
            <*> (x .:? "pullRequestEventType")
            <*> (x .:? "pullRequestStatusChangedEventMetadata")
            <*> (x .:? "actorArn")
            <*> (x .:? "pullRequestId")
            <*> (x .:? "eventDate")
            <*> (x .:? "approvalStateChangedEventMetadata")
            <*> (x .:? "pullRequestSourceReferenceUpdatedEventMetadata")
            <*> (x .:? "approvalRuleOverriddenEventMetadata")
      )

instance Hashable PullRequestEvent

instance NFData PullRequestEvent
