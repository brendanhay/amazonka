{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestEventType where

import Network.AWS.Prelude

data PullRequestEventType
  = PullRequestApprovalRuleCreated
  | PullRequestApprovalRuleDeleted
  | PullRequestApprovalRuleOverridden
  | PullRequestApprovalRuleUpdated
  | PullRequestApprovalStateChanged
  | PullRequestCreated
  | PullRequestMergeStateChanged
  | PullRequestSourceReferenceUpdated
  | PullRequestStatusChanged
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PullRequestEventType where
  parser =
    takeLowerText >>= \case
      "pull_request_approval_rule_created" -> pure PullRequestApprovalRuleCreated
      "pull_request_approval_rule_deleted" -> pure PullRequestApprovalRuleDeleted
      "pull_request_approval_rule_overridden" -> pure PullRequestApprovalRuleOverridden
      "pull_request_approval_rule_updated" -> pure PullRequestApprovalRuleUpdated
      "pull_request_approval_state_changed" -> pure PullRequestApprovalStateChanged
      "pull_request_created" -> pure PullRequestCreated
      "pull_request_merge_state_changed" -> pure PullRequestMergeStateChanged
      "pull_request_source_reference_updated" -> pure PullRequestSourceReferenceUpdated
      "pull_request_status_changed" -> pure PullRequestStatusChanged
      e ->
        fromTextError $
          "Failure parsing PullRequestEventType from value: '" <> e
            <> "'. Accepted values: pull_request_approval_rule_created, pull_request_approval_rule_deleted, pull_request_approval_rule_overridden, pull_request_approval_rule_updated, pull_request_approval_state_changed, pull_request_created, pull_request_merge_state_changed, pull_request_source_reference_updated, pull_request_status_changed"

instance ToText PullRequestEventType where
  toText = \case
    PullRequestApprovalRuleCreated -> "PULL_REQUEST_APPROVAL_RULE_CREATED"
    PullRequestApprovalRuleDeleted -> "PULL_REQUEST_APPROVAL_RULE_DELETED"
    PullRequestApprovalRuleOverridden -> "PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN"
    PullRequestApprovalRuleUpdated -> "PULL_REQUEST_APPROVAL_RULE_UPDATED"
    PullRequestApprovalStateChanged -> "PULL_REQUEST_APPROVAL_STATE_CHANGED"
    PullRequestCreated -> "PULL_REQUEST_CREATED"
    PullRequestMergeStateChanged -> "PULL_REQUEST_MERGE_STATE_CHANGED"
    PullRequestSourceReferenceUpdated -> "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"
    PullRequestStatusChanged -> "PULL_REQUEST_STATUS_CHANGED"

instance Hashable PullRequestEventType

instance NFData PullRequestEventType

instance ToByteString PullRequestEventType

instance ToQuery PullRequestEventType

instance ToHeader PullRequestEventType

instance ToJSON PullRequestEventType where
  toJSON = toJSONText

instance FromJSON PullRequestEventType where
  parseJSON = parseJSONText "PullRequestEventType"
