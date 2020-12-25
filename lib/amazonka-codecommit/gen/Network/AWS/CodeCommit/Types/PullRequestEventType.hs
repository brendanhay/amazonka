{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestEventType
  ( PullRequestEventType
      ( PullRequestEventType',
        PullRequestEventTypePullRequestCreated,
        PullRequestEventTypePullRequestStatusChanged,
        PullRequestEventTypePullRequestSourceReferenceUpdated,
        PullRequestEventTypePullRequestMergeStateChanged,
        PullRequestEventTypePullRequestApprovalRuleCreated,
        PullRequestEventTypePullRequestApprovalRuleUpdated,
        PullRequestEventTypePullRequestApprovalRuleDeleted,
        PullRequestEventTypePullRequestApprovalRuleOverridden,
        PullRequestEventTypePullRequestApprovalStateChanged,
        fromPullRequestEventType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PullRequestEventType = PullRequestEventType'
  { fromPullRequestEventType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PullRequestEventTypePullRequestCreated :: PullRequestEventType
pattern PullRequestEventTypePullRequestCreated = PullRequestEventType' "PULL_REQUEST_CREATED"

pattern PullRequestEventTypePullRequestStatusChanged :: PullRequestEventType
pattern PullRequestEventTypePullRequestStatusChanged = PullRequestEventType' "PULL_REQUEST_STATUS_CHANGED"

pattern PullRequestEventTypePullRequestSourceReferenceUpdated :: PullRequestEventType
pattern PullRequestEventTypePullRequestSourceReferenceUpdated = PullRequestEventType' "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"

pattern PullRequestEventTypePullRequestMergeStateChanged :: PullRequestEventType
pattern PullRequestEventTypePullRequestMergeStateChanged = PullRequestEventType' "PULL_REQUEST_MERGE_STATE_CHANGED"

pattern PullRequestEventTypePullRequestApprovalRuleCreated :: PullRequestEventType
pattern PullRequestEventTypePullRequestApprovalRuleCreated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_CREATED"

pattern PullRequestEventTypePullRequestApprovalRuleUpdated :: PullRequestEventType
pattern PullRequestEventTypePullRequestApprovalRuleUpdated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_UPDATED"

pattern PullRequestEventTypePullRequestApprovalRuleDeleted :: PullRequestEventType
pattern PullRequestEventTypePullRequestApprovalRuleDeleted = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_DELETED"

pattern PullRequestEventTypePullRequestApprovalRuleOverridden :: PullRequestEventType
pattern PullRequestEventTypePullRequestApprovalRuleOverridden = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN"

pattern PullRequestEventTypePullRequestApprovalStateChanged :: PullRequestEventType
pattern PullRequestEventTypePullRequestApprovalStateChanged = PullRequestEventType' "PULL_REQUEST_APPROVAL_STATE_CHANGED"

{-# COMPLETE
  PullRequestEventTypePullRequestCreated,
  PullRequestEventTypePullRequestStatusChanged,
  PullRequestEventTypePullRequestSourceReferenceUpdated,
  PullRequestEventTypePullRequestMergeStateChanged,
  PullRequestEventTypePullRequestApprovalRuleCreated,
  PullRequestEventTypePullRequestApprovalRuleUpdated,
  PullRequestEventTypePullRequestApprovalRuleDeleted,
  PullRequestEventTypePullRequestApprovalRuleOverridden,
  PullRequestEventTypePullRequestApprovalStateChanged,
  PullRequestEventType'
  #-}
