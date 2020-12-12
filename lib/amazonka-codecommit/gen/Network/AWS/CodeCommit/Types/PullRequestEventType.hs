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
        PullRequestApprovalRuleCreated,
        PullRequestApprovalRuleDeleted,
        PullRequestApprovalRuleOverridden,
        PullRequestApprovalRuleUpdated,
        PullRequestApprovalStateChanged,
        PullRequestCreated,
        PullRequestMergeStateChanged,
        PullRequestSourceReferenceUpdated,
        PullRequestStatusChanged
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PullRequestEventType = PullRequestEventType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PullRequestApprovalRuleCreated :: PullRequestEventType
pattern PullRequestApprovalRuleCreated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_CREATED"

pattern PullRequestApprovalRuleDeleted :: PullRequestEventType
pattern PullRequestApprovalRuleDeleted = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_DELETED"

pattern PullRequestApprovalRuleOverridden :: PullRequestEventType
pattern PullRequestApprovalRuleOverridden = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN"

pattern PullRequestApprovalRuleUpdated :: PullRequestEventType
pattern PullRequestApprovalRuleUpdated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_UPDATED"

pattern PullRequestApprovalStateChanged :: PullRequestEventType
pattern PullRequestApprovalStateChanged = PullRequestEventType' "PULL_REQUEST_APPROVAL_STATE_CHANGED"

pattern PullRequestCreated :: PullRequestEventType
pattern PullRequestCreated = PullRequestEventType' "PULL_REQUEST_CREATED"

pattern PullRequestMergeStateChanged :: PullRequestEventType
pattern PullRequestMergeStateChanged = PullRequestEventType' "PULL_REQUEST_MERGE_STATE_CHANGED"

pattern PullRequestSourceReferenceUpdated :: PullRequestEventType
pattern PullRequestSourceReferenceUpdated = PullRequestEventType' "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"

pattern PullRequestStatusChanged :: PullRequestEventType
pattern PullRequestStatusChanged = PullRequestEventType' "PULL_REQUEST_STATUS_CHANGED"

{-# COMPLETE
  PullRequestApprovalRuleCreated,
  PullRequestApprovalRuleDeleted,
  PullRequestApprovalRuleOverridden,
  PullRequestApprovalRuleUpdated,
  PullRequestApprovalStateChanged,
  PullRequestCreated,
  PullRequestMergeStateChanged,
  PullRequestSourceReferenceUpdated,
  PullRequestStatusChanged,
  PullRequestEventType'
  #-}
