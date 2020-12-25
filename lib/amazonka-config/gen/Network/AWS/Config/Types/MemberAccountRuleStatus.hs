{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MemberAccountRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountRuleStatus
  ( MemberAccountRuleStatus
      ( MemberAccountRuleStatus',
        MemberAccountRuleStatusCreateSuccessful,
        MemberAccountRuleStatusCreateInProgress,
        MemberAccountRuleStatusCreateFailed,
        MemberAccountRuleStatusDeleteSuccessful,
        MemberAccountRuleStatusDeleteFailed,
        MemberAccountRuleStatusDeleteInProgress,
        MemberAccountRuleStatusUpdateSuccessful,
        MemberAccountRuleStatusUpdateInProgress,
        MemberAccountRuleStatusUpdateFailed,
        fromMemberAccountRuleStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MemberAccountRuleStatus = MemberAccountRuleStatus'
  { fromMemberAccountRuleStatus ::
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

pattern MemberAccountRuleStatusCreateSuccessful :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusCreateSuccessful = MemberAccountRuleStatus' "CREATE_SUCCESSFUL"

pattern MemberAccountRuleStatusCreateInProgress :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusCreateInProgress = MemberAccountRuleStatus' "CREATE_IN_PROGRESS"

pattern MemberAccountRuleStatusCreateFailed :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusCreateFailed = MemberAccountRuleStatus' "CREATE_FAILED"

pattern MemberAccountRuleStatusDeleteSuccessful :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusDeleteSuccessful = MemberAccountRuleStatus' "DELETE_SUCCESSFUL"

pattern MemberAccountRuleStatusDeleteFailed :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusDeleteFailed = MemberAccountRuleStatus' "DELETE_FAILED"

pattern MemberAccountRuleStatusDeleteInProgress :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusDeleteInProgress = MemberAccountRuleStatus' "DELETE_IN_PROGRESS"

pattern MemberAccountRuleStatusUpdateSuccessful :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusUpdateSuccessful = MemberAccountRuleStatus' "UPDATE_SUCCESSFUL"

pattern MemberAccountRuleStatusUpdateInProgress :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusUpdateInProgress = MemberAccountRuleStatus' "UPDATE_IN_PROGRESS"

pattern MemberAccountRuleStatusUpdateFailed :: MemberAccountRuleStatus
pattern MemberAccountRuleStatusUpdateFailed = MemberAccountRuleStatus' "UPDATE_FAILED"

{-# COMPLETE
  MemberAccountRuleStatusCreateSuccessful,
  MemberAccountRuleStatusCreateInProgress,
  MemberAccountRuleStatusCreateFailed,
  MemberAccountRuleStatusDeleteSuccessful,
  MemberAccountRuleStatusDeleteFailed,
  MemberAccountRuleStatusDeleteInProgress,
  MemberAccountRuleStatusUpdateSuccessful,
  MemberAccountRuleStatusUpdateInProgress,
  MemberAccountRuleStatusUpdateFailed,
  MemberAccountRuleStatus'
  #-}
