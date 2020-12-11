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
        MARSCreateFailed,
        MARSCreateInProgress,
        MARSCreateSuccessful,
        MARSDeleteFailed,
        MARSDeleteInProgress,
        MARSDeleteSuccessful,
        MARSUpdateFailed,
        MARSUpdateInProgress,
        MARSUpdateSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MemberAccountRuleStatus = MemberAccountRuleStatus' Lude.Text
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

pattern MARSCreateFailed :: MemberAccountRuleStatus
pattern MARSCreateFailed = MemberAccountRuleStatus' "CREATE_FAILED"

pattern MARSCreateInProgress :: MemberAccountRuleStatus
pattern MARSCreateInProgress = MemberAccountRuleStatus' "CREATE_IN_PROGRESS"

pattern MARSCreateSuccessful :: MemberAccountRuleStatus
pattern MARSCreateSuccessful = MemberAccountRuleStatus' "CREATE_SUCCESSFUL"

pattern MARSDeleteFailed :: MemberAccountRuleStatus
pattern MARSDeleteFailed = MemberAccountRuleStatus' "DELETE_FAILED"

pattern MARSDeleteInProgress :: MemberAccountRuleStatus
pattern MARSDeleteInProgress = MemberAccountRuleStatus' "DELETE_IN_PROGRESS"

pattern MARSDeleteSuccessful :: MemberAccountRuleStatus
pattern MARSDeleteSuccessful = MemberAccountRuleStatus' "DELETE_SUCCESSFUL"

pattern MARSUpdateFailed :: MemberAccountRuleStatus
pattern MARSUpdateFailed = MemberAccountRuleStatus' "UPDATE_FAILED"

pattern MARSUpdateInProgress :: MemberAccountRuleStatus
pattern MARSUpdateInProgress = MemberAccountRuleStatus' "UPDATE_IN_PROGRESS"

pattern MARSUpdateSuccessful :: MemberAccountRuleStatus
pattern MARSUpdateSuccessful = MemberAccountRuleStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  MARSCreateFailed,
  MARSCreateInProgress,
  MARSCreateSuccessful,
  MARSDeleteFailed,
  MARSDeleteInProgress,
  MARSDeleteSuccessful,
  MARSUpdateFailed,
  MARSUpdateInProgress,
  MARSUpdateSuccessful,
  MemberAccountRuleStatus'
  #-}
