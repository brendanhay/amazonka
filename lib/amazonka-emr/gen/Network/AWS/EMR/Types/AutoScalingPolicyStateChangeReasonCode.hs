-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
  ( AutoScalingPolicyStateChangeReasonCode
      ( AutoScalingPolicyStateChangeReasonCode',
        CleanupFailure,
        ProvisionFailure,
        UserRequest
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode' Lude.Text
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

pattern CleanupFailure :: AutoScalingPolicyStateChangeReasonCode
pattern CleanupFailure = AutoScalingPolicyStateChangeReasonCode' "CLEANUP_FAILURE"

pattern ProvisionFailure :: AutoScalingPolicyStateChangeReasonCode
pattern ProvisionFailure = AutoScalingPolicyStateChangeReasonCode' "PROVISION_FAILURE"

pattern UserRequest :: AutoScalingPolicyStateChangeReasonCode
pattern UserRequest = AutoScalingPolicyStateChangeReasonCode' "USER_REQUEST"

{-# COMPLETE
  CleanupFailure,
  ProvisionFailure,
  UserRequest,
  AutoScalingPolicyStateChangeReasonCode'
  #-}
