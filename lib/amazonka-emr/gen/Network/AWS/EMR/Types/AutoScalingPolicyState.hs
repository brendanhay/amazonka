{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyState
  ( AutoScalingPolicyState
      ( AutoScalingPolicyState',
        Attached,
        Attaching,
        Detached,
        Detaching,
        Failed,
        Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoScalingPolicyState = AutoScalingPolicyState' Lude.Text
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

pattern Attached :: AutoScalingPolicyState
pattern Attached = AutoScalingPolicyState' "ATTACHED"

pattern Attaching :: AutoScalingPolicyState
pattern Attaching = AutoScalingPolicyState' "ATTACHING"

pattern Detached :: AutoScalingPolicyState
pattern Detached = AutoScalingPolicyState' "DETACHED"

pattern Detaching :: AutoScalingPolicyState
pattern Detaching = AutoScalingPolicyState' "DETACHING"

pattern Failed :: AutoScalingPolicyState
pattern Failed = AutoScalingPolicyState' "FAILED"

pattern Pending :: AutoScalingPolicyState
pattern Pending = AutoScalingPolicyState' "PENDING"

{-# COMPLETE
  Attached,
  Attaching,
  Detached,
  Detaching,
  Failed,
  Pending,
  AutoScalingPolicyState'
  #-}
