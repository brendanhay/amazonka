{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyState
  ( AutoScalingPolicyState
      ( ..,
        AutoScalingPolicyState_ATTACHED,
        AutoScalingPolicyState_ATTACHING,
        AutoScalingPolicyState_DETACHED,
        AutoScalingPolicyState_DETACHING,
        AutoScalingPolicyState_FAILED,
        AutoScalingPolicyState_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoScalingPolicyState = AutoScalingPolicyState'
  { fromAutoScalingPolicyState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AutoScalingPolicyState_ATTACHED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_ATTACHED = AutoScalingPolicyState' "ATTACHED"

pattern AutoScalingPolicyState_ATTACHING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_ATTACHING = AutoScalingPolicyState' "ATTACHING"

pattern AutoScalingPolicyState_DETACHED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_DETACHED = AutoScalingPolicyState' "DETACHED"

pattern AutoScalingPolicyState_DETACHING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_DETACHING = AutoScalingPolicyState' "DETACHING"

pattern AutoScalingPolicyState_FAILED :: AutoScalingPolicyState
pattern AutoScalingPolicyState_FAILED = AutoScalingPolicyState' "FAILED"

pattern AutoScalingPolicyState_PENDING :: AutoScalingPolicyState
pattern AutoScalingPolicyState_PENDING = AutoScalingPolicyState' "PENDING"

{-# COMPLETE
  AutoScalingPolicyState_ATTACHED,
  AutoScalingPolicyState_ATTACHING,
  AutoScalingPolicyState_DETACHED,
  AutoScalingPolicyState_DETACHING,
  AutoScalingPolicyState_FAILED,
  AutoScalingPolicyState_PENDING,
  AutoScalingPolicyState'
  #-}
