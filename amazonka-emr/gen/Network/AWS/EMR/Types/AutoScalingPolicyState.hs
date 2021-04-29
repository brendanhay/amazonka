{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AutoScalingPolicyState = AutoScalingPolicyState'
  { fromAutoScalingPolicyState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
