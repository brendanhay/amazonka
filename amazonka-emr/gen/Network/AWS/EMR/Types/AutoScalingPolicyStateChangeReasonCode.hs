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
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
  ( AutoScalingPolicyStateChangeReasonCode
      ( ..,
        AutoScalingPolicyStateChangeReasonCode_CLEANUP_FAILURE,
        AutoScalingPolicyStateChangeReasonCode_PROVISION_FAILURE,
        AutoScalingPolicyStateChangeReasonCode_USER_REQUEST
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode'
  { fromAutoScalingPolicyStateChangeReasonCode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern AutoScalingPolicyStateChangeReasonCode_CLEANUP_FAILURE :: AutoScalingPolicyStateChangeReasonCode
pattern AutoScalingPolicyStateChangeReasonCode_CLEANUP_FAILURE = AutoScalingPolicyStateChangeReasonCode' "CLEANUP_FAILURE"

pattern AutoScalingPolicyStateChangeReasonCode_PROVISION_FAILURE :: AutoScalingPolicyStateChangeReasonCode
pattern AutoScalingPolicyStateChangeReasonCode_PROVISION_FAILURE = AutoScalingPolicyStateChangeReasonCode' "PROVISION_FAILURE"

pattern AutoScalingPolicyStateChangeReasonCode_USER_REQUEST :: AutoScalingPolicyStateChangeReasonCode
pattern AutoScalingPolicyStateChangeReasonCode_USER_REQUEST = AutoScalingPolicyStateChangeReasonCode' "USER_REQUEST"

{-# COMPLETE
  AutoScalingPolicyStateChangeReasonCode_CLEANUP_FAILURE,
  AutoScalingPolicyStateChangeReasonCode_PROVISION_FAILURE,
  AutoScalingPolicyStateChangeReasonCode_USER_REQUEST,
  AutoScalingPolicyStateChangeReasonCode'
  #-}
