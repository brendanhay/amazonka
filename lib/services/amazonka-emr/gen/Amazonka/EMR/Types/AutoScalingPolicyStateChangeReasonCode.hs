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
-- Module      : Amazonka.EMR.Types.AutoScalingPolicyStateChangeReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoScalingPolicyStateChangeReasonCode
  ( AutoScalingPolicyStateChangeReasonCode
      ( ..,
        AutoScalingPolicyStateChangeReasonCode_CLEANUP_FAILURE,
        AutoScalingPolicyStateChangeReasonCode_PROVISION_FAILURE,
        AutoScalingPolicyStateChangeReasonCode_USER_REQUEST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode'
  { fromAutoScalingPolicyStateChangeReasonCode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
