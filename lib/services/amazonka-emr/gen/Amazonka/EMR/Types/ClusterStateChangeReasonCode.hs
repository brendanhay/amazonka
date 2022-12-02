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
-- Module      : Amazonka.EMR.Types.ClusterStateChangeReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterStateChangeReasonCode
  ( ClusterStateChangeReasonCode
      ( ..,
        ClusterStateChangeReasonCode_ALL_STEPS_COMPLETED,
        ClusterStateChangeReasonCode_BOOTSTRAP_FAILURE,
        ClusterStateChangeReasonCode_INSTANCE_FAILURE,
        ClusterStateChangeReasonCode_INSTANCE_FLEET_TIMEOUT,
        ClusterStateChangeReasonCode_INTERNAL_ERROR,
        ClusterStateChangeReasonCode_STEP_FAILURE,
        ClusterStateChangeReasonCode_USER_REQUEST,
        ClusterStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClusterStateChangeReasonCode = ClusterStateChangeReasonCode'
  { fromClusterStateChangeReasonCode ::
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

pattern ClusterStateChangeReasonCode_ALL_STEPS_COMPLETED :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_ALL_STEPS_COMPLETED = ClusterStateChangeReasonCode' "ALL_STEPS_COMPLETED"

pattern ClusterStateChangeReasonCode_BOOTSTRAP_FAILURE :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_BOOTSTRAP_FAILURE = ClusterStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern ClusterStateChangeReasonCode_INSTANCE_FAILURE :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_INSTANCE_FAILURE = ClusterStateChangeReasonCode' "INSTANCE_FAILURE"

pattern ClusterStateChangeReasonCode_INSTANCE_FLEET_TIMEOUT :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_INSTANCE_FLEET_TIMEOUT = ClusterStateChangeReasonCode' "INSTANCE_FLEET_TIMEOUT"

pattern ClusterStateChangeReasonCode_INTERNAL_ERROR :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_INTERNAL_ERROR = ClusterStateChangeReasonCode' "INTERNAL_ERROR"

pattern ClusterStateChangeReasonCode_STEP_FAILURE :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_STEP_FAILURE = ClusterStateChangeReasonCode' "STEP_FAILURE"

pattern ClusterStateChangeReasonCode_USER_REQUEST :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_USER_REQUEST = ClusterStateChangeReasonCode' "USER_REQUEST"

pattern ClusterStateChangeReasonCode_VALIDATION_ERROR :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCode_VALIDATION_ERROR = ClusterStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  ClusterStateChangeReasonCode_ALL_STEPS_COMPLETED,
  ClusterStateChangeReasonCode_BOOTSTRAP_FAILURE,
  ClusterStateChangeReasonCode_INSTANCE_FAILURE,
  ClusterStateChangeReasonCode_INSTANCE_FLEET_TIMEOUT,
  ClusterStateChangeReasonCode_INTERNAL_ERROR,
  ClusterStateChangeReasonCode_STEP_FAILURE,
  ClusterStateChangeReasonCode_USER_REQUEST,
  ClusterStateChangeReasonCode_VALIDATION_ERROR,
  ClusterStateChangeReasonCode'
  #-}
