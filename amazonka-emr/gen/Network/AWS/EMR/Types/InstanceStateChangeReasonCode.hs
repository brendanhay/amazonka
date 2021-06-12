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
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReasonCode
  ( InstanceStateChangeReasonCode
      ( ..,
        InstanceStateChangeReasonCode_BOOTSTRAP_FAILURE,
        InstanceStateChangeReasonCode_CLUSTER_TERMINATED,
        InstanceStateChangeReasonCode_INSTANCE_FAILURE,
        InstanceStateChangeReasonCode_INTERNAL_ERROR,
        InstanceStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstanceStateChangeReasonCode = InstanceStateChangeReasonCode'
  { fromInstanceStateChangeReasonCode ::
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

pattern InstanceStateChangeReasonCode_BOOTSTRAP_FAILURE :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCode_BOOTSTRAP_FAILURE = InstanceStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern InstanceStateChangeReasonCode_CLUSTER_TERMINATED :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCode_CLUSTER_TERMINATED = InstanceStateChangeReasonCode' "CLUSTER_TERMINATED"

pattern InstanceStateChangeReasonCode_INSTANCE_FAILURE :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCode_INSTANCE_FAILURE = InstanceStateChangeReasonCode' "INSTANCE_FAILURE"

pattern InstanceStateChangeReasonCode_INTERNAL_ERROR :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCode_INTERNAL_ERROR = InstanceStateChangeReasonCode' "INTERNAL_ERROR"

pattern InstanceStateChangeReasonCode_VALIDATION_ERROR :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCode_VALIDATION_ERROR = InstanceStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  InstanceStateChangeReasonCode_BOOTSTRAP_FAILURE,
  InstanceStateChangeReasonCode_CLUSTER_TERMINATED,
  InstanceStateChangeReasonCode_INSTANCE_FAILURE,
  InstanceStateChangeReasonCode_INTERNAL_ERROR,
  InstanceStateChangeReasonCode_VALIDATION_ERROR,
  InstanceStateChangeReasonCode'
  #-}
