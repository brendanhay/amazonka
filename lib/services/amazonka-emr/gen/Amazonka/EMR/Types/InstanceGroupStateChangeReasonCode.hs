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
-- Module      : Amazonka.EMR.Types.InstanceGroupStateChangeReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupStateChangeReasonCode
  ( InstanceGroupStateChangeReasonCode
      ( ..,
        InstanceGroupStateChangeReasonCode_CLUSTER_TERMINATED,
        InstanceGroupStateChangeReasonCode_INSTANCE_FAILURE,
        InstanceGroupStateChangeReasonCode_INTERNAL_ERROR,
        InstanceGroupStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode'
  { fromInstanceGroupStateChangeReasonCode ::
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

pattern InstanceGroupStateChangeReasonCode_CLUSTER_TERMINATED :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCode_CLUSTER_TERMINATED = InstanceGroupStateChangeReasonCode' "CLUSTER_TERMINATED"

pattern InstanceGroupStateChangeReasonCode_INSTANCE_FAILURE :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCode_INSTANCE_FAILURE = InstanceGroupStateChangeReasonCode' "INSTANCE_FAILURE"

pattern InstanceGroupStateChangeReasonCode_INTERNAL_ERROR :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCode_INTERNAL_ERROR = InstanceGroupStateChangeReasonCode' "INTERNAL_ERROR"

pattern InstanceGroupStateChangeReasonCode_VALIDATION_ERROR :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCode_VALIDATION_ERROR = InstanceGroupStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  InstanceGroupStateChangeReasonCode_CLUSTER_TERMINATED,
  InstanceGroupStateChangeReasonCode_INSTANCE_FAILURE,
  InstanceGroupStateChangeReasonCode_INTERNAL_ERROR,
  InstanceGroupStateChangeReasonCode_VALIDATION_ERROR,
  InstanceGroupStateChangeReasonCode'
  #-}
