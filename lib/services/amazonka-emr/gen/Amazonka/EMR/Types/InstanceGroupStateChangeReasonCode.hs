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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode'
  { fromInstanceGroupStateChangeReasonCode ::
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
