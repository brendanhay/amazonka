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
-- Module      : Amazonka.EMR.Types.InstanceFleetStateChangeReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetStateChangeReasonCode
  ( InstanceFleetStateChangeReasonCode
      ( ..,
        InstanceFleetStateChangeReasonCode_CLUSTER_TERMINATED,
        InstanceFleetStateChangeReasonCode_INSTANCE_FAILURE,
        InstanceFleetStateChangeReasonCode_INTERNAL_ERROR,
        InstanceFleetStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceFleetStateChangeReasonCode = InstanceFleetStateChangeReasonCode'
  { fromInstanceFleetStateChangeReasonCode ::
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

pattern InstanceFleetStateChangeReasonCode_CLUSTER_TERMINATED :: InstanceFleetStateChangeReasonCode
pattern InstanceFleetStateChangeReasonCode_CLUSTER_TERMINATED = InstanceFleetStateChangeReasonCode' "CLUSTER_TERMINATED"

pattern InstanceFleetStateChangeReasonCode_INSTANCE_FAILURE :: InstanceFleetStateChangeReasonCode
pattern InstanceFleetStateChangeReasonCode_INSTANCE_FAILURE = InstanceFleetStateChangeReasonCode' "INSTANCE_FAILURE"

pattern InstanceFleetStateChangeReasonCode_INTERNAL_ERROR :: InstanceFleetStateChangeReasonCode
pattern InstanceFleetStateChangeReasonCode_INTERNAL_ERROR = InstanceFleetStateChangeReasonCode' "INTERNAL_ERROR"

pattern InstanceFleetStateChangeReasonCode_VALIDATION_ERROR :: InstanceFleetStateChangeReasonCode
pattern InstanceFleetStateChangeReasonCode_VALIDATION_ERROR = InstanceFleetStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  InstanceFleetStateChangeReasonCode_CLUSTER_TERMINATED,
  InstanceFleetStateChangeReasonCode_INSTANCE_FAILURE,
  InstanceFleetStateChangeReasonCode_INTERNAL_ERROR,
  InstanceFleetStateChangeReasonCode_VALIDATION_ERROR,
  InstanceFleetStateChangeReasonCode'
  #-}
