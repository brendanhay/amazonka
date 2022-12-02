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
-- Module      : Amazonka.EMR.Types.InstanceStateChangeReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceStateChangeReasonCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceStateChangeReasonCode = InstanceStateChangeReasonCode'
  { fromInstanceStateChangeReasonCode ::
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
