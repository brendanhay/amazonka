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
-- Module      : Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
  ( InstanceFleetStateChangeReasonCode
      ( ..,
        InstanceFleetStateChangeReasonCode_CLUSTER_TERMINATED,
        InstanceFleetStateChangeReasonCode_INSTANCE_FAILURE,
        InstanceFleetStateChangeReasonCode_INTERNAL_ERROR,
        InstanceFleetStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InstanceFleetStateChangeReasonCode = InstanceFleetStateChangeReasonCode'
  { fromInstanceFleetStateChangeReasonCode ::
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
