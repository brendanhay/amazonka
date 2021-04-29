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
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
  ( InstanceGroupStateChangeReasonCode
      ( ..,
        InstanceGroupStateChangeReasonCode_CLUSTER_TERMINATED,
        InstanceGroupStateChangeReasonCode_INSTANCE_FAILURE,
        InstanceGroupStateChangeReasonCode_INTERNAL_ERROR,
        InstanceGroupStateChangeReasonCode_VALIDATION_ERROR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode'
  { fromInstanceGroupStateChangeReasonCode ::
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
