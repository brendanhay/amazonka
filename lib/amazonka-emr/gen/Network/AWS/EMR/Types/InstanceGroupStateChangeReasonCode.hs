{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode where

import Network.AWS.Prelude

data InstanceGroupStateChangeReasonCode
  = ClusterTerminated
  | InstanceFailure
  | InternalError
  | ValidationError
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText InstanceGroupStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "cluster_terminated" -> pure ClusterTerminated
      "instance_failure" -> pure InstanceFailure
      "internal_error" -> pure InternalError
      "validation_error" -> pure ValidationError
      e ->
        fromTextError $
          "Failure parsing InstanceGroupStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceGroupStateChangeReasonCode where
  toText = \case
    ClusterTerminated -> "CLUSTER_TERMINATED"
    InstanceFailure -> "INSTANCE_FAILURE"
    InternalError -> "INTERNAL_ERROR"
    ValidationError -> "VALIDATION_ERROR"

instance Hashable InstanceGroupStateChangeReasonCode

instance NFData InstanceGroupStateChangeReasonCode

instance ToByteString InstanceGroupStateChangeReasonCode

instance ToQuery InstanceGroupStateChangeReasonCode

instance ToHeader InstanceGroupStateChangeReasonCode

instance FromJSON InstanceGroupStateChangeReasonCode where
  parseJSON = parseJSONText "InstanceGroupStateChangeReasonCode"
