{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReasonCode where

import Network.AWS.Prelude

data InstanceStateChangeReasonCode
  = ISCRCBootstrapFailure
  | ISCRCClusterTerminated
  | ISCRCInstanceFailure
  | ISCRCInternalError
  | ISCRCValidationError
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

instance FromText InstanceStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "bootstrap_failure" -> pure ISCRCBootstrapFailure
      "cluster_terminated" -> pure ISCRCClusterTerminated
      "instance_failure" -> pure ISCRCInstanceFailure
      "internal_error" -> pure ISCRCInternalError
      "validation_error" -> pure ISCRCValidationError
      e ->
        fromTextError $
          "Failure parsing InstanceStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: bootstrap_failure, cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceStateChangeReasonCode where
  toText = \case
    ISCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
    ISCRCClusterTerminated -> "CLUSTER_TERMINATED"
    ISCRCInstanceFailure -> "INSTANCE_FAILURE"
    ISCRCInternalError -> "INTERNAL_ERROR"
    ISCRCValidationError -> "VALIDATION_ERROR"

instance Hashable InstanceStateChangeReasonCode

instance NFData InstanceStateChangeReasonCode

instance ToByteString InstanceStateChangeReasonCode

instance ToQuery InstanceStateChangeReasonCode

instance ToHeader InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode where
  parseJSON = parseJSONText "InstanceStateChangeReasonCode"
