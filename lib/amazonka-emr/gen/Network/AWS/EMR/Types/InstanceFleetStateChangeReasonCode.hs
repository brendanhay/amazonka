{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode where

import Network.AWS.Prelude

data InstanceFleetStateChangeReasonCode
  = IFSCRCClusterTerminated
  | IFSCRCInstanceFailure
  | IFSCRCInternalError
  | IFSCRCValidationError
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

instance FromText InstanceFleetStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "cluster_terminated" -> pure IFSCRCClusterTerminated
      "instance_failure" -> pure IFSCRCInstanceFailure
      "internal_error" -> pure IFSCRCInternalError
      "validation_error" -> pure IFSCRCValidationError
      e ->
        fromTextError $
          "Failure parsing InstanceFleetStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceFleetStateChangeReasonCode where
  toText = \case
    IFSCRCClusterTerminated -> "CLUSTER_TERMINATED"
    IFSCRCInstanceFailure -> "INSTANCE_FAILURE"
    IFSCRCInternalError -> "INTERNAL_ERROR"
    IFSCRCValidationError -> "VALIDATION_ERROR"

instance Hashable InstanceFleetStateChangeReasonCode

instance NFData InstanceFleetStateChangeReasonCode

instance ToByteString InstanceFleetStateChangeReasonCode

instance ToQuery InstanceFleetStateChangeReasonCode

instance ToHeader InstanceFleetStateChangeReasonCode

instance FromJSON InstanceFleetStateChangeReasonCode where
  parseJSON = parseJSONText "InstanceFleetStateChangeReasonCode"
