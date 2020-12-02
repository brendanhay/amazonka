{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode where

import Network.AWS.Prelude

data AutoScalingPolicyStateChangeReasonCode
  = CleanupFailure
  | ProvisionFailure
  | UserRequest
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

instance FromText AutoScalingPolicyStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "cleanup_failure" -> pure CleanupFailure
      "provision_failure" -> pure ProvisionFailure
      "user_request" -> pure UserRequest
      e ->
        fromTextError $
          "Failure parsing AutoScalingPolicyStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: cleanup_failure, provision_failure, user_request"

instance ToText AutoScalingPolicyStateChangeReasonCode where
  toText = \case
    CleanupFailure -> "CLEANUP_FAILURE"
    ProvisionFailure -> "PROVISION_FAILURE"
    UserRequest -> "USER_REQUEST"

instance Hashable AutoScalingPolicyStateChangeReasonCode

instance NFData AutoScalingPolicyStateChangeReasonCode

instance ToByteString AutoScalingPolicyStateChangeReasonCode

instance ToQuery AutoScalingPolicyStateChangeReasonCode

instance ToHeader AutoScalingPolicyStateChangeReasonCode

instance FromJSON AutoScalingPolicyStateChangeReasonCode where
  parseJSON = parseJSONText "AutoScalingPolicyStateChangeReasonCode"
