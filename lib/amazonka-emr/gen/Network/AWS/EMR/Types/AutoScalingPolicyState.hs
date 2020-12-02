{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyState where

import Network.AWS.Prelude

data AutoScalingPolicyState
  = Attached
  | Attaching
  | Detached
  | Detaching
  | Failed
  | Pending
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

instance FromText AutoScalingPolicyState where
  parser =
    takeLowerText >>= \case
      "attached" -> pure Attached
      "attaching" -> pure Attaching
      "detached" -> pure Detached
      "detaching" -> pure Detaching
      "failed" -> pure Failed
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing AutoScalingPolicyState from value: '" <> e
            <> "'. Accepted values: attached, attaching, detached, detaching, failed, pending"

instance ToText AutoScalingPolicyState where
  toText = \case
    Attached -> "ATTACHED"
    Attaching -> "ATTACHING"
    Detached -> "DETACHED"
    Detaching -> "DETACHING"
    Failed -> "FAILED"
    Pending -> "PENDING"

instance Hashable AutoScalingPolicyState

instance NFData AutoScalingPolicyState

instance ToByteString AutoScalingPolicyState

instance ToQuery AutoScalingPolicyState

instance ToHeader AutoScalingPolicyState

instance FromJSON AutoScalingPolicyState where
  parseJSON = parseJSONText "AutoScalingPolicyState"
