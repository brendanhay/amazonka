{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStepState where

import Network.AWS.Prelude

data RemediationExecutionStepState
  = Failed
  | Pending
  | Succeeded
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

instance FromText RemediationExecutionStepState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "pending" -> pure Pending
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing RemediationExecutionStepState from value: '" <> e
            <> "'. Accepted values: failed, pending, succeeded"

instance ToText RemediationExecutionStepState where
  toText = \case
    Failed -> "FAILED"
    Pending -> "PENDING"
    Succeeded -> "SUCCEEDED"

instance Hashable RemediationExecutionStepState

instance NFData RemediationExecutionStepState

instance ToByteString RemediationExecutionStepState

instance ToQuery RemediationExecutionStepState

instance ToHeader RemediationExecutionStepState

instance FromJSON RemediationExecutionStepState where
  parseJSON = parseJSONText "RemediationExecutionStepState"
