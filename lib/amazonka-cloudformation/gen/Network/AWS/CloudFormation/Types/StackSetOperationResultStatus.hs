{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationResultStatus where

import Network.AWS.Prelude

data StackSetOperationResultStatus
  = Cancelled
  | Failed
  | Pending
  | Running
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

instance FromText StackSetOperationResultStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure Cancelled
      "failed" -> pure Failed
      "pending" -> pure Pending
      "running" -> pure Running
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing StackSetOperationResultStatus from value: '" <> e
            <> "'. Accepted values: cancelled, failed, pending, running, succeeded"

instance ToText StackSetOperationResultStatus where
  toText = \case
    Cancelled -> "CANCELLED"
    Failed -> "FAILED"
    Pending -> "PENDING"
    Running -> "RUNNING"
    Succeeded -> "SUCCEEDED"

instance Hashable StackSetOperationResultStatus

instance NFData StackSetOperationResultStatus

instance ToByteString StackSetOperationResultStatus

instance ToQuery StackSetOperationResultStatus

instance ToHeader StackSetOperationResultStatus

instance FromXML StackSetOperationResultStatus where
  parseXML = parseXMLText "StackSetOperationResultStatus"
