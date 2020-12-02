{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionStatus where

import Network.AWS.Prelude

data ActionExecutionStatus
  = AESAbandoned
  | AESFailed
  | AESInProgress
  | AESSucceeded
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

instance FromText ActionExecutionStatus where
  parser =
    takeLowerText >>= \case
      "abandoned" -> pure AESAbandoned
      "failed" -> pure AESFailed
      "inprogress" -> pure AESInProgress
      "succeeded" -> pure AESSucceeded
      e ->
        fromTextError $
          "Failure parsing ActionExecutionStatus from value: '" <> e
            <> "'. Accepted values: abandoned, failed, inprogress, succeeded"

instance ToText ActionExecutionStatus where
  toText = \case
    AESAbandoned -> "Abandoned"
    AESFailed -> "Failed"
    AESInProgress -> "InProgress"
    AESSucceeded -> "Succeeded"

instance Hashable ActionExecutionStatus

instance NFData ActionExecutionStatus

instance ToByteString ActionExecutionStatus

instance ToQuery ActionExecutionStatus

instance ToHeader ActionExecutionStatus

instance FromJSON ActionExecutionStatus where
  parseJSON = parseJSONText "ActionExecutionStatus"
