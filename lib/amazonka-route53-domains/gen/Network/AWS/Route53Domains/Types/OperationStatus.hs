{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationStatus where

import Network.AWS.Prelude

data OperationStatus
  = Error'
  | Failed
  | InProgress
  | Submitted
  | Successful
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

instance FromText OperationStatus where
  parser =
    takeLowerText >>= \case
      "error" -> pure Error'
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "submitted" -> pure Submitted
      "successful" -> pure Successful
      e ->
        fromTextError $
          "Failure parsing OperationStatus from value: '" <> e
            <> "'. Accepted values: error, failed, in_progress, submitted, successful"

instance ToText OperationStatus where
  toText = \case
    Error' -> "ERROR"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    Submitted -> "SUBMITTED"
    Successful -> "SUCCESSFUL"

instance Hashable OperationStatus

instance NFData OperationStatus

instance ToByteString OperationStatus

instance ToQuery OperationStatus

instance ToHeader OperationStatus

instance FromJSON OperationStatus where
  parseJSON = parseJSONText "OperationStatus"
