{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ValidationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationStatus where

import Network.AWS.Prelude

data ValidationStatus
  = Failed
  | InProgress
  | Pending
  | ReadyForValidation
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

instance FromText ValidationStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "pending" -> pure Pending
      "ready_for_validation" -> pure ReadyForValidation
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing ValidationStatus from value: '" <> e
            <> "'. Accepted values: failed, in_progress, pending, ready_for_validation, succeeded"

instance ToText ValidationStatus where
  toText = \case
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    Pending -> "PENDING"
    ReadyForValidation -> "READY_FOR_VALIDATION"
    Succeeded -> "SUCCEEDED"

instance Hashable ValidationStatus

instance NFData ValidationStatus

instance ToByteString ValidationStatus

instance ToQuery ValidationStatus

instance ToHeader ValidationStatus

instance ToJSON ValidationStatus where
  toJSON = toJSONText

instance FromJSON ValidationStatus where
  parseJSON = parseJSONText "ValidationStatus"
