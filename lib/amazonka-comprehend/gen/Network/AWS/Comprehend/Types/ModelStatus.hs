{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ModelStatus where

import Network.AWS.Prelude

data ModelStatus
  = MSDeleting
  | MSInError
  | MSStopRequested
  | MSStopped
  | MSSubmitted
  | MSTrained
  | MSTraining
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

instance FromText ModelStatus where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure MSDeleting
      "in_error" -> pure MSInError
      "stop_requested" -> pure MSStopRequested
      "stopped" -> pure MSStopped
      "submitted" -> pure MSSubmitted
      "trained" -> pure MSTrained
      "training" -> pure MSTraining
      e ->
        fromTextError $
          "Failure parsing ModelStatus from value: '" <> e
            <> "'. Accepted values: deleting, in_error, stop_requested, stopped, submitted, trained, training"

instance ToText ModelStatus where
  toText = \case
    MSDeleting -> "DELETING"
    MSInError -> "IN_ERROR"
    MSStopRequested -> "STOP_REQUESTED"
    MSStopped -> "STOPPED"
    MSSubmitted -> "SUBMITTED"
    MSTrained -> "TRAINED"
    MSTraining -> "TRAINING"

instance Hashable ModelStatus

instance NFData ModelStatus

instance ToByteString ModelStatus

instance ToQuery ModelStatus

instance ToHeader ModelStatus

instance ToJSON ModelStatus where
  toJSON = toJSONText

instance FromJSON ModelStatus where
  parseJSON = parseJSONText "ModelStatus"
