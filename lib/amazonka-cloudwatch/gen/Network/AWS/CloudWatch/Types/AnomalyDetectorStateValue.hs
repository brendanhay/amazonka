{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue where

import Network.AWS.Prelude

data AnomalyDetectorStateValue
  = PendingTraining
  | Trained
  | TrainedInsufficientData
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

instance FromText AnomalyDetectorStateValue where
  parser =
    takeLowerText >>= \case
      "pending_training" -> pure PendingTraining
      "trained" -> pure Trained
      "trained_insufficient_data" -> pure TrainedInsufficientData
      e ->
        fromTextError $
          "Failure parsing AnomalyDetectorStateValue from value: '" <> e
            <> "'. Accepted values: pending_training, trained, trained_insufficient_data"

instance ToText AnomalyDetectorStateValue where
  toText = \case
    PendingTraining -> "PENDING_TRAINING"
    Trained -> "TRAINED"
    TrainedInsufficientData -> "TRAINED_INSUFFICIENT_DATA"

instance Hashable AnomalyDetectorStateValue

instance NFData AnomalyDetectorStateValue

instance ToByteString AnomalyDetectorStateValue

instance ToQuery AnomalyDetectorStateValue

instance ToHeader AnomalyDetectorStateValue

instance FromXML AnomalyDetectorStateValue where
  parseXML = parseXMLText "AnomalyDetectorStateValue"
