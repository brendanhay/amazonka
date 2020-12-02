{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunState where

import Network.AWS.Prelude

data AssessmentRunState
  = Canceled
  | CollectingData
  | Completed
  | CompletedWithErrors
  | Created
  | DataCollected
  | Error'
  | EvaluatingRules
  | Failed
  | StartDataCollectionInProgress
  | StartDataCollectionPending
  | StartEvaluatingRulesPending
  | StopDataCollectionPending
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

instance FromText AssessmentRunState where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure Canceled
      "collecting_data" -> pure CollectingData
      "completed" -> pure Completed
      "completed_with_errors" -> pure CompletedWithErrors
      "created" -> pure Created
      "data_collected" -> pure DataCollected
      "error" -> pure Error'
      "evaluating_rules" -> pure EvaluatingRules
      "failed" -> pure Failed
      "start_data_collection_in_progress" -> pure StartDataCollectionInProgress
      "start_data_collection_pending" -> pure StartDataCollectionPending
      "start_evaluating_rules_pending" -> pure StartEvaluatingRulesPending
      "stop_data_collection_pending" -> pure StopDataCollectionPending
      e ->
        fromTextError $
          "Failure parsing AssessmentRunState from value: '" <> e
            <> "'. Accepted values: canceled, collecting_data, completed, completed_with_errors, created, data_collected, error, evaluating_rules, failed, start_data_collection_in_progress, start_data_collection_pending, start_evaluating_rules_pending, stop_data_collection_pending"

instance ToText AssessmentRunState where
  toText = \case
    Canceled -> "CANCELED"
    CollectingData -> "COLLECTING_DATA"
    Completed -> "COMPLETED"
    CompletedWithErrors -> "COMPLETED_WITH_ERRORS"
    Created -> "CREATED"
    DataCollected -> "DATA_COLLECTED"
    Error' -> "ERROR"
    EvaluatingRules -> "EVALUATING_RULES"
    Failed -> "FAILED"
    StartDataCollectionInProgress -> "START_DATA_COLLECTION_IN_PROGRESS"
    StartDataCollectionPending -> "START_DATA_COLLECTION_PENDING"
    StartEvaluatingRulesPending -> "START_EVALUATING_RULES_PENDING"
    StopDataCollectionPending -> "STOP_DATA_COLLECTION_PENDING"

instance Hashable AssessmentRunState

instance NFData AssessmentRunState

instance ToByteString AssessmentRunState

instance ToQuery AssessmentRunState

instance ToHeader AssessmentRunState

instance ToJSON AssessmentRunState where
  toJSON = toJSONText

instance FromJSON AssessmentRunState where
  parseJSON = parseJSONText "AssessmentRunState"
