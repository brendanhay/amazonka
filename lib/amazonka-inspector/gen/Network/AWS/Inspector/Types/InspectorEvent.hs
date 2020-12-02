{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.InspectorEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.InspectorEvent where

import Network.AWS.Prelude

data InspectorEvent
  = AssessmentRunCompleted
  | AssessmentRunStarted
  | AssessmentRunStateChanged
  | FindingReported
  | Other
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

instance FromText InspectorEvent where
  parser =
    takeLowerText >>= \case
      "assessment_run_completed" -> pure AssessmentRunCompleted
      "assessment_run_started" -> pure AssessmentRunStarted
      "assessment_run_state_changed" -> pure AssessmentRunStateChanged
      "finding_reported" -> pure FindingReported
      "other" -> pure Other
      e ->
        fromTextError $
          "Failure parsing InspectorEvent from value: '" <> e
            <> "'. Accepted values: assessment_run_completed, assessment_run_started, assessment_run_state_changed, finding_reported, other"

instance ToText InspectorEvent where
  toText = \case
    AssessmentRunCompleted -> "ASSESSMENT_RUN_COMPLETED"
    AssessmentRunStarted -> "ASSESSMENT_RUN_STARTED"
    AssessmentRunStateChanged -> "ASSESSMENT_RUN_STATE_CHANGED"
    FindingReported -> "FINDING_REPORTED"
    Other -> "OTHER"

instance Hashable InspectorEvent

instance NFData InspectorEvent

instance ToByteString InspectorEvent

instance ToQuery InspectorEvent

instance ToHeader InspectorEvent

instance ToJSON InspectorEvent where
  toJSON = toJSONText

instance FromJSON InspectorEvent where
  parseJSON = parseJSONText "InspectorEvent"
