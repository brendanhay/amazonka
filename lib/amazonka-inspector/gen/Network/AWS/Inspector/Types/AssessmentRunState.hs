{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRunState
  ( AssessmentRunState
    ( AssessmentRunState'
    , AssessmentRunStateCreated
    , AssessmentRunStateStartDataCollectionPending
    , AssessmentRunStateStartDataCollectionInProgress
    , AssessmentRunStateCollectingData
    , AssessmentRunStateStopDataCollectionPending
    , AssessmentRunStateDataCollected
    , AssessmentRunStateStartEvaluatingRulesPending
    , AssessmentRunStateEvaluatingRules
    , AssessmentRunStateFailed
    , AssessmentRunStateError
    , AssessmentRunStateCompleted
    , AssessmentRunStateCompletedWithErrors
    , AssessmentRunStateCanceled
    , fromAssessmentRunState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AssessmentRunState = AssessmentRunState'{fromAssessmentRunState
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern AssessmentRunStateCreated :: AssessmentRunState
pattern AssessmentRunStateCreated = AssessmentRunState' "CREATED"

pattern AssessmentRunStateStartDataCollectionPending :: AssessmentRunState
pattern AssessmentRunStateStartDataCollectionPending = AssessmentRunState' "START_DATA_COLLECTION_PENDING"

pattern AssessmentRunStateStartDataCollectionInProgress :: AssessmentRunState
pattern AssessmentRunStateStartDataCollectionInProgress = AssessmentRunState' "START_DATA_COLLECTION_IN_PROGRESS"

pattern AssessmentRunStateCollectingData :: AssessmentRunState
pattern AssessmentRunStateCollectingData = AssessmentRunState' "COLLECTING_DATA"

pattern AssessmentRunStateStopDataCollectionPending :: AssessmentRunState
pattern AssessmentRunStateStopDataCollectionPending = AssessmentRunState' "STOP_DATA_COLLECTION_PENDING"

pattern AssessmentRunStateDataCollected :: AssessmentRunState
pattern AssessmentRunStateDataCollected = AssessmentRunState' "DATA_COLLECTED"

pattern AssessmentRunStateStartEvaluatingRulesPending :: AssessmentRunState
pattern AssessmentRunStateStartEvaluatingRulesPending = AssessmentRunState' "START_EVALUATING_RULES_PENDING"

pattern AssessmentRunStateEvaluatingRules :: AssessmentRunState
pattern AssessmentRunStateEvaluatingRules = AssessmentRunState' "EVALUATING_RULES"

pattern AssessmentRunStateFailed :: AssessmentRunState
pattern AssessmentRunStateFailed = AssessmentRunState' "FAILED"

pattern AssessmentRunStateError :: AssessmentRunState
pattern AssessmentRunStateError = AssessmentRunState' "ERROR"

pattern AssessmentRunStateCompleted :: AssessmentRunState
pattern AssessmentRunStateCompleted = AssessmentRunState' "COMPLETED"

pattern AssessmentRunStateCompletedWithErrors :: AssessmentRunState
pattern AssessmentRunStateCompletedWithErrors = AssessmentRunState' "COMPLETED_WITH_ERRORS"

pattern AssessmentRunStateCanceled :: AssessmentRunState
pattern AssessmentRunStateCanceled = AssessmentRunState' "CANCELED"

{-# COMPLETE 
  AssessmentRunStateCreated,

  AssessmentRunStateStartDataCollectionPending,

  AssessmentRunStateStartDataCollectionInProgress,

  AssessmentRunStateCollectingData,

  AssessmentRunStateStopDataCollectionPending,

  AssessmentRunStateDataCollected,

  AssessmentRunStateStartEvaluatingRulesPending,

  AssessmentRunStateEvaluatingRules,

  AssessmentRunStateFailed,

  AssessmentRunStateError,

  AssessmentRunStateCompleted,

  AssessmentRunStateCompletedWithErrors,

  AssessmentRunStateCanceled,
  AssessmentRunState'
  #-}
