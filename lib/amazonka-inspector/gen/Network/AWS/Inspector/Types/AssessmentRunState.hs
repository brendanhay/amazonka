-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunState
  ( AssessmentRunState
      ( AssessmentRunState',
        Canceled,
        CollectingData,
        Completed,
        CompletedWithErrors,
        Created,
        DataCollected,
        Error,
        EvaluatingRules,
        Failed,
        StartDataCollectionInProgress,
        StartDataCollectionPending,
        StartEvaluatingRulesPending,
        StopDataCollectionPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssessmentRunState = AssessmentRunState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Canceled :: AssessmentRunState
pattern Canceled = AssessmentRunState' "CANCELED"

pattern CollectingData :: AssessmentRunState
pattern CollectingData = AssessmentRunState' "COLLECTING_DATA"

pattern Completed :: AssessmentRunState
pattern Completed = AssessmentRunState' "COMPLETED"

pattern CompletedWithErrors :: AssessmentRunState
pattern CompletedWithErrors = AssessmentRunState' "COMPLETED_WITH_ERRORS"

pattern Created :: AssessmentRunState
pattern Created = AssessmentRunState' "CREATED"

pattern DataCollected :: AssessmentRunState
pattern DataCollected = AssessmentRunState' "DATA_COLLECTED"

pattern Error :: AssessmentRunState
pattern Error = AssessmentRunState' "ERROR"

pattern EvaluatingRules :: AssessmentRunState
pattern EvaluatingRules = AssessmentRunState' "EVALUATING_RULES"

pattern Failed :: AssessmentRunState
pattern Failed = AssessmentRunState' "FAILED"

pattern StartDataCollectionInProgress :: AssessmentRunState
pattern StartDataCollectionInProgress = AssessmentRunState' "START_DATA_COLLECTION_IN_PROGRESS"

pattern StartDataCollectionPending :: AssessmentRunState
pattern StartDataCollectionPending = AssessmentRunState' "START_DATA_COLLECTION_PENDING"

pattern StartEvaluatingRulesPending :: AssessmentRunState
pattern StartEvaluatingRulesPending = AssessmentRunState' "START_EVALUATING_RULES_PENDING"

pattern StopDataCollectionPending :: AssessmentRunState
pattern StopDataCollectionPending = AssessmentRunState' "STOP_DATA_COLLECTION_PENDING"

{-# COMPLETE
  Canceled,
  CollectingData,
  Completed,
  CompletedWithErrors,
  Created,
  DataCollected,
  Error,
  EvaluatingRules,
  Failed,
  StartDataCollectionInProgress,
  StartDataCollectionPending,
  StartEvaluatingRulesPending,
  StopDataCollectionPending,
  AssessmentRunState'
  #-}
