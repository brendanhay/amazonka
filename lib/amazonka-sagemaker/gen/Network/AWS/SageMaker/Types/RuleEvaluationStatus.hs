{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RuleEvaluationStatus
  ( RuleEvaluationStatus
      ( RuleEvaluationStatus',
        RuleEvaluationStatusInProgress,
        RuleEvaluationStatusNoIssuesFound,
        RuleEvaluationStatusIssuesFound,
        RuleEvaluationStatusError,
        RuleEvaluationStatusStopping,
        RuleEvaluationStatusStopped,
        fromRuleEvaluationStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RuleEvaluationStatus = RuleEvaluationStatus'
  { fromRuleEvaluationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern RuleEvaluationStatusInProgress :: RuleEvaluationStatus
pattern RuleEvaluationStatusInProgress = RuleEvaluationStatus' "InProgress"

pattern RuleEvaluationStatusNoIssuesFound :: RuleEvaluationStatus
pattern RuleEvaluationStatusNoIssuesFound = RuleEvaluationStatus' "NoIssuesFound"

pattern RuleEvaluationStatusIssuesFound :: RuleEvaluationStatus
pattern RuleEvaluationStatusIssuesFound = RuleEvaluationStatus' "IssuesFound"

pattern RuleEvaluationStatusError :: RuleEvaluationStatus
pattern RuleEvaluationStatusError = RuleEvaluationStatus' "Error"

pattern RuleEvaluationStatusStopping :: RuleEvaluationStatus
pattern RuleEvaluationStatusStopping = RuleEvaluationStatus' "Stopping"

pattern RuleEvaluationStatusStopped :: RuleEvaluationStatus
pattern RuleEvaluationStatusStopped = RuleEvaluationStatus' "Stopped"

{-# COMPLETE
  RuleEvaluationStatusInProgress,
  RuleEvaluationStatusNoIssuesFound,
  RuleEvaluationStatusIssuesFound,
  RuleEvaluationStatusError,
  RuleEvaluationStatusStopping,
  RuleEvaluationStatusStopped,
  RuleEvaluationStatus'
  #-}
