{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RuleEvaluationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RuleEvaluationStatus
  ( RuleEvaluationStatus
      ( ..,
        RuleEvaluationStatus_Error,
        RuleEvaluationStatus_InProgress,
        RuleEvaluationStatus_IssuesFound,
        RuleEvaluationStatus_NoIssuesFound,
        RuleEvaluationStatus_Stopped,
        RuleEvaluationStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RuleEvaluationStatus = RuleEvaluationStatus'
  { fromRuleEvaluationStatus ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern RuleEvaluationStatus_Error :: RuleEvaluationStatus
pattern RuleEvaluationStatus_Error = RuleEvaluationStatus' "Error"

pattern RuleEvaluationStatus_InProgress :: RuleEvaluationStatus
pattern RuleEvaluationStatus_InProgress = RuleEvaluationStatus' "InProgress"

pattern RuleEvaluationStatus_IssuesFound :: RuleEvaluationStatus
pattern RuleEvaluationStatus_IssuesFound = RuleEvaluationStatus' "IssuesFound"

pattern RuleEvaluationStatus_NoIssuesFound :: RuleEvaluationStatus
pattern RuleEvaluationStatus_NoIssuesFound = RuleEvaluationStatus' "NoIssuesFound"

pattern RuleEvaluationStatus_Stopped :: RuleEvaluationStatus
pattern RuleEvaluationStatus_Stopped = RuleEvaluationStatus' "Stopped"

pattern RuleEvaluationStatus_Stopping :: RuleEvaluationStatus
pattern RuleEvaluationStatus_Stopping = RuleEvaluationStatus' "Stopping"

{-# COMPLETE
  RuleEvaluationStatus_Error,
  RuleEvaluationStatus_InProgress,
  RuleEvaluationStatus_IssuesFound,
  RuleEvaluationStatus_NoIssuesFound,
  RuleEvaluationStatus_Stopped,
  RuleEvaluationStatus_Stopping,
  RuleEvaluationStatus'
  #-}
