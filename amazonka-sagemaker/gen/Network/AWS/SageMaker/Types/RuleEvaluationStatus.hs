{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype RuleEvaluationStatus = RuleEvaluationStatus'
  { fromRuleEvaluationStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
