{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RuleEvaluationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RuleEvaluationStatus where

import Network.AWS.Prelude

data RuleEvaluationStatus
  = Error'
  | InProgress
  | IssuesFound
  | NoIssuesFound
  | Stopped
  | Stopping
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

instance FromText RuleEvaluationStatus where
  parser =
    takeLowerText >>= \case
      "error" -> pure Error'
      "inprogress" -> pure InProgress
      "issuesfound" -> pure IssuesFound
      "noissuesfound" -> pure NoIssuesFound
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      e ->
        fromTextError $
          "Failure parsing RuleEvaluationStatus from value: '" <> e
            <> "'. Accepted values: error, inprogress, issuesfound, noissuesfound, stopped, stopping"

instance ToText RuleEvaluationStatus where
  toText = \case
    Error' -> "Error"
    InProgress -> "InProgress"
    IssuesFound -> "IssuesFound"
    NoIssuesFound -> "NoIssuesFound"
    Stopped -> "Stopped"
    Stopping -> "Stopping"

instance Hashable RuleEvaluationStatus

instance NFData RuleEvaluationStatus

instance ToByteString RuleEvaluationStatus

instance ToQuery RuleEvaluationStatus

instance ToHeader RuleEvaluationStatus

instance FromJSON RuleEvaluationStatus where
  parseJSON = parseJSONText "RuleEvaluationStatus"
