{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode where

import Network.AWS.Prelude

data ScalingPlanStatusCode
  = SPSCActive
  | SPSCActiveWithProblems
  | SPSCCreationFailed
  | SPSCCreationInProgress
  | SPSCDeletionFailed
  | SPSCDeletionInProgress
  | SPSCUpdateFailed
  | SPSCUpdateInProgress
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

instance FromText ScalingPlanStatusCode where
  parser =
    takeLowerText >>= \case
      "active" -> pure SPSCActive
      "activewithproblems" -> pure SPSCActiveWithProblems
      "creationfailed" -> pure SPSCCreationFailed
      "creationinprogress" -> pure SPSCCreationInProgress
      "deletionfailed" -> pure SPSCDeletionFailed
      "deletioninprogress" -> pure SPSCDeletionInProgress
      "updatefailed" -> pure SPSCUpdateFailed
      "updateinprogress" -> pure SPSCUpdateInProgress
      e ->
        fromTextError $
          "Failure parsing ScalingPlanStatusCode from value: '" <> e
            <> "'. Accepted values: active, activewithproblems, creationfailed, creationinprogress, deletionfailed, deletioninprogress, updatefailed, updateinprogress"

instance ToText ScalingPlanStatusCode where
  toText = \case
    SPSCActive -> "Active"
    SPSCActiveWithProblems -> "ActiveWithProblems"
    SPSCCreationFailed -> "CreationFailed"
    SPSCCreationInProgress -> "CreationInProgress"
    SPSCDeletionFailed -> "DeletionFailed"
    SPSCDeletionInProgress -> "DeletionInProgress"
    SPSCUpdateFailed -> "UpdateFailed"
    SPSCUpdateInProgress -> "UpdateInProgress"

instance Hashable ScalingPlanStatusCode

instance NFData ScalingPlanStatusCode

instance ToByteString ScalingPlanStatusCode

instance ToQuery ScalingPlanStatusCode

instance ToHeader ScalingPlanStatusCode

instance FromJSON ScalingPlanStatusCode where
  parseJSON = parseJSONText "ScalingPlanStatusCode"
