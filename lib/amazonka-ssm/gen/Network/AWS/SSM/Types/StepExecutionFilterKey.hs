{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecutionFilterKey where

import Network.AWS.Prelude

data StepExecutionFilterKey
  = Action
  | StartTimeAfter
  | StartTimeBefore
  | StepExecutionId
  | StepExecutionStatus
  | StepName
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

instance FromText StepExecutionFilterKey where
  parser =
    takeLowerText >>= \case
      "action" -> pure Action
      "starttimeafter" -> pure StartTimeAfter
      "starttimebefore" -> pure StartTimeBefore
      "stepexecutionid" -> pure StepExecutionId
      "stepexecutionstatus" -> pure StepExecutionStatus
      "stepname" -> pure StepName
      e ->
        fromTextError $
          "Failure parsing StepExecutionFilterKey from value: '" <> e
            <> "'. Accepted values: action, starttimeafter, starttimebefore, stepexecutionid, stepexecutionstatus, stepname"

instance ToText StepExecutionFilterKey where
  toText = \case
    Action -> "Action"
    StartTimeAfter -> "StartTimeAfter"
    StartTimeBefore -> "StartTimeBefore"
    StepExecutionId -> "StepExecutionId"
    StepExecutionStatus -> "StepExecutionStatus"
    StepName -> "StepName"

instance Hashable StepExecutionFilterKey

instance NFData StepExecutionFilterKey

instance ToByteString StepExecutionFilterKey

instance ToQuery StepExecutionFilterKey

instance ToHeader StepExecutionFilterKey

instance ToJSON StepExecutionFilterKey where
  toJSON = toJSONText
