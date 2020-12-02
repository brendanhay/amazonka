{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskType where

import Network.AWS.Prelude

data MaintenanceWindowTaskType
  = Automation
  | Lambda
  | RunCommand
  | StepFunctions
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

instance FromText MaintenanceWindowTaskType where
  parser =
    takeLowerText >>= \case
      "automation" -> pure Automation
      "lambda" -> pure Lambda
      "run_command" -> pure RunCommand
      "step_functions" -> pure StepFunctions
      e ->
        fromTextError $
          "Failure parsing MaintenanceWindowTaskType from value: '" <> e
            <> "'. Accepted values: automation, lambda, run_command, step_functions"

instance ToText MaintenanceWindowTaskType where
  toText = \case
    Automation -> "AUTOMATION"
    Lambda -> "LAMBDA"
    RunCommand -> "RUN_COMMAND"
    StepFunctions -> "STEP_FUNCTIONS"

instance Hashable MaintenanceWindowTaskType

instance NFData MaintenanceWindowTaskType

instance ToByteString MaintenanceWindowTaskType

instance ToQuery MaintenanceWindowTaskType

instance ToHeader MaintenanceWindowTaskType

instance ToJSON MaintenanceWindowTaskType where
  toJSON = toJSONText

instance FromJSON MaintenanceWindowTaskType where
  parseJSON = parseJSONText "MaintenanceWindowTaskType"
