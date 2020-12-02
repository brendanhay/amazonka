{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateMachineStatus where

import Network.AWS.Prelude

data StateMachineStatus
  = Active
  | Deleting
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

instance FromText StateMachineStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing StateMachineStatus from value: '" <> e
            <> "'. Accepted values: active, deleting"

instance ToText StateMachineStatus where
  toText = \case
    Active -> "ACTIVE"
    Deleting -> "DELETING"

instance Hashable StateMachineStatus

instance NFData StateMachineStatus

instance ToByteString StateMachineStatus

instance ToQuery StateMachineStatus

instance ToHeader StateMachineStatus

instance FromJSON StateMachineStatus where
  parseJSON = parseJSONText "StateMachineStatus"
