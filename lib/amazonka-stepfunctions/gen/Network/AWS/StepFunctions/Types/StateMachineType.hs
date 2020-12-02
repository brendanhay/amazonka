{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateMachineType where

import Network.AWS.Prelude

data StateMachineType
  = Express
  | Standard
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

instance FromText StateMachineType where
  parser =
    takeLowerText >>= \case
      "express" -> pure Express
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing StateMachineType from value: '" <> e
            <> "'. Accepted values: express, standard"

instance ToText StateMachineType where
  toText = \case
    Express -> "EXPRESS"
    Standard -> "STANDARD"

instance Hashable StateMachineType

instance NFData StateMachineType

instance ToByteString StateMachineType

instance ToQuery StateMachineType

instance ToHeader StateMachineType

instance ToJSON StateMachineType where
  toJSON = toJSONText

instance FromJSON StateMachineType where
  parseJSON = parseJSONText "StateMachineType"
