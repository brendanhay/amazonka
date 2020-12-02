{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut where

import Network.AWS.Prelude

-- | Input Loss Action For Ms Smooth Out
data InputLossActionForMsSmoothOut
  = EmitOutput
  | PauseOutput
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

instance FromText InputLossActionForMsSmoothOut where
  parser =
    takeLowerText >>= \case
      "emit_output" -> pure EmitOutput
      "pause_output" -> pure PauseOutput
      e ->
        fromTextError $
          "Failure parsing InputLossActionForMsSmoothOut from value: '" <> e
            <> "'. Accepted values: emit_output, pause_output"

instance ToText InputLossActionForMsSmoothOut where
  toText = \case
    EmitOutput -> "EMIT_OUTPUT"
    PauseOutput -> "PAUSE_OUTPUT"

instance Hashable InputLossActionForMsSmoothOut

instance NFData InputLossActionForMsSmoothOut

instance ToByteString InputLossActionForMsSmoothOut

instance ToQuery InputLossActionForMsSmoothOut

instance ToHeader InputLossActionForMsSmoothOut

instance ToJSON InputLossActionForMsSmoothOut where
  toJSON = toJSONText

instance FromJSON InputLossActionForMsSmoothOut where
  parseJSON = parseJSONText "InputLossActionForMsSmoothOut"
