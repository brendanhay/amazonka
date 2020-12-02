{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForUdpOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForUdpOut where

import Network.AWS.Prelude

-- | Input Loss Action For Udp Out
data InputLossActionForUdpOut
  = DropProgram
  | DropTs
  | EmitProgram
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

instance FromText InputLossActionForUdpOut where
  parser =
    takeLowerText >>= \case
      "drop_program" -> pure DropProgram
      "drop_ts" -> pure DropTs
      "emit_program" -> pure EmitProgram
      e ->
        fromTextError $
          "Failure parsing InputLossActionForUdpOut from value: '" <> e
            <> "'. Accepted values: drop_program, drop_ts, emit_program"

instance ToText InputLossActionForUdpOut where
  toText = \case
    DropProgram -> "DROP_PROGRAM"
    DropTs -> "DROP_TS"
    EmitProgram -> "EMIT_PROGRAM"

instance Hashable InputLossActionForUdpOut

instance NFData InputLossActionForUdpOut

instance ToByteString InputLossActionForUdpOut

instance ToQuery InputLossActionForUdpOut

instance ToHeader InputLossActionForUdpOut

instance ToJSON InputLossActionForUdpOut where
  toJSON = toJSONText

instance FromJSON InputLossActionForUdpOut where
  parseJSON = parseJSONText "InputLossActionForUdpOut"
