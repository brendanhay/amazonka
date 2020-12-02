{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForHlsOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForHlsOut where

import Network.AWS.Prelude

-- | Input Loss Action For Hls Out
data InputLossActionForHlsOut
  = ILAFHOEmitOutput
  | ILAFHOPauseOutput
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

instance FromText InputLossActionForHlsOut where
  parser =
    takeLowerText >>= \case
      "emit_output" -> pure ILAFHOEmitOutput
      "pause_output" -> pure ILAFHOPauseOutput
      e ->
        fromTextError $
          "Failure parsing InputLossActionForHlsOut from value: '" <> e
            <> "'. Accepted values: emit_output, pause_output"

instance ToText InputLossActionForHlsOut where
  toText = \case
    ILAFHOEmitOutput -> "EMIT_OUTPUT"
    ILAFHOPauseOutput -> "PAUSE_OUTPUT"

instance Hashable InputLossActionForHlsOut

instance NFData InputLossActionForHlsOut

instance ToByteString InputLossActionForHlsOut

instance ToQuery InputLossActionForHlsOut

instance ToHeader InputLossActionForHlsOut

instance ToJSON InputLossActionForHlsOut where
  toJSON = toJSONText

instance FromJSON InputLossActionForHlsOut where
  parseJSON = parseJSONText "InputLossActionForHlsOut"
