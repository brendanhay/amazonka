{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForRtmpOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForRtmpOut where

import Network.AWS.Prelude

-- | Input Loss Action For Rtmp Out
data InputLossActionForRtmpOut
  = ILAFROEmitOutput
  | ILAFROPauseOutput
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

instance FromText InputLossActionForRtmpOut where
  parser =
    takeLowerText >>= \case
      "emit_output" -> pure ILAFROEmitOutput
      "pause_output" -> pure ILAFROPauseOutput
      e ->
        fromTextError $
          "Failure parsing InputLossActionForRtmpOut from value: '" <> e
            <> "'. Accepted values: emit_output, pause_output"

instance ToText InputLossActionForRtmpOut where
  toText = \case
    ILAFROEmitOutput -> "EMIT_OUTPUT"
    ILAFROPauseOutput -> "PAUSE_OUTPUT"

instance Hashable InputLossActionForRtmpOut

instance NFData InputLossActionForRtmpOut

instance ToByteString InputLossActionForRtmpOut

instance ToQuery InputLossActionForRtmpOut

instance ToHeader InputLossActionForRtmpOut

instance ToJSON InputLossActionForRtmpOut where
  toJSON = toJSONText

instance FromJSON InputLossActionForRtmpOut where
  parseJSON = parseJSONText "InputLossActionForRtmpOut"
