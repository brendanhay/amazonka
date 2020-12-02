{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.VoiceRecordingTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingTrack where

import Network.AWS.Prelude

data VoiceRecordingTrack
  = All
  | FromAgent
  | ToAgent
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

instance FromText VoiceRecordingTrack where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "from_agent" -> pure FromAgent
      "to_agent" -> pure ToAgent
      e ->
        fromTextError $
          "Failure parsing VoiceRecordingTrack from value: '" <> e
            <> "'. Accepted values: all, from_agent, to_agent"

instance ToText VoiceRecordingTrack where
  toText = \case
    All -> "ALL"
    FromAgent -> "FROM_AGENT"
    ToAgent -> "TO_AGENT"

instance Hashable VoiceRecordingTrack

instance NFData VoiceRecordingTrack

instance ToByteString VoiceRecordingTrack

instance ToQuery VoiceRecordingTrack

instance ToHeader VoiceRecordingTrack

instance ToJSON VoiceRecordingTrack where
  toJSON = toJSONText
