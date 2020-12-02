{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ChannelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ChannelType where

import Network.AWS.Prelude

data ChannelType
  = Facebook
  | Kik
  | Slack
  | TwilioSms
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

instance FromText ChannelType where
  parser =
    takeLowerText >>= \case
      "facebook" -> pure Facebook
      "kik" -> pure Kik
      "slack" -> pure Slack
      "twilio-sms" -> pure TwilioSms
      e ->
        fromTextError $
          "Failure parsing ChannelType from value: '" <> e
            <> "'. Accepted values: facebook, kik, slack, twilio-sms"

instance ToText ChannelType where
  toText = \case
    Facebook -> "Facebook"
    Kik -> "Kik"
    Slack -> "Slack"
    TwilioSms -> "Twilio-Sms"

instance Hashable ChannelType

instance NFData ChannelType

instance ToByteString ChannelType

instance ToQuery ChannelType

instance ToHeader ChannelType

instance FromJSON ChannelType where
  parseJSON = parseJSONText "ChannelType"
