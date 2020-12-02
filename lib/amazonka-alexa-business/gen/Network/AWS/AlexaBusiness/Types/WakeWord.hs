{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.WakeWord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.WakeWord where

import Network.AWS.Prelude

data WakeWord
  = Alexa
  | Amazon
  | Computer
  | Echo
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

instance FromText WakeWord where
  parser =
    takeLowerText >>= \case
      "alexa" -> pure Alexa
      "amazon" -> pure Amazon
      "computer" -> pure Computer
      "echo" -> pure Echo
      e ->
        fromTextError $
          "Failure parsing WakeWord from value: '" <> e
            <> "'. Accepted values: alexa, amazon, computer, echo"

instance ToText WakeWord where
  toText = \case
    Alexa -> "ALEXA"
    Amazon -> "AMAZON"
    Computer -> "COMPUTER"
    Echo -> "ECHO"

instance Hashable WakeWord

instance NFData WakeWord

instance ToByteString WakeWord

instance ToQuery WakeWord

instance ToHeader WakeWord

instance ToJSON WakeWord where
  toJSON = toJSONText

instance FromJSON WakeWord where
  parseJSON = parseJSONText "WakeWord"
