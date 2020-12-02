{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CommsProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CommsProtocol where

import Network.AWS.Prelude

data CommsProtocol
  = H323
  | Sip
  | Sips
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

instance FromText CommsProtocol where
  parser =
    takeLowerText >>= \case
      "h323" -> pure H323
      "sip" -> pure Sip
      "sips" -> pure Sips
      e ->
        fromTextError $
          "Failure parsing CommsProtocol from value: '" <> e
            <> "'. Accepted values: h323, sip, sips"

instance ToText CommsProtocol where
  toText = \case
    H323 -> "H323"
    Sip -> "SIP"
    Sips -> "SIPS"

instance Hashable CommsProtocol

instance NFData CommsProtocol

instance ToByteString CommsProtocol

instance ToQuery CommsProtocol

instance ToHeader CommsProtocol

instance ToJSON CommsProtocol where
  toJSON = toJSONText

instance FromJSON CommsProtocol where
  parseJSON = parseJSONText "CommsProtocol"
