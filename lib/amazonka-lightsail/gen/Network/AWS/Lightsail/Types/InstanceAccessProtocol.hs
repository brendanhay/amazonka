{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceAccessProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceAccessProtocol where

import Network.AWS.Prelude

data InstanceAccessProtocol
  = Rdp
  | SSH
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

instance FromText InstanceAccessProtocol where
  parser =
    takeLowerText >>= \case
      "rdp" -> pure Rdp
      "ssh" -> pure SSH
      e ->
        fromTextError $
          "Failure parsing InstanceAccessProtocol from value: '" <> e
            <> "'. Accepted values: rdp, ssh"

instance ToText InstanceAccessProtocol where
  toText = \case
    Rdp -> "rdp"
    SSH -> "ssh"

instance Hashable InstanceAccessProtocol

instance NFData InstanceAccessProtocol

instance ToByteString InstanceAccessProtocol

instance ToQuery InstanceAccessProtocol

instance ToHeader InstanceAccessProtocol

instance ToJSON InstanceAccessProtocol where
  toJSON = toJSONText

instance FromJSON InstanceAccessProtocol where
  parseJSON = parseJSONText "InstanceAccessProtocol"
