{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactProtocol where

import Network.AWS.Prelude

data ContactProtocol
  = Email
  | Sms
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

instance FromText ContactProtocol where
  parser =
    takeLowerText >>= \case
      "email" -> pure Email
      "sms" -> pure Sms
      e ->
        fromTextError $
          "Failure parsing ContactProtocol from value: '" <> e
            <> "'. Accepted values: email, sms"

instance ToText ContactProtocol where
  toText = \case
    Email -> "Email"
    Sms -> "SMS"

instance Hashable ContactProtocol

instance NFData ContactProtocol

instance ToByteString ContactProtocol

instance ToQuery ContactProtocol

instance ToHeader ContactProtocol

instance ToJSON ContactProtocol where
  toJSON = toJSONText

instance FromJSON ContactProtocol where
  parseJSON = parseJSONText "ContactProtocol"
