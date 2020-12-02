{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType where

import Network.AWS.Prelude

data UsernameAttributeType
  = UATEmail
  | UATPhoneNumber
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

instance FromText UsernameAttributeType where
  parser =
    takeLowerText >>= \case
      "email" -> pure UATEmail
      "phone_number" -> pure UATPhoneNumber
      e ->
        fromTextError $
          "Failure parsing UsernameAttributeType from value: '" <> e
            <> "'. Accepted values: email, phone_number"

instance ToText UsernameAttributeType where
  toText = \case
    UATEmail -> "email"
    UATPhoneNumber -> "phone_number"

instance Hashable UsernameAttributeType

instance NFData UsernameAttributeType

instance ToByteString UsernameAttributeType

instance ToQuery UsernameAttributeType

instance ToHeader UsernameAttributeType

instance ToJSON UsernameAttributeType where
  toJSON = toJSONText

instance FromJSON UsernameAttributeType where
  parseJSON = parseJSONText "UsernameAttributeType"
