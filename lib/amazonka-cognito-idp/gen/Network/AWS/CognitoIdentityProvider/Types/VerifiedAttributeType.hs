{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType where

import Network.AWS.Prelude

data VerifiedAttributeType
  = Email
  | PhoneNumber
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

instance FromText VerifiedAttributeType where
  parser =
    takeLowerText >>= \case
      "email" -> pure Email
      "phone_number" -> pure PhoneNumber
      e ->
        fromTextError $
          "Failure parsing VerifiedAttributeType from value: '" <> e
            <> "'. Accepted values: email, phone_number"

instance ToText VerifiedAttributeType where
  toText = \case
    Email -> "email"
    PhoneNumber -> "phone_number"

instance Hashable VerifiedAttributeType

instance NFData VerifiedAttributeType

instance ToByteString VerifiedAttributeType

instance ToQuery VerifiedAttributeType

instance ToHeader VerifiedAttributeType

instance ToJSON VerifiedAttributeType where
  toJSON = toJSONText

instance FromJSON VerifiedAttributeType where
  parseJSON = parseJSONText "VerifiedAttributeType"
