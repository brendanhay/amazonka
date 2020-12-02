{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType where

import Network.AWS.Prelude

data AliasAttributeType
  = AATEmail
  | AATPhoneNumber
  | AATPreferredUsername
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

instance FromText AliasAttributeType where
  parser =
    takeLowerText >>= \case
      "email" -> pure AATEmail
      "phone_number" -> pure AATPhoneNumber
      "preferred_username" -> pure AATPreferredUsername
      e ->
        fromTextError $
          "Failure parsing AliasAttributeType from value: '" <> e
            <> "'. Accepted values: email, phone_number, preferred_username"

instance ToText AliasAttributeType where
  toText = \case
    AATEmail -> "email"
    AATPhoneNumber -> "phone_number"
    AATPreferredUsername -> "preferred_username"

instance Hashable AliasAttributeType

instance NFData AliasAttributeType

instance ToByteString AliasAttributeType

instance ToQuery AliasAttributeType

instance ToHeader AliasAttributeType

instance ToJSON AliasAttributeType where
  toJSON = toJSONText

instance FromJSON AliasAttributeType where
  parseJSON = parseJSONText "AliasAttributeType"
