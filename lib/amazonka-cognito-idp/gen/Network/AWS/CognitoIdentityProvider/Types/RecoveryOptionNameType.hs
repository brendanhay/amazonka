{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType where

import Network.AWS.Prelude

data RecoveryOptionNameType
  = AdminOnly
  | VerifiedEmail
  | VerifiedPhoneNumber
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

instance FromText RecoveryOptionNameType where
  parser =
    takeLowerText >>= \case
      "admin_only" -> pure AdminOnly
      "verified_email" -> pure VerifiedEmail
      "verified_phone_number" -> pure VerifiedPhoneNumber
      e ->
        fromTextError $
          "Failure parsing RecoveryOptionNameType from value: '" <> e
            <> "'. Accepted values: admin_only, verified_email, verified_phone_number"

instance ToText RecoveryOptionNameType where
  toText = \case
    AdminOnly -> "admin_only"
    VerifiedEmail -> "verified_email"
    VerifiedPhoneNumber -> "verified_phone_number"

instance Hashable RecoveryOptionNameType

instance NFData RecoveryOptionNameType

instance ToByteString RecoveryOptionNameType

instance ToQuery RecoveryOptionNameType

instance ToHeader RecoveryOptionNameType

instance ToJSON RecoveryOptionNameType where
  toJSON = toJSONText

instance FromJSON RecoveryOptionNameType where
  parseJSON = parseJSONText "RecoveryOptionNameType"
