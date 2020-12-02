{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFilterType where

import Network.AWS.Prelude

data EventFilterType
  = PasswordChange
  | SignIn
  | SignUp
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

instance FromText EventFilterType where
  parser =
    takeLowerText >>= \case
      "password_change" -> pure PasswordChange
      "sign_in" -> pure SignIn
      "sign_up" -> pure SignUp
      e ->
        fromTextError $
          "Failure parsing EventFilterType from value: '" <> e
            <> "'. Accepted values: password_change, sign_in, sign_up"

instance ToText EventFilterType where
  toText = \case
    PasswordChange -> "PASSWORD_CHANGE"
    SignIn -> "SIGN_IN"
    SignUp -> "SIGN_UP"

instance Hashable EventFilterType

instance NFData EventFilterType

instance ToByteString EventFilterType

instance ToQuery EventFilterType

instance ToHeader EventFilterType

instance ToJSON EventFilterType where
  toJSON = toJSONText

instance FromJSON EventFilterType where
  parseJSON = parseJSONText "EventFilterType"
