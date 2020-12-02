{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventType where

import Network.AWS.Prelude

data EventType
  = ETForgotPassword
  | ETSignIn
  | ETSignUp
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "forgotpassword" -> pure ETForgotPassword
      "signin" -> pure ETSignIn
      "signup" -> pure ETSignUp
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: forgotpassword, signin, signup"

instance ToText EventType where
  toText = \case
    ETForgotPassword -> "ForgotPassword"
    ETSignIn -> "SignIn"
    ETSignUp -> "SignUp"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromJSON EventType where
  parseJSON = parseJSONText "EventType"
