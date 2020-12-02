{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType where

import Network.AWS.Prelude

data IdentityProviderTypeType
  = Facebook
  | Google
  | LoginWithAmazon
  | Oidc
  | Saml
  | SignInWithApple
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

instance FromText IdentityProviderTypeType where
  parser =
    takeLowerText >>= \case
      "facebook" -> pure Facebook
      "google" -> pure Google
      "loginwithamazon" -> pure LoginWithAmazon
      "oidc" -> pure Oidc
      "saml" -> pure Saml
      "signinwithapple" -> pure SignInWithApple
      e ->
        fromTextError $
          "Failure parsing IdentityProviderTypeType from value: '" <> e
            <> "'. Accepted values: facebook, google, loginwithamazon, oidc, saml, signinwithapple"

instance ToText IdentityProviderTypeType where
  toText = \case
    Facebook -> "Facebook"
    Google -> "Google"
    LoginWithAmazon -> "LoginWithAmazon"
    Oidc -> "OIDC"
    Saml -> "SAML"
    SignInWithApple -> "SignInWithApple"

instance Hashable IdentityProviderTypeType

instance NFData IdentityProviderTypeType

instance ToByteString IdentityProviderTypeType

instance ToQuery IdentityProviderTypeType

instance ToHeader IdentityProviderTypeType

instance ToJSON IdentityProviderTypeType where
  toJSON = toJSONText

instance FromJSON IdentityProviderTypeType where
  parseJSON = parseJSONText "IdentityProviderTypeType"
