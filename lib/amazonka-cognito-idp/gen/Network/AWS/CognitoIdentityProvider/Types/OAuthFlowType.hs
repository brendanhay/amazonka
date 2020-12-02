{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType where

import Network.AWS.Prelude

data OAuthFlowType
  = ClientCredentials
  | Code
  | Implicit
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

instance FromText OAuthFlowType where
  parser =
    takeLowerText >>= \case
      "client_credentials" -> pure ClientCredentials
      "code" -> pure Code
      "implicit" -> pure Implicit
      e ->
        fromTextError $
          "Failure parsing OAuthFlowType from value: '" <> e
            <> "'. Accepted values: client_credentials, code, implicit"

instance ToText OAuthFlowType where
  toText = \case
    ClientCredentials -> "client_credentials"
    Code -> "code"
    Implicit -> "implicit"

instance Hashable OAuthFlowType

instance NFData OAuthFlowType

instance ToByteString OAuthFlowType

instance ToQuery OAuthFlowType

instance ToHeader OAuthFlowType

instance ToJSON OAuthFlowType where
  toJSON = toJSONText

instance FromJSON OAuthFlowType where
  parseJSON = parseJSONText "OAuthFlowType"
