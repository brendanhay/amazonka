{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AuthorizerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.AuthorizerType where

import Network.AWS.Prelude

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
data AuthorizerType
  = AuthorizerCognitoUserPools
  | AuthorizerRequest
  | AuthorizerToken
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

instance FromText AuthorizerType where
  parser =
    takeLowerText >>= \case
      "cognito_user_pools" -> pure AuthorizerCognitoUserPools
      "request" -> pure AuthorizerRequest
      "token" -> pure AuthorizerToken
      e ->
        fromTextError $
          "Failure parsing AuthorizerType from value: '" <> e
            <> "'. Accepted values: cognito_user_pools, request, token"

instance ToText AuthorizerType where
  toText = \case
    AuthorizerCognitoUserPools -> "COGNITO_USER_POOLS"
    AuthorizerRequest -> "REQUEST"
    AuthorizerToken -> "TOKEN"

instance Hashable AuthorizerType

instance NFData AuthorizerType

instance ToByteString AuthorizerType

instance ToQuery AuthorizerType

instance ToHeader AuthorizerType

instance ToJSON AuthorizerType where
  toJSON = toJSONText

instance FromJSON AuthorizerType where
  parseJSON = parseJSONText "AuthorizerType"
