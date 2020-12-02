{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthenticationType where

import Network.AWS.Prelude

data AuthenticationType
  = ATAPIKey
  | ATAWSIAM
  | ATAmazonCognitoUserPools
  | ATOpenidConnect
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

instance FromText AuthenticationType where
  parser =
    takeLowerText >>= \case
      "api_key" -> pure ATAPIKey
      "aws_iam" -> pure ATAWSIAM
      "amazon_cognito_user_pools" -> pure ATAmazonCognitoUserPools
      "openid_connect" -> pure ATOpenidConnect
      e ->
        fromTextError $
          "Failure parsing AuthenticationType from value: '" <> e
            <> "'. Accepted values: api_key, aws_iam, amazon_cognito_user_pools, openid_connect"

instance ToText AuthenticationType where
  toText = \case
    ATAPIKey -> "API_KEY"
    ATAWSIAM -> "AWS_IAM"
    ATAmazonCognitoUserPools -> "AMAZON_COGNITO_USER_POOLS"
    ATOpenidConnect -> "OPENID_CONNECT"

instance Hashable AuthenticationType

instance NFData AuthenticationType

instance ToByteString AuthenticationType

instance ToQuery AuthenticationType

instance ToHeader AuthenticationType

instance ToJSON AuthenticationType where
  toJSON = toJSONText

instance FromJSON AuthenticationType where
  parseJSON = parseJSONText "AuthenticationType"
