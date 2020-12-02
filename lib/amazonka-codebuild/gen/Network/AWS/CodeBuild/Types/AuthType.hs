{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.AuthType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.AuthType where

import Network.AWS.Prelude

data AuthType
  = ATBasicAuth
  | ATOauth
  | ATPersonalAccessToken
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

instance FromText AuthType where
  parser =
    takeLowerText >>= \case
      "basic_auth" -> pure ATBasicAuth
      "oauth" -> pure ATOauth
      "personal_access_token" -> pure ATPersonalAccessToken
      e ->
        fromTextError $
          "Failure parsing AuthType from value: '" <> e
            <> "'. Accepted values: basic_auth, oauth, personal_access_token"

instance ToText AuthType where
  toText = \case
    ATBasicAuth -> "BASIC_AUTH"
    ATOauth -> "OAUTH"
    ATPersonalAccessToken -> "PERSONAL_ACCESS_TOKEN"

instance Hashable AuthType

instance NFData AuthType

instance ToByteString AuthType

instance ToQuery AuthType

instance ToHeader AuthType

instance ToJSON AuthType where
  toJSON = toJSONText

instance FromJSON AuthType where
  parseJSON = parseJSONText "AuthType"
