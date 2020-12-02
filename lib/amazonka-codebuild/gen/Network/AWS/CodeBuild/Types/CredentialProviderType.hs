{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CredentialProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CredentialProviderType where

import Network.AWS.Prelude

data CredentialProviderType = SecretsManager
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

instance FromText CredentialProviderType where
  parser =
    takeLowerText >>= \case
      "secrets_manager" -> pure SecretsManager
      e ->
        fromTextError $
          "Failure parsing CredentialProviderType from value: '" <> e
            <> "'. Accepted values: secrets_manager"

instance ToText CredentialProviderType where
  toText = \case
    SecretsManager -> "SECRETS_MANAGER"

instance Hashable CredentialProviderType

instance NFData CredentialProviderType

instance ToByteString CredentialProviderType

instance ToQuery CredentialProviderType

instance ToHeader CredentialProviderType

instance ToJSON CredentialProviderType where
  toJSON = toJSONText

instance FromJSON CredentialProviderType where
  parseJSON = parseJSONText "CredentialProviderType"
