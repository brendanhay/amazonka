{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType where

import Network.AWS.Prelude

data EmailSendingAccountType
  = CognitoDefault
  | Developer
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

instance FromText EmailSendingAccountType where
  parser =
    takeLowerText >>= \case
      "cognito_default" -> pure CognitoDefault
      "developer" -> pure Developer
      e ->
        fromTextError $
          "Failure parsing EmailSendingAccountType from value: '" <> e
            <> "'. Accepted values: cognito_default, developer"

instance ToText EmailSendingAccountType where
  toText = \case
    CognitoDefault -> "COGNITO_DEFAULT"
    Developer -> "DEVELOPER"

instance Hashable EmailSendingAccountType

instance NFData EmailSendingAccountType

instance ToByteString EmailSendingAccountType

instance ToQuery EmailSendingAccountType

instance ToHeader EmailSendingAccountType

instance ToJSON EmailSendingAccountType where
  toJSON = toJSONText

instance FromJSON EmailSendingAccountType where
  parseJSON = parseJSONText "EmailSendingAccountType"
