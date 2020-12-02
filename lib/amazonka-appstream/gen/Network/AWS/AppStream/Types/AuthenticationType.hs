{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AuthenticationType where

import Network.AWS.Prelude

data AuthenticationType
  = API
  | Saml
  | Userpool
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
      "api" -> pure API
      "saml" -> pure Saml
      "userpool" -> pure Userpool
      e ->
        fromTextError $
          "Failure parsing AuthenticationType from value: '" <> e
            <> "'. Accepted values: api, saml, userpool"

instance ToText AuthenticationType where
  toText = \case
    API -> "API"
    Saml -> "SAML"
    Userpool -> "USERPOOL"

instance Hashable AuthenticationType

instance NFData AuthenticationType

instance ToByteString AuthenticationType

instance ToQuery AuthenticationType

instance ToHeader AuthenticationType

instance ToJSON AuthenticationType where
  toJSON = toJSONText

instance FromJSON AuthenticationType where
  parseJSON = parseJSONText "AuthenticationType"
