{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMappingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RoleMappingType where

import Network.AWS.Prelude

data RoleMappingType
  = Rules
  | Token
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

instance FromText RoleMappingType where
  parser =
    takeLowerText >>= \case
      "rules" -> pure Rules
      "token" -> pure Token
      e ->
        fromTextError $
          "Failure parsing RoleMappingType from value: '" <> e
            <> "'. Accepted values: rules, token"

instance ToText RoleMappingType where
  toText = \case
    Rules -> "Rules"
    Token -> "Token"

instance Hashable RoleMappingType

instance NFData RoleMappingType

instance ToByteString RoleMappingType

instance ToQuery RoleMappingType

instance ToHeader RoleMappingType

instance ToJSON RoleMappingType where
  toJSON = toJSONText

instance FromJSON RoleMappingType where
  parseJSON = parseJSONText "RoleMappingType"
