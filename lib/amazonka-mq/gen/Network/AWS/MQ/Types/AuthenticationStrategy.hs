{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.AuthenticationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.AuthenticationStrategy where

import Network.AWS.Prelude

-- | The authentication strategy used to secure the broker.
data AuthenticationStrategy
  = Ldap
  | Simple
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

instance FromText AuthenticationStrategy where
  parser =
    takeLowerText >>= \case
      "ldap" -> pure Ldap
      "simple" -> pure Simple
      e ->
        fromTextError $
          "Failure parsing AuthenticationStrategy from value: '" <> e
            <> "'. Accepted values: ldap, simple"

instance ToText AuthenticationStrategy where
  toText = \case
    Ldap -> "LDAP"
    Simple -> "SIMPLE"

instance Hashable AuthenticationStrategy

instance NFData AuthenticationStrategy

instance ToByteString AuthenticationStrategy

instance ToQuery AuthenticationStrategy

instance ToHeader AuthenticationStrategy

instance ToJSON AuthenticationStrategy where
  toJSON = toJSONText

instance FromJSON AuthenticationStrategy where
  parseJSON = parseJSONText "AuthenticationStrategy"
