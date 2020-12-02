{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthenticationType where

import Network.AWS.Prelude

data AuthenticationType
  = NoPassword
  | Password
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
      "no-password" -> pure NoPassword
      "password" -> pure Password
      e ->
        fromTextError $
          "Failure parsing AuthenticationType from value: '" <> e
            <> "'. Accepted values: no-password, password"

instance ToText AuthenticationType where
  toText = \case
    NoPassword -> "no-password"
    Password -> "password"

instance Hashable AuthenticationType

instance NFData AuthenticationType

instance ToByteString AuthenticationType

instance ToQuery AuthenticationType

instance ToHeader AuthenticationType

instance FromXML AuthenticationType where
  parseXML = parseXMLText "AuthenticationType"
