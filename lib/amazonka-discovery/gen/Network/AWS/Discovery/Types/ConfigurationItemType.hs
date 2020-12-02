{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ConfigurationItemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationItemType where

import Network.AWS.Prelude

data ConfigurationItemType
  = Application
  | Connection
  | Process
  | Server
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

instance FromText ConfigurationItemType where
  parser =
    takeLowerText >>= \case
      "application" -> pure Application
      "connection" -> pure Connection
      "process" -> pure Process
      "server" -> pure Server
      e ->
        fromTextError $
          "Failure parsing ConfigurationItemType from value: '" <> e
            <> "'. Accepted values: application, connection, process, server"

instance ToText ConfigurationItemType where
  toText = \case
    Application -> "APPLICATION"
    Connection -> "CONNECTION"
    Process -> "PROCESS"
    Server -> "SERVER"

instance Hashable ConfigurationItemType

instance NFData ConfigurationItemType

instance ToByteString ConfigurationItemType

instance ToQuery ConfigurationItemType

instance ToHeader ConfigurationItemType

instance ToJSON ConfigurationItemType where
  toJSON = toJSONText

instance FromJSON ConfigurationItemType where
  parseJSON = parseJSONText "ConfigurationItemType"
