{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Fault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Fault where

import Network.AWS.Prelude

data Fault
  = Client
  | Server
  | Unknown
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

instance FromText Fault where
  parser =
    takeLowerText >>= \case
      "client" -> pure Client
      "server" -> pure Server
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing Fault from value: '" <> e
            <> "'. Accepted values: client, server, unknown"

instance ToText Fault where
  toText = \case
    Client -> "Client"
    Server -> "Server"
    Unknown -> "Unknown"

instance Hashable Fault

instance NFData Fault

instance ToByteString Fault

instance ToQuery Fault

instance ToHeader Fault

instance FromJSON Fault where
  parseJSON = parseJSONText "Fault"
