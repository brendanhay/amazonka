{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.ConnectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.ConnectionType where

import Network.AWS.Prelude

data ConnectionType
  = ConnectSSH
  | ConnectSsm
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

instance FromText ConnectionType where
  parser =
    takeLowerText >>= \case
      "connect_ssh" -> pure ConnectSSH
      "connect_ssm" -> pure ConnectSsm
      e ->
        fromTextError $
          "Failure parsing ConnectionType from value: '" <> e
            <> "'. Accepted values: connect_ssh, connect_ssm"

instance ToText ConnectionType where
  toText = \case
    ConnectSSH -> "CONNECT_SSH"
    ConnectSsm -> "CONNECT_SSM"

instance Hashable ConnectionType

instance NFData ConnectionType

instance ToByteString ConnectionType

instance ToQuery ConnectionType

instance ToHeader ConnectionType

instance ToJSON ConnectionType where
  toJSON = toJSONText

instance FromJSON ConnectionType where
  parseJSON = parseJSONText "ConnectionType"
