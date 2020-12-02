{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PingStatus where

import Network.AWS.Prelude

data PingStatus
  = ConnectionLost
  | Inactive
  | Online
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

instance FromText PingStatus where
  parser =
    takeLowerText >>= \case
      "connectionlost" -> pure ConnectionLost
      "inactive" -> pure Inactive
      "online" -> pure Online
      e ->
        fromTextError $
          "Failure parsing PingStatus from value: '" <> e
            <> "'. Accepted values: connectionlost, inactive, online"

instance ToText PingStatus where
  toText = \case
    ConnectionLost -> "ConnectionLost"
    Inactive -> "Inactive"
    Online -> "Online"

instance Hashable PingStatus

instance NFData PingStatus

instance ToByteString PingStatus

instance ToQuery PingStatus

instance ToHeader PingStatus

instance FromJSON PingStatus where
  parseJSON = parseJSONText "PingStatus"
