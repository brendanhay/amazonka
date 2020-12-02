{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPStatus where

import Network.AWS.Prelude

data BGPStatus
  = Down
  | UP
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

instance FromText BGPStatus where
  parser =
    takeLowerText >>= \case
      "down" -> pure Down
      "up" -> pure UP
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing BGPStatus from value: '" <> e
            <> "'. Accepted values: down, up, unknown"

instance ToText BGPStatus where
  toText = \case
    Down -> "down"
    UP -> "up"
    Unknown -> "unknown"

instance Hashable BGPStatus

instance NFData BGPStatus

instance ToByteString BGPStatus

instance ToQuery BGPStatus

instance ToHeader BGPStatus

instance FromJSON BGPStatus where
  parseJSON = parseJSONText "BGPStatus"
