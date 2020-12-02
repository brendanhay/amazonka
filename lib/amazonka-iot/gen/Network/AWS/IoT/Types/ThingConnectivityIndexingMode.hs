{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingConnectivityIndexingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingConnectivityIndexingMode where

import Network.AWS.Prelude

data ThingConnectivityIndexingMode
  = TCIMOff
  | TCIMStatus
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

instance FromText ThingConnectivityIndexingMode where
  parser =
    takeLowerText >>= \case
      "off" -> pure TCIMOff
      "status" -> pure TCIMStatus
      e ->
        fromTextError $
          "Failure parsing ThingConnectivityIndexingMode from value: '" <> e
            <> "'. Accepted values: off, status"

instance ToText ThingConnectivityIndexingMode where
  toText = \case
    TCIMOff -> "OFF"
    TCIMStatus -> "STATUS"

instance Hashable ThingConnectivityIndexingMode

instance NFData ThingConnectivityIndexingMode

instance ToByteString ThingConnectivityIndexingMode

instance ToQuery ThingConnectivityIndexingMode

instance ToHeader ThingConnectivityIndexingMode

instance ToJSON ThingConnectivityIndexingMode where
  toJSON = toJSONText

instance FromJSON ThingConnectivityIndexingMode where
  parseJSON = parseJSONText "ThingConnectivityIndexingMode"
