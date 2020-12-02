{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightState where

import Network.AWS.Prelude

data InsightState
  = ISActive
  | ISClosed
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

instance FromText InsightState where
  parser =
    takeLowerText >>= \case
      "active" -> pure ISActive
      "closed" -> pure ISClosed
      e ->
        fromTextError $
          "Failure parsing InsightState from value: '" <> e
            <> "'. Accepted values: active, closed"

instance ToText InsightState where
  toText = \case
    ISActive -> "ACTIVE"
    ISClosed -> "CLOSED"

instance Hashable InsightState

instance NFData InsightState

instance ToByteString InsightState

instance ToQuery InsightState

instance ToHeader InsightState

instance ToJSON InsightState where
  toJSON = toJSONText

instance FromJSON InsightState where
  parseJSON = parseJSONText "InsightState"
