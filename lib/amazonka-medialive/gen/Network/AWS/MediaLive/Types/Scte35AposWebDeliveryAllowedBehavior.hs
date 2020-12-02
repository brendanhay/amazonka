{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior where

import Network.AWS.Prelude

-- | Scte35 Apos Web Delivery Allowed Behavior
data Scte35AposWebDeliveryAllowedBehavior
  = SAWDABFollow
  | SAWDABIgnore
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

instance FromText Scte35AposWebDeliveryAllowedBehavior where
  parser =
    takeLowerText >>= \case
      "follow" -> pure SAWDABFollow
      "ignore" -> pure SAWDABIgnore
      e ->
        fromTextError $
          "Failure parsing Scte35AposWebDeliveryAllowedBehavior from value: '" <> e
            <> "'. Accepted values: follow, ignore"

instance ToText Scte35AposWebDeliveryAllowedBehavior where
  toText = \case
    SAWDABFollow -> "FOLLOW"
    SAWDABIgnore -> "IGNORE"

instance Hashable Scte35AposWebDeliveryAllowedBehavior

instance NFData Scte35AposWebDeliveryAllowedBehavior

instance ToByteString Scte35AposWebDeliveryAllowedBehavior

instance ToQuery Scte35AposWebDeliveryAllowedBehavior

instance ToHeader Scte35AposWebDeliveryAllowedBehavior

instance ToJSON Scte35AposWebDeliveryAllowedBehavior where
  toJSON = toJSONText

instance FromJSON Scte35AposWebDeliveryAllowedBehavior where
  parseJSON = parseJSONText "Scte35AposWebDeliveryAllowedBehavior"
