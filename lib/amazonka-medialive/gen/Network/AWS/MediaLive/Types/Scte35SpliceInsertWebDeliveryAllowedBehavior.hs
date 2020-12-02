{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior where

import Network.AWS.Prelude

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
data Scte35SpliceInsertWebDeliveryAllowedBehavior
  = SSIWDABFollow
  | SSIWDABIgnore
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

instance FromText Scte35SpliceInsertWebDeliveryAllowedBehavior where
  parser =
    takeLowerText >>= \case
      "follow" -> pure SSIWDABFollow
      "ignore" -> pure SSIWDABIgnore
      e ->
        fromTextError $
          "Failure parsing Scte35SpliceInsertWebDeliveryAllowedBehavior from value: '" <> e
            <> "'. Accepted values: follow, ignore"

instance ToText Scte35SpliceInsertWebDeliveryAllowedBehavior where
  toText = \case
    SSIWDABFollow -> "FOLLOW"
    SSIWDABIgnore -> "IGNORE"

instance Hashable Scte35SpliceInsertWebDeliveryAllowedBehavior

instance NFData Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToByteString Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToQuery Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToHeader Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
  toJSON = toJSONText

instance FromJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
  parseJSON = parseJSONText "Scte35SpliceInsertWebDeliveryAllowedBehavior"
