{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag where

import Network.AWS.Prelude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35WebDeliveryAllowedFlag
  = WebDeliveryAllowed
  | WebDeliveryNotAllowed
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

instance FromText Scte35WebDeliveryAllowedFlag where
  parser =
    takeLowerText >>= \case
      "web_delivery_allowed" -> pure WebDeliveryAllowed
      "web_delivery_not_allowed" -> pure WebDeliveryNotAllowed
      e ->
        fromTextError $
          "Failure parsing Scte35WebDeliveryAllowedFlag from value: '" <> e
            <> "'. Accepted values: web_delivery_allowed, web_delivery_not_allowed"

instance ToText Scte35WebDeliveryAllowedFlag where
  toText = \case
    WebDeliveryAllowed -> "WEB_DELIVERY_ALLOWED"
    WebDeliveryNotAllowed -> "WEB_DELIVERY_NOT_ALLOWED"

instance Hashable Scte35WebDeliveryAllowedFlag

instance NFData Scte35WebDeliveryAllowedFlag

instance ToByteString Scte35WebDeliveryAllowedFlag

instance ToQuery Scte35WebDeliveryAllowedFlag

instance ToHeader Scte35WebDeliveryAllowedFlag

instance ToJSON Scte35WebDeliveryAllowedFlag where
  toJSON = toJSONText

instance FromJSON Scte35WebDeliveryAllowedFlag where
  parseJSON = parseJSONText "Scte35WebDeliveryAllowedFlag"
