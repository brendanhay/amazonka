{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus where

import Network.AWS.Prelude

data RealtimeMetricsSubscriptionStatus
  = Disabled
  | Enabled
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

instance FromText RealtimeMetricsSubscriptionStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing RealtimeMetricsSubscriptionStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText RealtimeMetricsSubscriptionStatus where
  toText = \case
    Disabled -> "Disabled"
    Enabled -> "Enabled"

instance Hashable RealtimeMetricsSubscriptionStatus

instance NFData RealtimeMetricsSubscriptionStatus

instance ToByteString RealtimeMetricsSubscriptionStatus

instance ToQuery RealtimeMetricsSubscriptionStatus

instance ToHeader RealtimeMetricsSubscriptionStatus

instance FromXML RealtimeMetricsSubscriptionStatus where
  parseXML = parseXMLText "RealtimeMetricsSubscriptionStatus"

instance ToXML RealtimeMetricsSubscriptionStatus where
  toXML = toXMLText
