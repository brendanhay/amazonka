{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanStatus where

import Network.AWS.Prelude

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
data ReservationPlanStatus
  = Active
  | Expired
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

instance FromText ReservationPlanStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "expired" -> pure Expired
      e ->
        fromTextError $
          "Failure parsing ReservationPlanStatus from value: '" <> e
            <> "'. Accepted values: active, expired"

instance ToText ReservationPlanStatus where
  toText = \case
    Active -> "ACTIVE"
    Expired -> "EXPIRED"

instance Hashable ReservationPlanStatus

instance NFData ReservationPlanStatus

instance ToByteString ReservationPlanStatus

instance ToQuery ReservationPlanStatus

instance ToHeader ReservationPlanStatus

instance FromJSON ReservationPlanStatus where
  parseJSON = parseJSONText "ReservationPlanStatus"
