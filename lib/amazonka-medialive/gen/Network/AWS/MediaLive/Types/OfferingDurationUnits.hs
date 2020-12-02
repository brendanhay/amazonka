{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OfferingDurationUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OfferingDurationUnits where

import Network.AWS.Prelude

-- | Units for duration, e.g. 'MONTHS'
data OfferingDurationUnits = Months
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

instance FromText OfferingDurationUnits where
  parser =
    takeLowerText >>= \case
      "months" -> pure Months
      e ->
        fromTextError $
          "Failure parsing OfferingDurationUnits from value: '" <> e
            <> "'. Accepted values: months"

instance ToText OfferingDurationUnits where
  toText = \case
    Months -> "MONTHS"

instance Hashable OfferingDurationUnits

instance NFData OfferingDurationUnits

instance ToByteString OfferingDurationUnits

instance ToQuery OfferingDurationUnits

instance ToHeader OfferingDurationUnits

instance FromJSON OfferingDurationUnits where
  parseJSON = parseJSONText "OfferingDurationUnits"
