{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OfferingTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OfferingTypeValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data OfferingTypeValues
  = AllUpfront
  | HeavyUtilization
  | LightUtilization
  | MediumUtilization
  | NoUpfront
  | PartialUpfront
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

instance FromText OfferingTypeValues where
  parser =
    takeLowerText >>= \case
      "all upfront" -> pure AllUpfront
      "heavy utilization" -> pure HeavyUtilization
      "light utilization" -> pure LightUtilization
      "medium utilization" -> pure MediumUtilization
      "no upfront" -> pure NoUpfront
      "partial upfront" -> pure PartialUpfront
      e ->
        fromTextError $
          "Failure parsing OfferingTypeValues from value: '" <> e
            <> "'. Accepted values: all upfront, heavy utilization, light utilization, medium utilization, no upfront, partial upfront"

instance ToText OfferingTypeValues where
  toText = \case
    AllUpfront -> "All Upfront"
    HeavyUtilization -> "Heavy Utilization"
    LightUtilization -> "Light Utilization"
    MediumUtilization -> "Medium Utilization"
    NoUpfront -> "No Upfront"
    PartialUpfront -> "Partial Upfront"

instance Hashable OfferingTypeValues

instance NFData OfferingTypeValues

instance ToByteString OfferingTypeValues

instance ToQuery OfferingTypeValues

instance ToHeader OfferingTypeValues

instance FromXML OfferingTypeValues where
  parseXML = parseXMLText "OfferingTypeValues"
