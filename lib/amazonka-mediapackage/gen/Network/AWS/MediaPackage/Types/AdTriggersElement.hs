{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdTriggersElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdTriggersElement where

import Network.AWS.Prelude

data AdTriggersElement
  = Break
  | DistributorAdvertisement
  | DistributorOverlayPlacementOpportunity
  | DistributorPlacementOpportunity
  | ProviderAdvertisement
  | ProviderOverlayPlacementOpportunity
  | ProviderPlacementOpportunity
  | SpliceInsert
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

instance FromText AdTriggersElement where
  parser =
    takeLowerText >>= \case
      "break" -> pure Break
      "distributor_advertisement" -> pure DistributorAdvertisement
      "distributor_overlay_placement_opportunity" -> pure DistributorOverlayPlacementOpportunity
      "distributor_placement_opportunity" -> pure DistributorPlacementOpportunity
      "provider_advertisement" -> pure ProviderAdvertisement
      "provider_overlay_placement_opportunity" -> pure ProviderOverlayPlacementOpportunity
      "provider_placement_opportunity" -> pure ProviderPlacementOpportunity
      "splice_insert" -> pure SpliceInsert
      e ->
        fromTextError $
          "Failure parsing AdTriggersElement from value: '" <> e
            <> "'. Accepted values: break, distributor_advertisement, distributor_overlay_placement_opportunity, distributor_placement_opportunity, provider_advertisement, provider_overlay_placement_opportunity, provider_placement_opportunity, splice_insert"

instance ToText AdTriggersElement where
  toText = \case
    Break -> "BREAK"
    DistributorAdvertisement -> "DISTRIBUTOR_ADVERTISEMENT"
    DistributorOverlayPlacementOpportunity -> "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"
    DistributorPlacementOpportunity -> "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"
    ProviderAdvertisement -> "PROVIDER_ADVERTISEMENT"
    ProviderOverlayPlacementOpportunity -> "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"
    ProviderPlacementOpportunity -> "PROVIDER_PLACEMENT_OPPORTUNITY"
    SpliceInsert -> "SPLICE_INSERT"

instance Hashable AdTriggersElement

instance NFData AdTriggersElement

instance ToByteString AdTriggersElement

instance ToQuery AdTriggersElement

instance ToHeader AdTriggersElement

instance ToJSON AdTriggersElement where
  toJSON = toJSONText

instance FromJSON AdTriggersElement where
  parseJSON = parseJSONText "AdTriggersElement"
