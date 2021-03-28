{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdTriggersElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.AdTriggersElement
  ( AdTriggersElement
    ( AdTriggersElement'
    , AdTriggersElementSpliceInsert
    , AdTriggersElementBreak
    , AdTriggersElementProviderAdvertisement
    , AdTriggersElementDistributorAdvertisement
    , AdTriggersElementProviderPlacementOpportunity
    , AdTriggersElementDistributorPlacementOpportunity
    , AdTriggersElementProviderOverlayPlacementOpportunity
    , AdTriggersElementDistributorOverlayPlacementOpportunity
    , fromAdTriggersElement
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AdTriggersElement = AdTriggersElement'{fromAdTriggersElement
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern AdTriggersElementSpliceInsert :: AdTriggersElement
pattern AdTriggersElementSpliceInsert = AdTriggersElement' "SPLICE_INSERT"

pattern AdTriggersElementBreak :: AdTriggersElement
pattern AdTriggersElementBreak = AdTriggersElement' "BREAK"

pattern AdTriggersElementProviderAdvertisement :: AdTriggersElement
pattern AdTriggersElementProviderAdvertisement = AdTriggersElement' "PROVIDER_ADVERTISEMENT"

pattern AdTriggersElementDistributorAdvertisement :: AdTriggersElement
pattern AdTriggersElementDistributorAdvertisement = AdTriggersElement' "DISTRIBUTOR_ADVERTISEMENT"

pattern AdTriggersElementProviderPlacementOpportunity :: AdTriggersElement
pattern AdTriggersElementProviderPlacementOpportunity = AdTriggersElement' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElementDistributorPlacementOpportunity :: AdTriggersElement
pattern AdTriggersElementDistributorPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElementProviderOverlayPlacementOpportunity :: AdTriggersElement
pattern AdTriggersElementProviderOverlayPlacementOpportunity = AdTriggersElement' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElementDistributorOverlayPlacementOpportunity :: AdTriggersElement
pattern AdTriggersElementDistributorOverlayPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

{-# COMPLETE 
  AdTriggersElementSpliceInsert,

  AdTriggersElementBreak,

  AdTriggersElementProviderAdvertisement,

  AdTriggersElementDistributorAdvertisement,

  AdTriggersElementProviderPlacementOpportunity,

  AdTriggersElementDistributorPlacementOpportunity,

  AdTriggersElementProviderOverlayPlacementOpportunity,

  AdTriggersElementDistributorOverlayPlacementOpportunity,
  AdTriggersElement'
  #-}
