-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdTriggersElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdTriggersElement
  ( AdTriggersElement
      ( AdTriggersElement',
        Break,
        DistributorAdvertisement,
        DistributorOverlayPlacementOpportunity,
        DistributorPlacementOpportunity,
        ProviderAdvertisement,
        ProviderOverlayPlacementOpportunity,
        ProviderPlacementOpportunity,
        SpliceInsert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AdTriggersElement = AdTriggersElement' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Break :: AdTriggersElement
pattern Break = AdTriggersElement' "BREAK"

pattern DistributorAdvertisement :: AdTriggersElement
pattern DistributorAdvertisement = AdTriggersElement' "DISTRIBUTOR_ADVERTISEMENT"

pattern DistributorOverlayPlacementOpportunity :: AdTriggersElement
pattern DistributorOverlayPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern DistributorPlacementOpportunity :: AdTriggersElement
pattern DistributorPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern ProviderAdvertisement :: AdTriggersElement
pattern ProviderAdvertisement = AdTriggersElement' "PROVIDER_ADVERTISEMENT"

pattern ProviderOverlayPlacementOpportunity :: AdTriggersElement
pattern ProviderOverlayPlacementOpportunity = AdTriggersElement' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern ProviderPlacementOpportunity :: AdTriggersElement
pattern ProviderPlacementOpportunity = AdTriggersElement' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern SpliceInsert :: AdTriggersElement
pattern SpliceInsert = AdTriggersElement' "SPLICE_INSERT"

{-# COMPLETE
  Break,
  DistributorAdvertisement,
  DistributorOverlayPlacementOpportunity,
  DistributorPlacementOpportunity,
  ProviderAdvertisement,
  ProviderOverlayPlacementOpportunity,
  ProviderPlacementOpportunity,
  SpliceInsert,
  AdTriggersElement'
  #-}
