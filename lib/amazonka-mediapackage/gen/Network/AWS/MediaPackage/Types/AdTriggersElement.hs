{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SpliceInsert,
        Break,
        ProviderAdvertisement,
        DistributorAdvertisement,
        ProviderPlacementOpportunity,
        DistributorPlacementOpportunity,
        ProviderOverlayPlacementOpportunity,
        DistributorOverlayPlacementOpportunity
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

pattern SpliceInsert :: AdTriggersElement
pattern SpliceInsert = AdTriggersElement' "SPLICE_INSERT"

pattern Break :: AdTriggersElement
pattern Break = AdTriggersElement' "BREAK"

pattern ProviderAdvertisement :: AdTriggersElement
pattern ProviderAdvertisement = AdTriggersElement' "PROVIDER_ADVERTISEMENT"

pattern DistributorAdvertisement :: AdTriggersElement
pattern DistributorAdvertisement = AdTriggersElement' "DISTRIBUTOR_ADVERTISEMENT"

pattern ProviderPlacementOpportunity :: AdTriggersElement
pattern ProviderPlacementOpportunity = AdTriggersElement' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern DistributorPlacementOpportunity :: AdTriggersElement
pattern DistributorPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern ProviderOverlayPlacementOpportunity :: AdTriggersElement
pattern ProviderOverlayPlacementOpportunity = AdTriggersElement' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern DistributorOverlayPlacementOpportunity :: AdTriggersElement
pattern DistributorOverlayPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

{-# COMPLETE
  SpliceInsert,
  Break,
  ProviderAdvertisement,
  DistributorAdvertisement,
  ProviderPlacementOpportunity,
  DistributorPlacementOpportunity,
  ProviderOverlayPlacementOpportunity,
  DistributorOverlayPlacementOpportunity,
  AdTriggersElement'
  #-}
