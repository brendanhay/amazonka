-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OfferingTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OfferingTypeValues
  ( OfferingTypeValues
      ( OfferingTypeValues',
        AllUpfront,
        HeavyUtilization,
        LightUtilization,
        MediumUtilization,
        NoUpfront,
        PartialUpfront
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OfferingTypeValues = OfferingTypeValues' Lude.Text
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

pattern AllUpfront :: OfferingTypeValues
pattern AllUpfront = OfferingTypeValues' "All Upfront"

pattern HeavyUtilization :: OfferingTypeValues
pattern HeavyUtilization = OfferingTypeValues' "Heavy Utilization"

pattern LightUtilization :: OfferingTypeValues
pattern LightUtilization = OfferingTypeValues' "Light Utilization"

pattern MediumUtilization :: OfferingTypeValues
pattern MediumUtilization = OfferingTypeValues' "Medium Utilization"

pattern NoUpfront :: OfferingTypeValues
pattern NoUpfront = OfferingTypeValues' "No Upfront"

pattern PartialUpfront :: OfferingTypeValues
pattern PartialUpfront = OfferingTypeValues' "Partial Upfront"

{-# COMPLETE
  AllUpfront,
  HeavyUtilization,
  LightUtilization,
  MediumUtilization,
  NoUpfront,
  PartialUpfront,
  OfferingTypeValues'
  #-}
