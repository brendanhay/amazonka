{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.PaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.PaymentOption
  ( PaymentOption
      ( PaymentOption',
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

newtype PaymentOption = PaymentOption' Lude.Text
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

pattern AllUpfront :: PaymentOption
pattern AllUpfront = PaymentOption' "ALL_UPFRONT"

pattern HeavyUtilization :: PaymentOption
pattern HeavyUtilization = PaymentOption' "HEAVY_UTILIZATION"

pattern LightUtilization :: PaymentOption
pattern LightUtilization = PaymentOption' "LIGHT_UTILIZATION"

pattern MediumUtilization :: PaymentOption
pattern MediumUtilization = PaymentOption' "MEDIUM_UTILIZATION"

pattern NoUpfront :: PaymentOption
pattern NoUpfront = PaymentOption' "NO_UPFRONT"

pattern PartialUpfront :: PaymentOption
pattern PartialUpfront = PaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  AllUpfront,
  HeavyUtilization,
  LightUtilization,
  MediumUtilization,
  NoUpfront,
  PartialUpfront,
  PaymentOption'
  #-}
