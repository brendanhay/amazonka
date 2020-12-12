{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
  ( ProductionVariantAcceleratorType
      ( ProductionVariantAcceleratorType',
        PVATMl_EIA1_Large,
        PVATMl_EIA1_Medium,
        PVATMl_EIA1_XLarge,
        PVATMl_EIA2_Large,
        PVATMl_EIA2_Medium,
        PVATMl_EIA2_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProductionVariantAcceleratorType = ProductionVariantAcceleratorType' Lude.Text
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

pattern PVATMl_EIA1_Large :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_Large = ProductionVariantAcceleratorType' "ml.eia1.large"

pattern PVATMl_EIA1_Medium :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_Medium = ProductionVariantAcceleratorType' "ml.eia1.medium"

pattern PVATMl_EIA1_XLarge :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_XLarge = ProductionVariantAcceleratorType' "ml.eia1.xlarge"

pattern PVATMl_EIA2_Large :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_Large = ProductionVariantAcceleratorType' "ml.eia2.large"

pattern PVATMl_EIA2_Medium :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_Medium = ProductionVariantAcceleratorType' "ml.eia2.medium"

pattern PVATMl_EIA2_XLarge :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_XLarge = ProductionVariantAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  PVATMl_EIA1_Large,
  PVATMl_EIA1_Medium,
  PVATMl_EIA1_XLarge,
  PVATMl_EIA2_Large,
  PVATMl_EIA2_Medium,
  PVATMl_EIA2_XLarge,
  ProductionVariantAcceleratorType'
  #-}
