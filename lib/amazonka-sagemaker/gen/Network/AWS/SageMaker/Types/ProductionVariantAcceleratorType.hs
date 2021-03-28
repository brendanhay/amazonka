{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
  ( ProductionVariantAcceleratorType
    ( ProductionVariantAcceleratorType'
    , ProductionVariantAcceleratorTypeMl_EIA1_Medium
    , ProductionVariantAcceleratorTypeMl_EIA1_Large
    , ProductionVariantAcceleratorTypeMl_EIA1_Xlarge
    , ProductionVariantAcceleratorTypeMl_EIA2_Medium
    , ProductionVariantAcceleratorTypeMl_EIA2_Large
    , ProductionVariantAcceleratorTypeMl_EIA2_Xlarge
    , fromProductionVariantAcceleratorType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProductionVariantAcceleratorType = ProductionVariantAcceleratorType'{fromProductionVariantAcceleratorType
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern ProductionVariantAcceleratorTypeMl_EIA1_Medium :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA1_Medium = ProductionVariantAcceleratorType' "ml.eia1.medium"

pattern ProductionVariantAcceleratorTypeMl_EIA1_Large :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA1_Large = ProductionVariantAcceleratorType' "ml.eia1.large"

pattern ProductionVariantAcceleratorTypeMl_EIA1_Xlarge :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA1_Xlarge = ProductionVariantAcceleratorType' "ml.eia1.xlarge"

pattern ProductionVariantAcceleratorTypeMl_EIA2_Medium :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA2_Medium = ProductionVariantAcceleratorType' "ml.eia2.medium"

pattern ProductionVariantAcceleratorTypeMl_EIA2_Large :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA2_Large = ProductionVariantAcceleratorType' "ml.eia2.large"

pattern ProductionVariantAcceleratorTypeMl_EIA2_Xlarge :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorTypeMl_EIA2_Xlarge = ProductionVariantAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE 
  ProductionVariantAcceleratorTypeMl_EIA1_Medium,

  ProductionVariantAcceleratorTypeMl_EIA1_Large,

  ProductionVariantAcceleratorTypeMl_EIA1_Xlarge,

  ProductionVariantAcceleratorTypeMl_EIA2_Medium,

  ProductionVariantAcceleratorTypeMl_EIA2_Large,

  ProductionVariantAcceleratorTypeMl_EIA2_Xlarge,
  ProductionVariantAcceleratorType'
  #-}
