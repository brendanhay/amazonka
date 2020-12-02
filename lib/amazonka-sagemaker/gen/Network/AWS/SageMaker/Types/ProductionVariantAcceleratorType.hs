{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType where

import Network.AWS.Prelude

data ProductionVariantAcceleratorType
  = PVATMl_EIA1_Large
  | PVATMl_EIA1_Medium
  | PVATMl_EIA1_XLarge
  | PVATMl_EIA2_Large
  | PVATMl_EIA2_Medium
  | PVATMl_EIA2_XLarge
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

instance FromText ProductionVariantAcceleratorType where
  parser =
    takeLowerText >>= \case
      "ml.eia1.large" -> pure PVATMl_EIA1_Large
      "ml.eia1.medium" -> pure PVATMl_EIA1_Medium
      "ml.eia1.xlarge" -> pure PVATMl_EIA1_XLarge
      "ml.eia2.large" -> pure PVATMl_EIA2_Large
      "ml.eia2.medium" -> pure PVATMl_EIA2_Medium
      "ml.eia2.xlarge" -> pure PVATMl_EIA2_XLarge
      e ->
        fromTextError $
          "Failure parsing ProductionVariantAcceleratorType from value: '" <> e
            <> "'. Accepted values: ml.eia1.large, ml.eia1.medium, ml.eia1.xlarge, ml.eia2.large, ml.eia2.medium, ml.eia2.xlarge"

instance ToText ProductionVariantAcceleratorType where
  toText = \case
    PVATMl_EIA1_Large -> "ml.eia1.large"
    PVATMl_EIA1_Medium -> "ml.eia1.medium"
    PVATMl_EIA1_XLarge -> "ml.eia1.xlarge"
    PVATMl_EIA2_Large -> "ml.eia2.large"
    PVATMl_EIA2_Medium -> "ml.eia2.medium"
    PVATMl_EIA2_XLarge -> "ml.eia2.xlarge"

instance Hashable ProductionVariantAcceleratorType

instance NFData ProductionVariantAcceleratorType

instance ToByteString ProductionVariantAcceleratorType

instance ToQuery ProductionVariantAcceleratorType

instance ToHeader ProductionVariantAcceleratorType

instance ToJSON ProductionVariantAcceleratorType where
  toJSON = toJSONText

instance FromJSON ProductionVariantAcceleratorType where
  parseJSON = parseJSONText "ProductionVariantAcceleratorType"
