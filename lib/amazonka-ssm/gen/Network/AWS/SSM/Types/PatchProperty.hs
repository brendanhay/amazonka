{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchProperty where

import Network.AWS.Prelude

data PatchProperty
  = PPClassification
  | PPMsrcSeverity
  | PPPriority
  | PPProduct
  | PPProductFamily
  | PPSeverity
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

instance FromText PatchProperty where
  parser =
    takeLowerText >>= \case
      "classification" -> pure PPClassification
      "msrc_severity" -> pure PPMsrcSeverity
      "priority" -> pure PPPriority
      "product" -> pure PPProduct
      "product_family" -> pure PPProductFamily
      "severity" -> pure PPSeverity
      e ->
        fromTextError $
          "Failure parsing PatchProperty from value: '" <> e
            <> "'. Accepted values: classification, msrc_severity, priority, product, product_family, severity"

instance ToText PatchProperty where
  toText = \case
    PPClassification -> "CLASSIFICATION"
    PPMsrcSeverity -> "MSRC_SEVERITY"
    PPPriority -> "PRIORITY"
    PPProduct -> "PRODUCT"
    PPProductFamily -> "PRODUCT_FAMILY"
    PPSeverity -> "SEVERITY"

instance Hashable PatchProperty

instance NFData PatchProperty

instance ToByteString PatchProperty

instance ToQuery PatchProperty

instance ToHeader PatchProperty

instance ToJSON PatchProperty where
  toJSON = toJSONText
