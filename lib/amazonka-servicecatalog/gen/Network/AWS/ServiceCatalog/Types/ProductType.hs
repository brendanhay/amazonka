{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductType where

import Network.AWS.Prelude

data ProductType
  = CloudFormationTemplate
  | Marketplace
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

instance FromText ProductType where
  parser =
    takeLowerText >>= \case
      "cloud_formation_template" -> pure CloudFormationTemplate
      "marketplace" -> pure Marketplace
      e ->
        fromTextError $
          "Failure parsing ProductType from value: '" <> e
            <> "'. Accepted values: cloud_formation_template, marketplace"

instance ToText ProductType where
  toText = \case
    CloudFormationTemplate -> "CLOUD_FORMATION_TEMPLATE"
    Marketplace -> "MARKETPLACE"

instance Hashable ProductType

instance NFData ProductType

instance ToByteString ProductType

instance ToQuery ProductType

instance ToHeader ProductType

instance ToJSON ProductType where
  toJSON = toJSONText

instance FromJSON ProductType where
  parseJSON = parseJSONText "ProductType"
