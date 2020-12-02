{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType where

import Network.AWS.Prelude

data ProvisionedProductPlanType = Cloudformation
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

instance FromText ProvisionedProductPlanType where
  parser =
    takeLowerText >>= \case
      "cloudformation" -> pure Cloudformation
      e ->
        fromTextError $
          "Failure parsing ProvisionedProductPlanType from value: '" <> e
            <> "'. Accepted values: cloudformation"

instance ToText ProvisionedProductPlanType where
  toText = \case
    Cloudformation -> "CLOUDFORMATION"

instance Hashable ProvisionedProductPlanType

instance NFData ProvisionedProductPlanType

instance ToByteString ProvisionedProductPlanType

instance ToQuery ProvisionedProductPlanType

instance ToHeader ProvisionedProductPlanType

instance ToJSON ProvisionedProductPlanType where
  toJSON = toJSONText

instance FromJSON ProvisionedProductPlanType where
  parseJSON = parseJSONText "ProvisionedProductPlanType"
