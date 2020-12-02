{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType where

import Network.AWS.Prelude

data ProvisioningArtifactType
  = PATCloudFormationTemplate
  | PATMarketplaceAMI
  | PATMarketplaceCar
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

instance FromText ProvisioningArtifactType where
  parser =
    takeLowerText >>= \case
      "cloud_formation_template" -> pure PATCloudFormationTemplate
      "marketplace_ami" -> pure PATMarketplaceAMI
      "marketplace_car" -> pure PATMarketplaceCar
      e ->
        fromTextError $
          "Failure parsing ProvisioningArtifactType from value: '" <> e
            <> "'. Accepted values: cloud_formation_template, marketplace_ami, marketplace_car"

instance ToText ProvisioningArtifactType where
  toText = \case
    PATCloudFormationTemplate -> "CLOUD_FORMATION_TEMPLATE"
    PATMarketplaceAMI -> "MARKETPLACE_AMI"
    PATMarketplaceCar -> "MARKETPLACE_CAR"

instance Hashable ProvisioningArtifactType

instance NFData ProvisioningArtifactType

instance ToByteString ProvisioningArtifactType

instance ToQuery ProvisioningArtifactType

instance ToHeader ProvisioningArtifactType

instance ToJSON ProvisioningArtifactType where
  toJSON = toJSONText

instance FromJSON ProvisioningArtifactType where
  parseJSON = parseJSONText "ProvisioningArtifactType"
