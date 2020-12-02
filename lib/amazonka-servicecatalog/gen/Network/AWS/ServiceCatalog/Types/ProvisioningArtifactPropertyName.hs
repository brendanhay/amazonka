{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName where

import Network.AWS.Prelude

data ProvisioningArtifactPropertyName = Id
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

instance FromText ProvisioningArtifactPropertyName where
  parser =
    takeLowerText >>= \case
      "id" -> pure Id
      e ->
        fromTextError $
          "Failure parsing ProvisioningArtifactPropertyName from value: '" <> e
            <> "'. Accepted values: id"

instance ToText ProvisioningArtifactPropertyName where
  toText = \case
    Id -> "Id"

instance Hashable ProvisioningArtifactPropertyName

instance NFData ProvisioningArtifactPropertyName

instance ToByteString ProvisioningArtifactPropertyName

instance ToQuery ProvisioningArtifactPropertyName

instance ToHeader ProvisioningArtifactPropertyName

instance ToJSON ProvisioningArtifactPropertyName where
  toJSON = toJSONText
