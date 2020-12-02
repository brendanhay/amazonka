{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance where

import Network.AWS.Prelude

data ProvisioningArtifactGuidance
  = Default
  | Deprecated
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

instance FromText ProvisioningArtifactGuidance where
  parser =
    takeLowerText >>= \case
      "default" -> pure Default
      "deprecated" -> pure Deprecated
      e ->
        fromTextError $
          "Failure parsing ProvisioningArtifactGuidance from value: '" <> e
            <> "'. Accepted values: default, deprecated"

instance ToText ProvisioningArtifactGuidance where
  toText = \case
    Default -> "DEFAULT"
    Deprecated -> "DEPRECATED"

instance Hashable ProvisioningArtifactGuidance

instance NFData ProvisioningArtifactGuidance

instance ToByteString ProvisioningArtifactGuidance

instance ToQuery ProvisioningArtifactGuidance

instance ToHeader ProvisioningArtifactGuidance

instance ToJSON ProvisioningArtifactGuidance where
  toJSON = toJSONText

instance FromJSON ProvisioningArtifactGuidance where
  parseJSON = parseJSONText "ProvisioningArtifactGuidance"
