{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowResourceType where

import Network.AWS.Prelude

data MaintenanceWindowResourceType
  = Instance
  | ResourceGroup
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

instance FromText MaintenanceWindowResourceType where
  parser =
    takeLowerText >>= \case
      "instance" -> pure Instance
      "resource_group" -> pure ResourceGroup
      e ->
        fromTextError $
          "Failure parsing MaintenanceWindowResourceType from value: '" <> e
            <> "'. Accepted values: instance, resource_group"

instance ToText MaintenanceWindowResourceType where
  toText = \case
    Instance -> "INSTANCE"
    ResourceGroup -> "RESOURCE_GROUP"

instance Hashable MaintenanceWindowResourceType

instance NFData MaintenanceWindowResourceType

instance ToByteString MaintenanceWindowResourceType

instance ToQuery MaintenanceWindowResourceType

instance ToHeader MaintenanceWindowResourceType

instance ToJSON MaintenanceWindowResourceType where
  toJSON = toJSONText

instance FromJSON MaintenanceWindowResourceType where
  parseJSON = parseJSONText "MaintenanceWindowResourceType"
