{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceCollectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceCollectionType where

import Network.AWS.Prelude

data InstanceCollectionType
  = InstanceFleet
  | InstanceGroup
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

instance FromText InstanceCollectionType where
  parser =
    takeLowerText >>= \case
      "instance_fleet" -> pure InstanceFleet
      "instance_group" -> pure InstanceGroup
      e ->
        fromTextError $
          "Failure parsing InstanceCollectionType from value: '" <> e
            <> "'. Accepted values: instance_fleet, instance_group"

instance ToText InstanceCollectionType where
  toText = \case
    InstanceFleet -> "INSTANCE_FLEET"
    InstanceGroup -> "INSTANCE_GROUP"

instance Hashable InstanceCollectionType

instance NFData InstanceCollectionType

instance ToByteString InstanceCollectionType

instance ToQuery InstanceCollectionType

instance ToHeader InstanceCollectionType

instance FromJSON InstanceCollectionType where
  parseJSON = parseJSONText "InstanceCollectionType"
