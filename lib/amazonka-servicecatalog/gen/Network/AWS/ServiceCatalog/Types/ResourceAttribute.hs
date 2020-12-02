{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceAttribute where

import Network.AWS.Prelude

data ResourceAttribute
  = Creationpolicy
  | Deletionpolicy
  | Metadata
  | Properties
  | Tags
  | Updatepolicy
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

instance FromText ResourceAttribute where
  parser =
    takeLowerText >>= \case
      "creationpolicy" -> pure Creationpolicy
      "deletionpolicy" -> pure Deletionpolicy
      "metadata" -> pure Metadata
      "properties" -> pure Properties
      "tags" -> pure Tags
      "updatepolicy" -> pure Updatepolicy
      e ->
        fromTextError $
          "Failure parsing ResourceAttribute from value: '" <> e
            <> "'. Accepted values: creationpolicy, deletionpolicy, metadata, properties, tags, updatepolicy"

instance ToText ResourceAttribute where
  toText = \case
    Creationpolicy -> "CREATIONPOLICY"
    Deletionpolicy -> "DELETIONPOLICY"
    Metadata -> "METADATA"
    Properties -> "PROPERTIES"
    Tags -> "TAGS"
    Updatepolicy -> "UPDATEPOLICY"

instance Hashable ResourceAttribute

instance NFData ResourceAttribute

instance ToByteString ResourceAttribute

instance ToQuery ResourceAttribute

instance ToHeader ResourceAttribute

instance FromJSON ResourceAttribute where
  parseJSON = parseJSONText "ResourceAttribute"
