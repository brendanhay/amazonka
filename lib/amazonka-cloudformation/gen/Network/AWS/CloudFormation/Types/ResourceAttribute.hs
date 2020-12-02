{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceAttribute where

import Network.AWS.Prelude

data ResourceAttribute
  = CreationPolicy
  | DeletionPolicy
  | Metadata
  | Properties
  | Tags
  | UpdatePolicy
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
      "creationpolicy" -> pure CreationPolicy
      "deletionpolicy" -> pure DeletionPolicy
      "metadata" -> pure Metadata
      "properties" -> pure Properties
      "tags" -> pure Tags
      "updatepolicy" -> pure UpdatePolicy
      e ->
        fromTextError $
          "Failure parsing ResourceAttribute from value: '" <> e
            <> "'. Accepted values: creationpolicy, deletionpolicy, metadata, properties, tags, updatepolicy"

instance ToText ResourceAttribute where
  toText = \case
    CreationPolicy -> "CreationPolicy"
    DeletionPolicy -> "DeletionPolicy"
    Metadata -> "Metadata"
    Properties -> "Properties"
    Tags -> "Tags"
    UpdatePolicy -> "UpdatePolicy"

instance Hashable ResourceAttribute

instance NFData ResourceAttribute

instance ToByteString ResourceAttribute

instance ToQuery ResourceAttribute

instance ToHeader ResourceAttribute

instance FromXML ResourceAttribute where
  parseXML = parseXMLText "ResourceAttribute"
