{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SourceType where

import Network.AWS.Prelude

data SourceType
  = Cluster
  | ParameterGroup
  | SubnetGroup
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

instance FromText SourceType where
  parser =
    takeLowerText >>= \case
      "cluster" -> pure Cluster
      "parameter_group" -> pure ParameterGroup
      "subnet_group" -> pure SubnetGroup
      e ->
        fromTextError $
          "Failure parsing SourceType from value: '" <> e
            <> "'. Accepted values: cluster, parameter_group, subnet_group"

instance ToText SourceType where
  toText = \case
    Cluster -> "CLUSTER"
    ParameterGroup -> "PARAMETER_GROUP"
    SubnetGroup -> "SUBNET_GROUP"

instance Hashable SourceType

instance NFData SourceType

instance ToByteString SourceType

instance ToQuery SourceType

instance ToHeader SourceType

instance ToJSON SourceType where
  toJSON = toJSONText

instance FromJSON SourceType where
  parseJSON = parseJSONText "SourceType"
