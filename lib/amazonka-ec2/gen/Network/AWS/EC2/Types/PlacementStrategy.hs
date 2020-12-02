{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementStrategy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PlacementStrategy
  = Cluster
  | Partition
  | Spread
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

instance FromText PlacementStrategy where
  parser =
    takeLowerText >>= \case
      "cluster" -> pure Cluster
      "partition" -> pure Partition
      "spread" -> pure Spread
      e ->
        fromTextError $
          "Failure parsing PlacementStrategy from value: '" <> e
            <> "'. Accepted values: cluster, partition, spread"

instance ToText PlacementStrategy where
  toText = \case
    Cluster -> "cluster"
    Partition -> "partition"
    Spread -> "spread"

instance Hashable PlacementStrategy

instance NFData PlacementStrategy

instance ToByteString PlacementStrategy

instance ToQuery PlacementStrategy

instance ToHeader PlacementStrategy

instance FromXML PlacementStrategy where
  parseXML = parseXMLText "PlacementStrategy"
